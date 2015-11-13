{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens               ((&), (.~), (^.),view)
import           Data.Aeson                 (FromJSON, decode,encode)
import qualified Data.Aeson.Lens as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import           Data.Maybe                 (catMaybes, mapMaybe)
import           Data.Time.Clock            (getCurrentTime)
import           Hasbitica.Api
import           Hasbitica.LensStuff
import Control.Monad(forM)
import           Control.Monad.Trans.Reader (local)
import           Control.Monad.Trans (liftIO)
import           Hasbitica.Settings         
import           Servant.Common.Req         (responseBody)
import           System.Console.CmdArgs
import           System.Directory           (getHomeDirectory)
import           System.FilePath.Posix      ((</>))

data HasbiticaCli = New { newTodoText :: String }
               | User
               | UserNames
               | List
               | ListAll
               | ListMovable
               | Done { guid :: String }
               | Delete { guid :: String }
                 deriving (Show, Data, Typeable)

examples = [ New {newTodoText = "Buy milk" &= help "The text of the new todo" }
           , User &= help "Show user info"
           , UserNames &= help "Show IDs and usernames of all users"
           , List &= help "List all not-done todos"
           , ListAll &= help "List all todos"
           , ListMovable &= help "List all movable tasks"
           , Done "<GUID>" &= help "Mark todo with guid X as done"
           , Delete "<GUID>" &= help "Delete todo X"]
             &= program "hasbitica-cli"

myRun :: HMonad a -> HabiticaApiKey -> IO a
myRun x k = either error id <$> runHMonad x k
  
despatch :: HasbiticaCli -> HMonad String
despatch (New x) = postTask (todo x) >> return "OK"
despatch User = B.unpack . encode <$> getUser 
despatch UserNames = show <$> getUserNames
despatch List = unlines <$>  map (\(a,b) -> a++" "++b) <$> getAllNotDoneTodos 
despatch ListAll = show <$> (getAll :: HMonad [Todo])
despatch ListMovable = show <$> getMovableTasks 
despatch (Done x) = doneTask x
despatch (Delete x) = show <$> deleteTask x 

main :: IO ()
main = do
       x <- cmdArgs (modes examples)
       (Just key) <- getApiFromSettings
       myRun (despatch x) key >>= putStrLn

getUserNames :: HMonad [T.Text]
getUserNames = do
  allKeys <- liftIO getAllApisFromSettings
  forM allKeys $ \k -> local (const k) $ 
    view (L.key "profile" . L.key "name" . L._String) <$> getUser
    

getTarget :: String -> Maybe String
getTarget ('@':rest) = Just . takeWhile (/= ' ') $ rest
getTarget _ = Nothing

-- Would be nice if this was better.
mapMaybeKeepOrig :: (a -> Maybe b) -> [a] -> [(a,b)]
mapMaybeKeepOrig f [] = []
mapMaybeKeepOrig f (x:xs) = case f x of
  Nothing -> mapMaybeKeepOrig f xs
  Just y -> (x,y) : mapMaybeKeepOrig f xs


getMovableTasks :: HMonad [(Task,String)]
getMovableTasks = do
  allKeys <- liftIO getAllApisFromSettings
  fmap concat <$> forM allKeys $ \k -> local (const k) $ 
    mapMaybeKeepOrig (\i -> getTarget (toBase i ^. text)) <$> getTasks
    
doneTask :: String -> HMonad String
doneTask t = do
  task <- getTask t
  now <- liftIO getCurrentTime
  case task of 
    TaskTodo x -> do
      let newTask = x & todoCompleted .~ True
                      & todoDateCompleted .~ Just now
      show <$> updateTask t (TaskTodo newTask)
    x -> return ("Not a todo: " ++ show x)

getAll :: HasBaseTask a => HMonad [a]
getAll = mapMaybe fromTask <$> getTasks 

getAllNotDoneTodos :: HMonad [(String,String)]
getAllNotDoneTodos = map (\i -> (i^.todoBase . taskId , i^.todoBase . text)) <$> getTodos
