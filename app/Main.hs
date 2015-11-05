{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Lens               ((&), (.~), (^.))
import           Data.Aeson                 (FromJSON, decode,encode)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Maybe                 (catMaybes, mapMaybe)
import           Data.Time.Clock            (getCurrentTime)
import           Hasbitica.Api
import           Hasbitica.LensStuff
import           Control.Monad.Trans (liftIO)
import           Hasbitica.Settings         (getApiFromSettings)
import           Servant.Common.Req         (responseBody)
import           System.Console.CmdArgs
import           System.Directory           (getHomeDirectory)
import           System.FilePath.Posix      ((</>))

data HasbiticaCli = New { newTodoText :: String }
               | List
               | ListAll
               | Done { guid :: String }
               | Delete { guid :: String }
                 deriving (Show, Data, Typeable)

examples = [ New {newTodoText = "Buy milk" &= help "The text of the new todo" }
           , List &= help "List all not-done todos"
           , ListAll &= help "List all todos"
           , Done "<GUID>" &= help "Mark todo with guid X as done"
           , Delete "<GUID>" &= help "Delete todo X"]
             &= program "hasbitica-cli"

myRun :: HMonad a -> HabiticaApiKey -> IO a
myRun x k = either error id <$> runHMonad x k
  
despatch :: HasbiticaCli -> HMonad String
despatch (New x) = postTask (todo x) >> return "OK"
despatch List = unlines <$>  map (\(a,b) -> a++" "++b) <$> getAllNotDoneTodos 
despatch ListAll = show <$> (getAll :: HMonad [Todo])
despatch (Done x) = doneTask x
despatch (Delete x) = show <$> deleteTask x 

main :: IO ()
main = do
       x <- cmdArgs (modes examples)
       (Just key) <- getApiFromSettings
       myRun (despatch x) key >>= putStrLn

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
