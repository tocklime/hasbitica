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

main :: IO ()
main = do
       x <- cmdArgs (modes examples)
       (Just key) <- getApiFromSettings
       case x of
         New x -> addTask key x >>= putStrLn
         List -> getAllNotDoneTodos >>= mapM_ (putStrLn . (\(a,b) -> a++" "++b))
         ListAll -> getAllTodos key >>= mapM_ print 
         Done x -> either id id <$> runHMonad (doneTask x) key >>= putStrLn
         Delete x -> runHMonad (deleteTask x) key >>= print

addTask :: HabiticaApiKey -> String -> IO String
addTask k t = 
  either id (const "OK") <$> runHMonad (postTask (todo t)) k

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

getAllTodos :: HabiticaApiKey -> IO [Todo]
getAllTodos k = do
  ans <- runHMonad getTasks k
  case ans of
    Left err -> print err >> return []
    Right tasks -> return  (mapMaybe fromTask tasks)

getAllNotDoneTodos :: IO [(String,String)]
getAllNotDoneTodos = do
  (Just k) <- getApiFromSettings
  tasks <- getTodos k
  case tasks of
    Left err -> return [("ERROR",show err)]
    Right tsks -> do
                  let x = map (\i -> (i^.todoBase . taskId , i^.todoBase . text)) tsks
                  return x

