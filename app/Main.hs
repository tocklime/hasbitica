{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Lens               ((^.))
import           Control.Monad.Trans.Either (runEitherT)
import           Data.Aeson                 (FromJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as B
import           Hasbitica.Api              (deleteTask, getTask, getTodos,
                                             postTask, todo)
import           Hasbitica.LensStuff
import           Hasbitica.Settings         (getApiFromSettings)
import           Servant.Common.Req         (responseBody)
import           System.Console.CmdArgs
import           System.Directory           (getHomeDirectory)
import           System.FilePath.Posix      ((</>))

data HasbiticaCli = New { newTodoText :: String }
               | List
               | Done { guid :: String }
               | Delete { guid :: String }
                 deriving (Show, Data, Typeable)

examples = [ New {newTodoText = "Buy milk" &= help "The text of the new todo" }
           , List &= help "List all not-done todos"
           , Done "<GUID>" &= help "Mark todo with guid X as done"
           , Delete "<GUID>" &= help "Delete todo X"]
             &= program "hasbitica-cli"

main :: IO ()
main = do
       x <- cmdArgs (modes examples) 
       (Just key) <- getApiFromSettings 
       case x of
         New x -> addTask key x >>= putStrLn
         List -> getAllTodos >>= mapM_ (putStrLn . (\(a,b) -> a++" "++b))
         Done x ->  undefined
         Delete x -> runEitherT (deleteTask key x) >>= print

addTask :: HabiticaApiKey -> String -> IO String
addTask k t = do
  x <- runEitherT $ postTask k (todo t)
  case x of
    Left err -> return (B.unpack $ responseBody err)
    Right _ -> return "OK"
main2 :: IO ()
main2 = do
  (Just k) <- getApiFromSettings
  addTask k "A HAHAHA" >>= putStrLn

getAllTodos :: IO [(String,String)]
getAllTodos = do
  (Just k) <- getApiFromSettings
  tasks <- runEitherT $ getTodos k
  case tasks of
    Left err -> return [("ERROR",show err)]
    Right tsks -> do
                  let x = map (\i -> (i^.todoBase . taskId , i^.todoBase . text)) tsks
                  return x

