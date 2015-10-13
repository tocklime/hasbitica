{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Lens               ((^.))
import           Control.Monad.Trans.Either (runEitherT)
import           Data.Aeson                 (FromJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as B
import           Hasbitica.Api              (getTask, getTodos, postTask, todo)
import           Hasbitica.LensStuff        (HabiticaApiKey, text, todoBase)
import           Hasbitica.Settings         (getApiFromSettings)
import           Servant.Common.Req         (responseBody)
import           System.Directory           (getHomeDirectory)
import           System.FilePath.Posix      ((</>))


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

aTask = "a569065c-225d-4c47-aec2-54d815850e7f"
getATask :: IO ()
getATask = do
  (Just k) <- getApiFromSettings
  task <- runEitherT $ getTask k aTask
  print task
getAllTodos :: IO ()
getAllTodos = do
  (Just k) <- getApiFromSettings
  tasks <- runEitherT $ getTodos k
  case tasks of
    Left err -> print err
    Right tsks -> do
                  let x = map (\i -> i^.todoBase . text) tsks
                  print x

main = getAllTodos
