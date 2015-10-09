{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad.Trans.Either
import           Data.Aeson                 (FromJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as B
import           GHC.Generics
import           Hasbitica.Api
import           Hasbitica.LensStuff
import           Control.Lens
import           Servant.Common.Req
import           System.Directory           (getHomeDirectory)
import           System.FilePath.Posix      ((</>))

data Settings = Settings {address,user,key::String}
  deriving (Show,Generic)

instance FromJSON Settings

readSettings :: IO (Maybe Settings)
readSettings = fmap (</> ".habitica") getHomeDirectory >>= fmap decode . B.readFile

addTask :: HabiticaApiKey -> String -> IO String
addTask k t = do
  x <- runEitherT $ postTask k (todo t)
  case x of
    Left err -> return (B.unpack $ responseBody err)
    Right _ -> return "OK"
main2 :: IO ()
main2 = do
  (Just s) <- readSettings
  let k = HabiticaApiKey (user s) (key s)
  addTask k "A HAHAHA" >>= putStrLn

aTask = "a569065c-225d-4c47-aec2-54d815850e7f"
getATask :: IO ()
getATask = do
  (Just s) <- readSettings
  let k = HabiticaApiKey (user s) (key s)
  task <- runEitherT $ getTask k aTask
  print task
getAllTodos :: IO ()
getAllTodos = do
  (Just s) <- readSettings
  let k = HabiticaApiKey (user s) (key s)
  tasks <- runEitherT $ getTodos k
  case tasks of
    Left err -> print err
    Right tsks -> do
		  let x = map (\i -> i^.todoBase . text) tsks
		  print x

main = getAllTodos
