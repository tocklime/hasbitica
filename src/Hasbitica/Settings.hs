{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hasbitica.Settings where

import           Control.Applicative ((<|>))
import           Data.Maybe(listToMaybe,maybeToList)
import           Data.Aeson                 (FromJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as B
import           GHC.Generics
import           Hasbitica.Api
import           System.Directory           (getHomeDirectory)
import           System.FilePath.Posix      ((</>))


data Settings = Settings {address,user,key::String}
  deriving (Show,Generic)

instance FromJSON Settings

readSettings :: IO (Maybe Settings)
readSettings = fmap (</> ".habitica") getHomeDirectory >>= fmap decode . B.readFile

readMultiSettings :: IO [Settings]
readMultiSettings = fmap (</> ".habitica") getHomeDirectory >>= fmap (concat . decode) . B.readFile

settingsToKey :: Settings -> HabiticaApiKey
settingsToKey Settings{..} = HabiticaApiKey user key

getApiFromSettings :: IO (Maybe HabiticaApiKey)
getApiFromSettings = do
   sing <- readSettings
   many <- readMultiSettings
   return $ settingsToKey <$> (sing <|> listToMaybe many)

getAllApisFromSettings :: IO [HabiticaApiKey]
getAllApisFromSettings = do
   sing <- readSettings
   many <- readMultiSettings
   return $ settingsToKey <$> (maybeToList sing ++ many)


