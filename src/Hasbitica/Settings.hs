{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hasbitica.Settings where

import           Control.Lens
import           Control.Monad.Trans.Either
import           Data.Aeson                 (FromJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as B
import           GHC.Generics
import           Hasbitica.Api
import           Hasbitica.LensStuff
import           Servant.Common.Req
import           System.Directory           (getHomeDirectory)
import           System.FilePath.Posix      ((</>))


data Settings = Settings {address,user,key::String}
  deriving (Show,Generic)

instance FromJSON Settings

readSettings :: IO (Maybe Settings)
readSettings = fmap (</> ".habitica") getHomeDirectory >>= fmap decode . B.readFile

getApiFromSettings :: IO (Maybe HabiticaApiKey)
getApiFromSettings = readSettings >>= return . fmap x
  where
    x Settings{..} =  HabiticaApiKey user key
