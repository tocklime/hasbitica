{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Hasbitica
    (HabiticaApiKey
    ,getTasks
    ) where
import           Control.Arrow
import           Control.Monad              (mzero)
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import           Data.Aeson                 (FromJSON, Value (..), parseJSON,
                                             (.:), (.:?))
import           Data.List                  (filter, map)
import           Data.Map                   (Map (..), fromList, mapKeys,
                                             toList)
import           Data.Maybe                 (fromJust, isJust)
import           Data.Proxy                 (Proxy (..))
import           Data.Scientific            (floatingOrInteger)
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Data.Time.Clock.POSIX      (POSIXTime, posixSecondsToUTCTime)
import           Servant.API                
import           Servant.Client             
import           Servant.Common.Req         (ServantError)

import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

data HabiticaApiKey = HabiticaApiKey { authUser :: String, authApiKey :: String }

data RequireAuth

instance HasClient sub => HasClient (RequireAuth :> sub) where
  type Client (RequireAuth :> sub) = HabiticaApiKey -> Client sub

  clientWithRoute _ req baseurl HabiticaApiKey{..} =
    clientWithRoute
      (Proxy :: Proxy (Header "x-api-key" String :> Header "x-api-user" String :> sub))
      req
      baseurl
      (Just authApiKey)
      (Just authUser)


type Guid = String
type TaskId = String
type TagId = String


data TaskHistoryItem = TaskHistoryItem { histValue :: Double, date :: UTCTime }
  deriving Show

instance FromJSON POSIXTime where
  parseJSON (Number n) = case floatingOrInteger n of
                            Left f -> pure (realToFrac f/1000)
                            Right i -> pure (fromIntegral i/1000)
  parseJSON _ = mzero

instance FromJSON TaskHistoryItem where
  parseJSON (Object o) =
    TaskHistoryItem <$> o .: "value"
                    <*> (posixSecondsToUTCTime <$> o .: "date")
  parseJSON _ = mzero

data BaseTask = BaseTask
  { taskId      :: TaskId
  , dateCreated :: Maybe UTCTime
  , text        :: String
  , notes       :: String
  , tags        :: Map TagId Bool
  , taskValue   :: Double
  , priority    :: Int
  , attribute   :: String
  , challenge   :: ()
  } deriving (Show)

data Frequency = FreqDaily | FreqWeekly deriving (Show, Ord, Enum, Eq)
instance FromJSON Frequency where
  parseJSON (String "daily") = pure FreqDaily
  parseJSON (String "weekly") = pure FreqWeekly
  parseJSON _ = mzero

data CheckListItem = CheckListItem {cliText::String, id :: Guid, cliCompleted::Bool} deriving (Show)
instance FromJSON CheckListItem where
  parseJSON (Object o) = CheckListItem <$> o .: "text" <*> o .: "id" <*> o .: "completed"
  parseJSON _ = mzero

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Ord, Enum, Eq)
readDOW :: String -> Maybe DayOfWeek
readDOW "m" = Just Monday
readDOW "t" = Just Tuesday
readDOW "w" = Just Wednesday
readDOW "th" = Just Thursday
readDOW "f" = Just Friday
readDOW "s" = Just Saturday
readDOW "su" = Just Sunday
readDOW _ = Nothing

data TaskExt = Habit { history  :: [TaskHistoryItem]
                     , up, down :: Bool }
             | Daily { frequency         :: Frequency
                     , everyX            :: Int
                     , startDate         :: Maybe UTCTime
                     , history           :: [TaskHistoryItem]
                     , completed         :: Bool
                     , repeat            :: Map DayOfWeek Bool
                     , collapseChecklist :: Bool
                     , checklist         :: [CheckListItem] }
             | Todo  { completed         :: Bool
                     , dateCompleted     :: Maybe UTCTime
                     , dueDate           :: Maybe UTCTime
                     , collapseChecklist :: Bool
                     , checklist         :: [CheckListItem]}
             | Reward deriving (Show)

data Task = Task BaseTask TaskExt deriving (Show)

instance FromJSON BaseTask where
  parseJSON (Object o) =
    BaseTask <$> o .: "id"
             <*> o .: "dateCreated"
             <*> o .: "text"
             <*> o .: "notes"
             <*> o .: "tags"
             <*> o .: "value"
             <*> o .: "priority"
             <*> o .: "attribute"
             <*> pure ()

removeMaybe :: Ord k =>Map (Maybe k) v -> Map k v
removeMaybe = fromList . Data.List.map (first fromJust) . Data.List.filter (isJust . fst) . toList

instance FromJSON TaskExt where
  parseJSON (Object o) = do
    (t :: String) <- o .: "type"
    case t of
      "reward" -> pure Reward
      "habit" -> Habit <$> o .: "history" <*> pure False <*> pure False
      "daily" -> Daily <$> o .: "frequency"
                       <*> o .: "everyX"
                       <*> o .: "startDate"
                       <*> o .: "history"
                       <*> o .: "completed"
                       <*> (removeMaybe . mapKeys readDOW <$> o .: "repeat")
                       <*> o .: "collapseChecklist"
                       <*> o .: "checklist"
      "todo" -> Todo <$> o .: "completed"
                     <*> o .:? "dateCompleted"
                     <*> o .:? "date"
                     <*> o .: "collapseChecklist"
                     <*> o .: "checklist"

instance FromJSON Task where
  parseJSON o = Task <$> parseJSON o <*> parseJSON o
------------------------------------------------------------------------------
type HabiticaAPI = "api" :> "v2" :> (
             RequireAuth :>"user" :> "tasks" :> Get '[JSON] [Task] 
        :<|> RequireAuth :>"user" :> "tasks" :> Capture "taskId" String :> Get '[JSON] Task

        )

habiticaAPI :: Proxy HabiticaAPI
habiticaAPI = Proxy

getTasks :: HabiticaApiKey -> EitherT ServantError IO [Task]
getTask :: HabiticaApiKey -> String -> EitherT ServantError IO Task
(getTasks :<|> getTask) = client habiticaAPI (BaseUrl Https "habitica.com" 443)
