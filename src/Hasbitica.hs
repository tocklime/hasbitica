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
import           Data.Aeson                 (FromJSON, ToJSON, Value (..),
                                             object, parseJSON, toJSON, (.!=),
                                             (.:), (.:?), (.=))
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (filter, map)
import           Data.Map                   (Map (..), fromList, mapKeys,
                                             toList)
import           Data.Maybe                 (fromJust, isJust, catMaybes)
import           Data.Monoid                ((<>))
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

data CheckListItem = CheckListItem {cliText::String, cliId :: Guid, cliCompleted::Bool} deriving (Show)
instance FromJSON CheckListItem where
  parseJSON (Object o) = CheckListItem <$> o .: "text" <*> o .: "id" <*> o .: "completed"
  parseJSON _ = mzero
instance ToJSON CheckListItem where
  toJSON CheckListItem{..} = object [ "text" .= cliText
                                    , "id" .= cliId
				    , "completed" .= cliCompleted ]

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
                       <*> o .:? "collapseChecklist" .!= True
                       <*> o .:? "checklist" .!= []
      "todo" -> Todo <$> o .: "completed"
                     <*> o .:? "dateCompleted"
                     <*> o .:? "date"
                     <*> o .:? "collapseChecklist" .!= True
                     <*> o .:? "checklist" .!= []
instance ToJSON TaskExt where
  toJSON Reward = object [ "type" .= ("reward"::String) ]
  toJSON Habit{..} = undefined
  toJSON Daily{..} = undefined
  toJSON Todo{..} = object [ "type" .= ("todo"::String)
                           --, "completed" .= completed
                           --, "dateCompleted" .= dateCompleted
                           --, "date" .= dueDate
                           --, "collapseChecklist" .= collapseChecklist
                           ]--, "checklist" .= checklist ]

mergeValue :: Value -> Value -> Value
mergeValue (Object a) (Object b) = Object (HM.unionWith mergeValue a b)
mergeValue a _ = a

instance FromJSON Task where
  parseJSON o = Task <$> parseJSON o <*> parseJSON o
instance ToJSON Task where
  toJSON (Task base ext) = mergeValue (toJSON base) (toJSON ext)

instance ToJSON BaseTask where
  toJSON BaseTask{..} = object $ catMaybes 
    [ m (taskId /= "") $ "id" .= taskId --should send this if it's not "", but not otherwise. Also, it should be Maybe TaskId.
    , m (dateCreated /= Nothing) $ "dateCreated" .= dateCreated --ditto
    , Just $ "text" .= text
    , Just $ "notes" .= notes
    , Just $ "tags" .= tags
    , Just $ "value" .= taskValue
    , Just $ "priority" .= priority
    --, Just $ "attribute" .= attribute
    , Just $ "challenge" .= challenge
    ]
   where m False _ = Nothing
         m True  a = Just a


data Status = Up | Down deriving (Eq,Ord,Enum,Show)
instance FromJSON Status where
  parseJSON (Object o) = do
    (s::String) <- o .: "status"
    case s of
      "up" -> pure Up
      "down" -> pure Down

------------------------------------------------------------------------------
type HabiticaAPI = "api" :> "v2" :> (
             "status" :> Get '[JSON] Status
        :<|> RequireAuth :>"user" :> "tasks" :> Get '[JSON] [Task]
        :<|> RequireAuth :>"user" :> "tasks" :> Capture "taskId" String :> Get '[JSON] Task
        :<|> RequireAuth :> "user" :> "tasks" :> ReqBody '[JSON] Task :> Post '[JSON] Task

        )

habiticaAPI :: Proxy HabiticaAPI
habiticaAPI = Proxy

type Habitica a = EitherT ServantError IO a

getStatus :: Habitica Status
getTasks :: HabiticaApiKey -> Habitica [Task]
getTask :: HabiticaApiKey -> String -> Habitica Task
postTask :: HabiticaApiKey -> Task -> Habitica Task
getStatus
  :<|> getTasks
  :<|> getTask
  :<|> postTask = client habiticaAPI (BaseUrl Https "habitica.com" 443)

todo :: String -> Task
todo x = Task (BaseTask "" Nothing x "" (fromList []) 0 0 "" ())
              (Todo False Nothing Nothing True [])
