{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Hasbitica.Instances() where
import           Control.Arrow         (first)
import           Control.Monad         (mzero,guard)
import           Data.Aeson            (FromJSON, ToJSON, Value (..), object,
                                        parseJSON, toJSON, (.!=), (.:), (.:?),
                                        (.=))
import Data.Aeson.Types
import qualified Data.HashMap.Strict   as HM
import           Data.List             (filter, map)
import           Data.Map              (Map (..), fromList, mapKeys, toList)
import           Data.Maybe            (catMaybes, fromJust, isJust)
import           Data.Proxy            (Proxy (..))
import           Data.Scientific       (floatingOrInteger)
import           Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import           Hasbitica.LensStuff
import           Servant.API           ((:>), Header)
import           Servant.Client        (HasClient (..))

readDOW :: String -> Maybe DayOfWeek
readDOW "m" = Just Monday
readDOW "t" = Just Tuesday
readDOW "w" = Just Wednesday
readDOW "th" = Just Thursday
readDOW "f" = Just Friday
readDOW "s" = Just Saturday
readDOW "su" = Just Sunday
readDOW _ = Nothing

instance HasClient sub => HasClient (RequireAuth :> sub) where
  type Client (RequireAuth :> sub) = HabiticaApiKey -> Client sub

  clientWithRoute _ req baseurl HabiticaApiKey{..} =
    clientWithRoute
      (Proxy :: Proxy (Header "x-api-key" String :> Header "x-api-user" String :> sub))
      req
      baseurl
      (Just authApiKey)
      (Just authUser)

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

instance FromJSON Frequency where
  parseJSON (String "daily") = pure FreqDaily
  parseJSON (String "weekly") = pure FreqWeekly
  parseJSON _ = mzero

instance FromJSON CheckListItem where
  parseJSON (Object o) = CheckListItem <$> o .: "text" <*> o .: "id" <*> o .: "completed"
  parseJSON _ = mzero
instance ToJSON CheckListItem where
  toJSON CheckListItem{..} = object [ "text" .= cliText
                                    , "id" .= cliId
                                    , "completed" .= cliCompleted ]

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


instance FromJSON Sublist where
  parseJSON (Object o) = Sublist <$> o .:? "checklist" .!= [] <*> o .:? "collapseChecklist" .!= True
instance ToJSON Sublist where
  toJSON Sublist{..} = object ["checklist" .= _checklist, "collapseChecklist" .= _collapse]
instance FromJSON Todo where
  parseJSON v@(Object o) = do
    ("todo" :: String) <- o .: "type"
    Todo <$> parseJSON v
         <*> o .: "completed"
         <*> o .: "dateCompleted"
         <*> o .: "date"
         <*> parseJSON v
instance ToJSON Todo where
  toJSON Todo{..} = mergeValue (toJSON _todoBase) $ 
                  mergeValue (toJSON _todoSublist) $ 
                  object [ "type" .= ("todo"::String)
                           , "completed" .= _todoCompleted
                           , "dateCompleted" .= _todoDateCompleted
                           , "date" .= _todoDueDate
                         ]

instance FromJSON Reward where
  parseJSON v@(Object o) = do
    ("reward" :: String) <- o .: "type"
    Reward <$> parseJSON v

instance FromJSON Daily where
  parseJSON v@(Object o) = do
    ("daily" :: String) <- o .: "type"
    Daily <$> parseJSON v
          <*> o .: "frequency"
          <*> o .: "everyX"
          <*> o .: "startDate"
          <*> o .: "history"
          <*> o .: "completed"
          <*> (removeMaybe . mapKeys readDOW <$> o .: "repeat")
          <*> parseJSON v
instance FromJSON Habit where
  parseJSON v@(Object o) = do
    ("habit" :: String) <- o .: "type"
    Habit <$> parseJSON v
          <*> o .: "history"
          <*> o .: "up"
          <*> o .: "down"
{-
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
-}
mergeValue :: Value -> Value -> Value
mergeValue (Object a) (Object b) = Object (HM.unionWith mergeValue a b)
mergeValue a _ = a

instance ToJSON BaseTask where
  toJSON BaseTask{..} = object $ catMaybes
    [ m (_taskId /= "") $ "id" .= _taskId --should send this if it's not "", but not otherwise. Also, it should be Maybe TaskId.
    , m (isJust _dateCreated) $ "dateCreated" .= _dateCreated --ditto
    , Just $ "text" .= _text
    , Just $ "notes" .= _notes
    , Just $ "tags" .= _tags
    , Just $ "value" .= _taskValue
    , Just $ "priority" .= _priority
    --, Just $ "attribute" .= attribute
    , Just $ "challenge" .= _challenge
    ]
   where m False _ = Nothing
         m True  a = Just a


instance FromJSON Status where
  parseJSON (Object o) = do
    (s::String) <- o .: "status"
    case s of
      "up" -> pure Up
      "down" -> pure Down
