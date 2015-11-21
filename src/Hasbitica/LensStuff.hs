{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Hasbitica.LensStuff
     where
import           Control.Lens hiding ((.=))
import           Data.Map     (Map)
import           Data.Time    (UTCTime)
import           Data.Aeson                 (FromJSON, ToJSON, Value (..),
                                             object, parseJSON, toJSON,
                                             (.!=), (.:), (.:?), (.=))
import           Control.Applicative   ((<|>))
import           Control.Arrow         (first)
import           Control.Monad         (mzero)
import qualified Data.HashMap.Strict   as HM
import           Data.List             (filter, map)
import           Data.Map              (fromList, mapKeys, toList)
import           Data.Maybe            (catMaybes, fromJust, isJust)
import           Data.Scientific       (floatingOrInteger)
import           Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)

data HabiticaApiKey = HabiticaApiKey { authUser :: String, authApiKey :: String } deriving Show

type Guid = String

data TaskHistoryItem = TaskHistoryItem { _histValue :: Double, _histDate :: UTCTime }
  deriving Show

data BaseTask = BaseTask
  { _taskId      :: Guid
  , _dateCreated :: Maybe UTCTime
  , _text        :: String
  , _notes       :: String
  , _tags        :: Map Guid Bool
  , _taskValue   :: Double
  , _priority    :: Int
  , _attribute   :: String
  , _challenge   :: ()
  } deriving (Show)
data Frequency = FreqDaily | FreqWeekly deriving (Show, Ord, Enum, Eq)
data CheckListItem = CheckListItem {cliText::String, cliId :: Guid, cliCompleted::Bool} deriving (Show)

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Ord, Enum, Eq)

data Status = Up | Down deriving (Eq,Ord,Enum,Show)

data Sublist = Sublist { _checklist :: [CheckListItem], _collapse :: Bool }
  deriving (Show)

data Habit = Habit { _habitBase    :: BaseTask
                   , _habitHistory :: [TaskHistoryItem]
                   , _habitUp      :: Bool
                   , _habitDown    :: Bool }
  deriving (Show)
data Todo = Todo { _todoBase          :: BaseTask
                 , _todoCompleted     :: Bool
                 , _todoDateCompleted :: Maybe UTCTime
                 , _todoDueDate       :: Maybe UTCTime
                 , _todoSublist       :: Sublist
                 } deriving (Show)

data Daily = Daily { _dailyBase      :: BaseTask
                   , _dailyFrequency :: Frequency
                   , _dailyEveryX    :: Int
                   , _dailyStartDate :: Maybe UTCTime
                   , _dailyHistory   :: [TaskHistoryItem]
                   , _dailyCompleted :: Bool
                   , _dailyRepeat    :: Map DayOfWeek Bool
                   , _dailySublist   :: Sublist }
  deriving (Show)


data Reward = Reward { _rewardBase :: BaseTask }
  deriving (Show)

data Task = TaskTodo Todo | TaskHabit Habit | TaskDaily Daily | TaskReward Reward
  deriving (Show)
instance ToJSON Task where
  toJSON (TaskTodo t) = toJSON t
  toJSON (TaskHabit t) = toJSON t
  toJSON (TaskDaily t) = toJSON t
  toJSON (TaskReward t) = toJSON t
instance FromJSON Task where
  parseJSON v@(Object o) = do
    (x::String) <- o .: "type"
    case x of
      "todo" -> TaskTodo <$> parseJSON v
      "habit" -> TaskHabit <$>parseJSON v
      "reward" -> TaskReward <$>parseJSON v
      "daily" -> TaskDaily <$>parseJSON v
      _ -> mzero
  parseJSON _ = mzero


class HasBaseTask a where
   toBase :: a -> BaseTask
   fromTask :: Task -> Maybe a

readDOW :: String -> Maybe DayOfWeek
readDOW "m" = Just Monday
readDOW "t" = Just Tuesday
readDOW "w" = Just Wednesday
readDOW "th" = Just Thursday
readDOW "f" = Just Friday
readDOW "s" = Just Saturday
readDOW "su" = Just Sunday
readDOW _ = Nothing
writeDOW :: DayOfWeek -> String
writeDOW Monday = "m"
writeDOW Tuesday = "t"
writeDOW Wednesday = "w"
writeDOW Thursday = "th"
writeDOW Friday = "f"
writeDOW Saturday = "s"
writeDOW Sunday = "su"


instance FromJSON POSIXTime where
  parseJSON (Number n) = case floatingOrInteger n of
                            Left (f::Double) -> pure (realToFrac f/1000)
                            Right (i::Integer) -> pure (fromIntegral i/1000)
  parseJSON (String _) = undefined
  parseJSON x = error ("bad posixtime: " ++ show x)

instance FromJSON TaskHistoryItem where
  parseJSON (Object o) =
    TaskHistoryItem <$> o .: "value"
                    <*> ((o .: "date") <|> (posixSecondsToUTCTime <$> o .: "date"))
  parseJSON x = error ("bad taskHistItem: " ++ show x)
instance ToJSON TaskHistoryItem where
  toJSON TaskHistoryItem{..} = object [ "value" .= _histValue, "date" .= _histDate ]

instance FromJSON Frequency where
  parseJSON (String "daily") = pure FreqDaily
  parseJSON (String "weekly") = pure FreqWeekly
  parseJSON _ = mzero
instance ToJSON Frequency where
  toJSON FreqDaily = String "daily"
  toJSON FreqWeekly = String "weekly"

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
  parseJSON _ = mzero

removeMaybe :: Ord k =>Map (Maybe k) v -> Map k v
removeMaybe = fromList . Data.List.map (first fromJust) . Data.List.filter (isJust . fst) . toList


instance FromJSON Sublist where
  parseJSON (Object o) = Sublist <$> o .:? "checklist" .!= [] <*> o .:? "collapseChecklist" .!= True
  parseJSON _ = mzero

instance ToJSON Sublist where
  toJSON Sublist{..} = object ["checklist" .= _checklist, "collapseChecklist" .= _collapse]
instance FromJSON Todo where
  parseJSON v@(Object o) = do
    ("todo" :: String) <- o .: "type"
    Todo <$> parseJSON v
         <*> o .: "completed"
         <*> o .:? "dateCompleted"
         <*> o .:? "date"
         <*> parseJSON v
  parseJSON _ = mzero
instance ToJSON Todo where
  toJSON Todo{..} = mergeValue _todoBase $
                  mergeValue _todoSublist $
                  object [ "type" .= ("todo"::String)
                           , "completed" .= _todoCompleted
                           , "dateCompleted" .= _todoDateCompleted
                           , "date" .= _todoDueDate
                         ]

instance FromJSON Reward where
  parseJSON v@(Object o) = do
    ("reward" :: String) <- o .: "type"
    Reward <$> parseJSON v
  parseJSON _ = mzero
instance ToJSON Reward where
  toJSON Reward{..} = mergeValue _rewardBase $ object [ "type" .= ("reward"::String) ]

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
  parseJSON _ = mzero
instance ToJSON Daily where
  toJSON Daily{..} = mergeValue _dailyBase $ mergeValue _dailySublist $ object
    [ "type" .= ("daily"::String)
    , "frequency" .= _dailyFrequency
    , "everyX" .= _dailyEveryX
    , "startDate" .= _dailyStartDate
    , "history" .= _dailyHistory
    , "completed" .= _dailyCompleted
    , "repeat" .= mapKeys writeDOW _dailyRepeat
    ]
instance FromJSON Habit where
  parseJSON v@(Object o) = do
    ("habit" :: String) <- o .: "type"
    Habit <$> parseJSON v
          <*> o .: "history"
          <*> o .: "up"
          <*> o .: "down"
  parseJSON _ = mzero
instance ToJSON Habit where
  toJSON Habit{..} = mergeValue _habitBase $ object
    [ "type" .= ("habit"::String)
    , "history" .= _habitHistory
    , "up" .= _habitUp
    , "down" .= _habitDown
    ]

mergeValue :: ToJSON a => a -> Value -> Value
mergeValue a (Object b) = case toJSON a of
  (Object a') -> Object (HM.unionWith mergeValue a' b)
  _ -> Object b
mergeValue a _ = toJSON a

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
      _ -> mzero
  parseJSON _ = mzero

instance HasBaseTask Todo where
   toBase = _todoBase
   fromTask (TaskTodo a) = Just a
   fromTask _ = Nothing

instance HasBaseTask Habit where
   toBase = _habitBase
   fromTask (TaskHabit a) = Just a
   fromTask _ = Nothing

instance HasBaseTask Daily where
   toBase = _dailyBase
   fromTask (TaskDaily a) = Just a
   fromTask _ = Nothing

instance HasBaseTask Reward where
   toBase = _rewardBase
   fromTask (TaskReward a) = Just a
   fromTask _ = Nothing

instance HasBaseTask Task where
   toBase (TaskTodo a) = toBase a
   toBase (TaskHabit a) = toBase a
   toBase (TaskDaily a) = toBase a
   toBase (TaskReward a) = toBase a
   fromTask = Just


makeLenses ''BaseTask
makeLenses ''Todo
makeLenses ''Reward
makeLenses ''Daily
makeLenses ''Habit
makeLenses ''Sublist
makeLenses ''TaskHistoryItem
