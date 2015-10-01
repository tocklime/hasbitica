{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TemplateHaskell     #-}

module Hasbitica.LensStuff
     where
import           Data.Map  (Map (..))
import           Data.Time (UTCTime)
import Control.Lens

data HabiticaApiKey = HabiticaApiKey { authUser :: String, authApiKey :: String }

data RequireAuth

type Guid = String

data TaskHistoryItem = TaskHistoryItem { histValue :: Double, date :: UTCTime }
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

data Habit = Habit { _habitHistory  :: [TaskHistoryItem]
                   , _habitUp :: Bool
                   , _habitDown :: Bool }
  deriving (Show)
data Todo = Todo { _todoBase :: BaseTask
                 , _todoCompleted :: Bool
                 , _todoDateCompleted :: Maybe UTCTime
                 , _todoDueDate :: Maybe UTCTime
                 , _todoSublist :: Sublist
                 } deriving (Show)

data Daily = Daily { _dailyBase :: BaseTask
                   , _dailyFrequency         :: Frequency
                   , _dailyEveryX            :: Int
                   , _dailyStartDate         :: Maybe UTCTime
                   , _dailyHistory           :: [TaskHistoryItem]
                   , _dailyCompleted         :: Bool
                   , _dailyRepeat            :: Map DayOfWeek Bool
                   , _dailySublist      :: Sublist }
  deriving (Show)


data Reward = Reward { _rewardBase :: BaseTask }
  deriving (Show)

makeLenses ''BaseTask
makeLenses ''Todo
makeLenses ''Reward
makeLenses ''Daily
makeLenses ''Habit
makeLenses ''Sublist
