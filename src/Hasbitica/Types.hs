{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Hasbitica.Types
    (HabiticaApiKey(..)
    ,RequireAuth(..)
    ,Guid
    ,TaskHistoryItem(..)
    ,BaseTask(..)
    ,Frequency(..)
    ,CheckListItem(..)
    ,DayOfWeek(..)
    ,TaskExt(..)
    ,Task(..)
    ,Status(..)
    ,readDOW
    ) where
import           Data.Map  (Map (..))
import           Data.Time (UTCTime)

data HabiticaApiKey = HabiticaApiKey { authUser :: String, authApiKey :: String }

data RequireAuth

type Guid = String

data TaskHistoryItem = TaskHistoryItem { histValue :: Double, date :: UTCTime }
  deriving Show

data BaseTask = BaseTask
  { taskId      :: Guid
  , dateCreated :: Maybe UTCTime
  , text        :: String
  , notes       :: String
  , tags        :: Map Guid Bool
  , taskValue   :: Double
  , priority    :: Int
  , attribute   :: String
  , challenge   :: ()
  } deriving (Show)

data Frequency = FreqDaily | FreqWeekly deriving (Show, Ord, Enum, Eq)
data CheckListItem = CheckListItem {cliText::String, cliId :: Guid, cliCompleted::Bool} deriving (Show)

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

data Status = Up | Down deriving (Eq,Ord,Enum,Show)

