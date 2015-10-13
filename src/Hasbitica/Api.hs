{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Hasbitica.Api
    (getTasks
    ,getTask
    ,postTask
    ,HabiticaApiKey(..)
    ,todo
    ,getTodos
    ) where
import           Control.Applicative        ((<|>))
import           Control.Arrow
import           Control.Lens
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import           Data.Aeson                 (FromJSON, ToJSON, Value (..),
                                             object, parseJSON, toJSON, (.!=),
                                             (.:), (.:?), (.=))
import           Data.Map                   (fromList)
import           Data.Maybe                 (mapMaybe)
import           Data.Proxy                 (Proxy (..))
import           Hasbitica.Instances
import           Hasbitica.LensStuff
import           Servant.API
import           Servant.Client             (BaseUrl (..), Scheme (..), client)
import           Servant.Common.Req         (ServantError)


type HabiticaAPI = "api" :> "v2" :> (
       "status" :> Get '[JSON] Status
  :<|> RequireAuth :> "user" :> "tasks" :> Get '[JSON] [Task]
  :<|> RequireAuth :> "user" :> "tasks" :> Capture "taskId" String :> Get '[JSON] Task
  :<|> RequireAuth :> "user" :> "tasks" :> ReqBody '[JSON] Task :> Post '[JSON] Task
  :<|> RequireAuth :> "user" :> "tasks" :> Capture "taskId" String :> ReqBody '[JSON] Task :> Put '[JSON] Task)


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


type Habitica a = EitherT ServantError IO a

targetUrl = BaseUrl Https "habitica.com" 443

getStatus :: Habitica Status
getTasks :: HabiticaApiKey -> Habitica [Task]
getTask :: HabiticaApiKey -> String -> Habitica Task
postTask :: HabiticaApiKey -> Task -> Habitica Task
updateTask :: HabiticaApiKey -> String -> Task -> Habitica Task
getStatus
  :<|> getTasks
  :<|> getTask
  :<|> postTask
  :<|> updateTask = client (Proxy :: Proxy HabiticaAPI) targetUrl

---------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------
todo :: String -> Task
todo x = TaskTodo $ Todo (BaseTask "" Nothing x "" (fromList []) 0 0 "" ())
          False Nothing Nothing (Sublist [] True)

getTodos :: HabiticaApiKey -> Habitica [Todo]
getTodos key = filter (\x -> not $ x^.todoCompleted) . mapMaybe f <$> getTasks key
  where f (TaskTodo x) = Just x
        f _ = Nothing
