{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Hasbitica.Api
    (getTasks
    ,postTask
    ,HabiticaApiKey(..)
    ,todo
    ) where
import           Control.Arrow
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import           Data.Map                   (fromList)
import           Data.Proxy                 (Proxy (..))
import           Hasbitica.Instances
import           Hasbitica.LensStuff
import           Servant.API
import           Servant.Client             (BaseUrl (..), Scheme (..), client)
import           Servant.Common.Req         (ServantError)
import           Data.Aeson            (FromJSON, ToJSON, Value (..), object,
                                        parseJSON, toJSON, (.!=), (.:), (.:?),
                                        (.=))
import Control.Applicative ((<|>))

type HabiticaAPI = "api" :> "v2" :> (
             "status" :> Get '[JSON] Status
        :<|> RequireAuth :>"user" :> "tasks" :> Get '[JSON] [Task]
        :<|> RequireAuth :>"user" :> "tasks" :> Capture "taskId" String :> Get '[JSON] Task
        :<|> RequireAuth :> "user" :> "tasks" :> ReqBody '[JSON] Task :> Post '[JSON] Task

        )
data Task = TaskTodo Todo | TaskHabit Habit | TaskDaily Daily | TaskReward Reward
instance ToJSON Task where
  toJSON (TaskTodo t) = toJSON t
  toJSON (TaskHabit t) = undefined --toJSON t
  toJSON (TaskDaily t) = undefined --toJSON t
  toJSON (TaskReward t) = undefined --toJSON t
instance FromJSON Task where
  parseJSON o = TaskTodo <$> (parseJSON o) 
            <|> TaskHabit <$>undefined --(parseJSON o) 
            <|> TaskDaily <$>undefined --(parseJSON o) 
            <|> TaskReward <$>undefined --(parseJSON o)

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
todo x = TaskTodo $ Todo (BaseTask "" Nothing x "" (fromList []) 0 0 "" ())
          False Nothing Nothing (Sublist [] True)
