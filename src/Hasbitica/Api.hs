{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Hasbitica.Api
    (HabiticaApiKey
    ,getTasks
    ) where
import           Control.Arrow
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import           Data.Map                   (fromList)
import           Data.Proxy                 (Proxy (..))
import           Hasbitica.Instances
import           Hasbitica.Types
import           Servant.API
import           Servant.Client             (BaseUrl (..), Scheme (..), client)
import           Servant.Common.Req         (ServantError)

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
