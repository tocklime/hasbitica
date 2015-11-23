{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Hasbitica.Api
    (getStatus
    ,getTasks
    ,getTask
    ,getUser
    ,postTask
    ,HabiticaApiKey(..)
    ,todo
    ,getTodos
    ,deleteTask
    ,updateTask
    ,runHMonad
    ,HMonad
    ,findTasks
    ) where
import           Control.Arrow
import           Control.Lens
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import           Data.Aeson                 (FromJSON,  Value (..),
                                              parseJSON, 
                                             withObject 
                                             )
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (isInfixOf)
import           Data.Map                   (fromList)
import           Data.Maybe                 (mapMaybe)
import           Data.Proxy                 (Proxy (..))
import           Hasbitica.LensStuff
import           Servant.API
import           Servant.Client             (BaseUrl (..), Scheme (..), client, HasClient, Client,clientWithRoute)
import           Servant.Common.Req         (ServantError)


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

type HabiticaAPI = "api" :> "v2" :> (
       "status" :> Get '[JSON] Status
  :<|> "user" :> RequireAuth :> Get '[JSON] Value
  :<|> "user" :> "tasks" :> (
           RequireAuth :> Get '[JSON] [Task] 
      :<|> Capture "taskId" String :> RequireAuth :> Get '[JSON] Task 
      :<|> ReqBody '[JSON] Task :> RequireAuth :> Post '[JSON] Task 
      :<|> Capture "taskId" String :> ReqBody '[JSON] Task :> RequireAuth :> Put '[JSON] Task
      :<|> Capture "taskId" String :> RequireAuth :> Delete '[JSON] NoData
      )
  )


type HMonad a = ReaderT HabiticaApiKey (EitherT ServantError IO) a

type Habitica a = EitherT ServantError IO a
type HabiticaAuth a = HabiticaApiKey -> EitherT ServantError IO a

data NoData = NoData deriving Show
instance FromJSON NoData where
  parseJSON = withObject "NoData" $ \o ->
                 if HM.null o then pure NoData else fail "Expected empty object"

targetUrl :: BaseUrl
targetUrl = BaseUrl Https "habitica.com" 443

getStatus' :: Habitica Status
getTasks' :: HabiticaAuth [Task]
getTask' :: String -> HabiticaAuth Task
postTask' :: Task -> HabiticaAuth Task
updateTask' :: String -> Task -> HabiticaAuth Task
deleteTask' :: String -> HabiticaAuth NoData
getUser' :: HabiticaAuth Value

getStatus'
  :<|> getUser' 
  :<|> getTasks'
  :<|> getTask'
  :<|> postTask'
  :<|> updateTask'
  :<|> deleteTask' 
  = client (Proxy :: Proxy HabiticaAPI) targetUrl

runHMonad :: HMonad a -> HabiticaApiKey -> IO (Either String a)
runHMonad x key = left show <$> runEitherT (runReaderT x key)

getStatus :: HMonad Status
getTasks :: HMonad [Task]
getTask :: String -> HMonad Task
postTask :: Task -> HMonad Task
updateTask :: String -> Task -> HMonad Task
deleteTask :: String -> HMonad NoData
getUser :: HMonad Value

getStatus = lift getStatus'
getTasks = ask >>= lift . getTasks'
getTask guid = ask >>= lift . getTask' guid
postTask t = ask >>= lift . postTask' t
updateTask guid task = ask >>= lift . updateTask' guid task
deleteTask t = ask >>= lift . deleteTask' t
getUser = ask >>= lift . getUser' 
  

---------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------
todo :: String -> Task
todo x = TaskTodo $ Todo (BaseTask "" Nothing x "" (fromList []) 0 0 "" ())
          False Nothing Nothing (Sublist [] True)

getTodos :: HMonad [Todo]
getTodos = filter (\x -> not $ x^.todoCompleted) . mapMaybe fromTask <$> getTasks 

findTasks :: HabiticaApiKey -> String -> IO (Either String [Task])
findTasks key s = right(filter (\x -> s `isInfixOf` (toBase x ^. text))) <$> runHMonad getTasks key
