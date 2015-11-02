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
    ,deleteTask
    ,updateTask
    ,runHMonad
    ) where
import           Control.Applicative        ((<|>))
import           Control.Arrow
import           Control.Monad(liftM)
import           Control.Lens
import           Control.Monad.Trans (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import           Data.Aeson                 (FromJSON, ToJSON, Value (..),
                                             object, parseJSON, toJSON,
                                             withObject, (.!=), (.:), (.:?),
                                             (.=))
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (isInfixOf)
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
  :<|> "user" :> "tasks" :> (
       RequireAuth :> Get '[JSON] [Task]
  :<|> Capture "taskId" String :> RequireAuth :> Get '[JSON] Task
  :<|> ReqBody '[JSON] Task :> RequireAuth :> Post '[JSON] Task
  :<|> Capture "taskId" String :> ReqBody '[JSON] Task :> RequireAuth :> Put '[JSON] Task
  :<|> Capture "taskId" String :> RequireAuth :> Delete '[JSON] NoData)
  )

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

data HabiticaError = ClientError ServantError
type HMonad a = ReaderT HabiticaApiKey (EitherT ServantError IO) a
type HMonad' a = EitherT ServantError (ReaderT HabiticaApiKey IO) a

type Habitica a = EitherT ServantError IO a
type HabiticaAuth a = HabiticaApiKey -> EitherT ServantError IO a

runHabitica :: Habitica a -> IO (Either ServantError a)
runHabitica = runEitherT 

data NoData = NoData deriving Show
instance FromJSON NoData where
  parseJSON = withObject "NoData" $ \o ->
                 if HM.null o then pure NoData else fail "Expected empty object"

targetUrl = BaseUrl Https "habitica.com" 443

getStatus' :: Habitica Status
getTasks' :: HabiticaAuth [Task]
getTask' :: String -> HabiticaAuth Task
postTask' :: Task -> HabiticaAuth Task
updateTask' :: String -> Task -> HabiticaAuth Task
deleteTask' :: String -> HabiticaAuth NoData

getStatus'
  :<|> getTasks'
  :<|> getTask'
  :<|> postTask'
  :<|> updateTask'
  :<|> deleteTask' = client (Proxy :: Proxy HabiticaAPI) targetUrl

runHMonad' :: HMonad' a -> HabiticaApiKey -> IO (Either String a)
runHMonad' x key = left show <$> runReaderT (runEitherT x) key

runHMonad :: HMonad a -> HabiticaApiKey -> IO (Either String a)
runHMonad x key = left show <$> runEitherT (runReaderT x key)

getTasks :: HMonad [Task]
getTask :: String -> HMonad Task
postTask :: Task -> HMonad Task
updateTask :: String -> Task -> HMonad Task
deleteTask :: String -> HMonad NoData

getTasks = ask >>= lift . getTasks'
getTask guid = ask >>= lift . getTask' guid
postTask t = ask >>= lift . postTask' t
updateTask guid task = ask >>= lift . updateTask' guid task
deleteTask t = ask >>= lift . deleteTask' t
  

---------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------
todo :: String -> Task
todo x = TaskTodo $ Todo (BaseTask "" Nothing x "" (fromList []) 0 0 "" ())
          False Nothing Nothing (Sublist [] True)

getTodos :: HabiticaApiKey -> IO (Either String [Todo])
getTodos key = right (filter (\x -> not $ x^.todoCompleted) . mapMaybe fromTask) <$> runHMonad getTasks key


findTasks :: HabiticaApiKey -> String -> IO (Either String [Task])
findTasks key s = right(filter (\x -> s `isInfixOf` (toBase x ^. text))) <$> runHMonad getTasks key
