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
    ,WithJson(..)
    ,runHabitica
    ,runHabiticaWithJson
    ) where
import           Control.Applicative        ((<|>))
import           Control.Arrow
import           Control.Lens
import           Control.Monad.Trans.Either (EitherT, runEitherT)
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
       "status" :> Get '[JSON] (WithJson Status)
  :<|> RequireAuth :> "user" :> "tasks" :> Get '[JSON] (WithJson [Task])
  :<|> RequireAuth :> "user" :> "tasks" :> Capture "taskId" String :> Get '[JSON] (WithJson Task)
  :<|> RequireAuth :> "user" :> "tasks" :> ReqBody '[JSON] Task :> Post '[JSON] (WithJson Task)
  :<|> RequireAuth :> "user" :> "tasks" :> Capture "taskId" String :> ReqBody '[JSON] Task :> Put '[JSON] (WithJson Task)
  :<|> RequireAuth :> "user" :> "tasks" :> Capture "taskId" String :> Delete '[JSON] (WithJson NoData))

data WithJson a = WithJson Value a
instance FromJSON a => FromJSON (WithJson a) where
  parseJSON v = WithJson v <$> parseJSON v


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


type Habitica a = EitherT ServantError IO (WithJson a)

runHabitica :: Habitica a -> IO (Either ServantError a)
runHabitica x = right f <$> runEitherT x
  where
   f (WithJson _ a) = a
runHabiticaWithJson :: Habitica a -> IO (Either ServantError (Value,a))
runHabiticaWithJson x = right f <$> runEitherT x
  where
   f (WithJson a b) = (a,b)

data NoData = NoData deriving Show
instance FromJSON NoData where
  parseJSON = withObject "NoData" $ \o ->
                 if HM.null o then pure NoData else fail "Expected empty object"

targetUrl = BaseUrl Https "habitica.com" 443

getStatus :: Habitica Status
getTasks :: HabiticaApiKey -> Habitica [Task]
getTask :: HabiticaApiKey -> String -> Habitica Task
postTask :: HabiticaApiKey -> Task -> Habitica Task
updateTask :: HabiticaApiKey -> String -> Task -> Habitica Task
deleteTask :: HabiticaApiKey -> String -> Habitica NoData

getStatus
  :<|> getTasks
  :<|> getTask
  :<|> postTask
  :<|> updateTask
  :<|> deleteTask = client (Proxy :: Proxy HabiticaAPI) targetUrl

---------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------
todo :: String -> Task
todo x = TaskTodo $ Todo (BaseTask "" Nothing x "" (fromList []) 0 0 "" ())
          False Nothing Nothing (Sublist [] True)

getTodos :: HabiticaApiKey -> IO (Either ServantError [Todo])
getTodos key = right (filter (\x -> not $ x^.todoCompleted) . mapMaybe fromTask) <$> runHabitica ( getTasks key)


findTasks :: HabiticaApiKey -> String -> IO (Either ServantError [Task])
findTasks key s = right(filter (\x -> s `isInfixOf` (toBase x ^. text))) <$> runHabitica (getTasks key)
