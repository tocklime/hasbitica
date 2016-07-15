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
    ,getPartyChat
    ,HabiticaContext(..)
    ) where
import           Control.Arrow
import           Control.Lens
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import           Control.Monad.Trans.Except 
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
import           Servant.Client             (BaseUrl (..), Scheme (..), client, HasClient, Client,clientWithRoute,AuthClientData,mkAuthenticateReq,AuthenticateReq)
import           Servant.Common.Req         (ServantError, addHeader,Req)
import qualified Servant.Common.Req         as SCR
import Network.HTTP.Client (Manager)

type instance AuthClientData (AuthProtect "tag") = HabiticaApiKey

authenticateReq :: HabiticaApiKey -> Req -> Req
authenticateReq k req = SCR.addHeader "x-api-key" (authApiKey k) $ SCR.addHeader "x-api-user" (authUser k) req

type HabiticaAPI =  (
       AuthProtect "tag" :> "user" :>  Get '[JSON] (HabiticaResponse Value)
  :<|> AuthProtect "tag" :>"groups" :> "party" :> "chat" :>  Get '[JSON] (HabiticaResponse [Chat])
  :<|> "tasks" :> (
           "user" :> AuthProtect "tag" :> Get '[JSON] (HabiticaResponse [Task] )
      :<|> Capture "taskId" String :> AuthProtect "tag" :> Get '[JSON] (HabiticaResponse Task )
      :<|> ReqBody '[JSON] Task :> AuthProtect "tag" :> Post '[JSON] (HabiticaResponse Task )
      :<|> Capture "taskId" String :> ReqBody '[JSON] Task :> AuthProtect "tag" :> Put '[JSON] (HabiticaResponse Task)
      :<|> Capture "taskId" String :> AuthProtect "tag" :> Delete '[JSON] (HabiticaResponse NoData)
      )
   )

type Auth = AuthenticateReq (AuthProtect "tag")
type Habitica a = Manager -> BaseUrl -> SCR.ClientM (HabiticaResponse a)
type HabiticaAuth a = Auth -> Habitica a
data HabiticaContext = HabiticaContext {
    contextKey :: HabiticaApiKey,
    contextManager :: Manager
}

data NoData = NoData deriving Show
instance FromJSON NoData where
  parseJSON = withObject "NoData" $ \o ->
                 if HM.null o then pure NoData else fail "Expected empty object"

targetUrl :: BaseUrl
targetUrl = BaseUrl Https "habitica.com" 443 "/api/v3"

getTasks' :: HabiticaAuth [Task]
getTask' :: String -> HabiticaAuth Task
postTask' :: Task -> HabiticaAuth Task
updateTask' :: String -> Task -> HabiticaAuth Task
deleteTask' :: String -> HabiticaAuth NoData
getUser' :: HabiticaAuth Value
getPartyChat' :: HabiticaAuth [Chat]

getUser'
   :<|> getPartyChat'
   :<|>(getTasks'
   :<|> getTask'
   :<|> postTask'
   :<|> updateTask'
   :<|> deleteTask')
  = client (Proxy :: Proxy HabiticaAPI) --targetUrl

mkHMonad :: (Auth -> Manager -> BaseUrl -> SCR.ClientM (HabiticaResponse a)) -> HMonad a
mkHMonad x = do
  ctxt <- ask
  let auth = mkAuthenticateReq (contextKey ctxt) authenticateReq
  lift $ mapExceptT (f <$>) $ x auth (contextManager ctxt) targetUrl
  where 
    f (Right (Success a)) = Right a
    f (Right (Error s)) = Left s
    f (Left e) = Left (show e)


getTasks :: HMonad [Task]
getTasks = mkHMonad getTasks'
getTask :: String -> HMonad Task
getTask s = mkHMonad (getTask' s)
postTask :: Task -> HMonad Task
postTask = mkHMonad . postTask'
updateTask :: String -> Task -> HMonad Task
updateTask = (mkHMonad . ) . updateTask'
deleteTask :: String -> HMonad NoData
deleteTask = mkHMonad . deleteTask'
getUser :: HMonad Value
getUser = mkHMonad getUser'
getPartyChat :: HMonad [Chat]
getPartyChat = mkHMonad getPartyChat'

runClientM :: (Auth -> SCR.ClientM a) -> HabiticaApiKey -> IO (Either String a)
runClientM x key = left show <$> runExceptT (x (mkAuthenticateReq key authenticateReq))

type HMonad a = ReaderT HabiticaContext (ExceptT String IO) a
runHMonad :: HMonad a -> HabiticaContext -> IO (Either String a)
runHMonad ctxt a = runExceptT $ runReaderT ctxt a

---------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------
todo :: String -> Task
todo x = TaskTodo $ Todo (BaseTask "" Nothing x "" [] 0 0 "" ())
          False Nothing Nothing (Sublist [] True)

getTodos :: HMonad [Todo]
getTodos = filter (\x -> not $ x^.todoCompleted) . mapMaybe fromTask <$> getTasks 

findTasks :: String -> HMonad [Task]
findTasks s = filter (\x -> s `isInfixOf` (toBase x ^. text)) <$> getTasks
