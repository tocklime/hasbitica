module Main where

import Hasbitica.Api
import Control.Monad.Trans.Either
import Servant.Common.Req
import qualified Data.ByteString.Lazy.Char8 as B

apiKey :: String
apiKey = "foo"
apiUser :: String
apiUser = "bar"

addTask :: String -> IO String
addTask t = do
  x <- runEitherT $ postTask (HabiticaApiKey apiUser apiKey) (todo t)
  case x of
    Left err -> return (B.unpack $ responseBody err)
    Right _ -> return "OK"

main :: IO ()
main =  addTask "A HAHAHA" >>= putStrLn
