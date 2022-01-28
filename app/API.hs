{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module API where
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , encode
                                                )
import           GHC.Generics                   ( Generic )
import           Network.Wai.Middleware.Cors
import           Web.Scotty

import           BetterInterpreter
import           Control.Monad.State.Strict     ( MonadIO(liftIO)
                                                , join
                                                )
import System.Environment
import Control.Monad

newtype Res = Res (Either [Char] [Int])
    deriving (Eq,Show, Generic)

instance FromJSON Res
instance ToJSON Res


corsHelper = cors
    (const $ Just
        (simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] })
    )


serverhelper :: ScottyM ()
serverhelper = post "/runPreks" $ do
    content <- jsonData :: ActionM String
    json $ parseInterpret content



--port <- liftM read $ getEnv "PORT"
-- server :: IO ()
-- server = scotty 3000 $ do
--     middleware corsHelper
--     serverhelper

server :: IO ()
server = do port <- read <$> getEnv "PORT"
            scotty port $ do
                middleware corsHelper
                serverhelper
