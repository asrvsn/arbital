{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Arbital.Endpoints.Authenticated
  ( AuthStrategy(..)
  , Session(..)
  , login
  , authHandler
  , authContext
  ) where

import           GHC.Generics
import           Data.Aeson
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Control.Lens ((^?))
import           Control.Monad.IO.Class (liftIO)
import           Network.Wreq
import           Network.Wai
import           Servant
import           Servant.Server.Experimental.Auth ( AuthHandler
                                                  , AuthServerData
                                                  , mkAuthHandler
                                                  )
import           Servant.Server ( Context ((:.)
                                , EmptyContext)
                                , err401
                                , errBody
                                )
import           Arbital.Types
import           Arbital.Constants 
import qualified Arbital.Database.User as UserDB

newtype SessionID = SessionID Text deriving (Generic)

instance ToJSON SessionID

data Session = Session SessionID User

instance ToJSON Session where
  toJSON (Session s u) = object [ "sessionId" .= s
                                , "sessionUser" .= u
                                ]

-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "cookie-auth") = Session 

data AuthStrategy 
  = GoogleTokenAuth { idToken :: Text }
  deriving (Generic)

instance FromJSON AuthStrategy 

data GoogleAuthResponse = GoogleAuthResponse 
  { authClientId :: Text
  , authEmail :: Email
  , authName :: Text
  }

instance FromJSON GoogleAuthResponse where
  parseJSON = withObject "google auth response" $ \v -> 
    GoogleAuthResponse <$> v .: "aud"
                       <*> v .: "email"
                       <*> v .: "name"

-- * Exposed

login :: AuthStrategy -> Handler Session
login = \case
  GoogleTokenAuth token -> do
    let url = google_token_verify_url ++ T.unpack token
    resp <- liftIO $ get url
    case resp ^? responseBody of 
      Just body -> case eitherDecode body of 
        Right authResp -> do
          if authClientId authResp == T.pack google_app_client_id
            then getGapiUser authResp >>= makeSession
            else gapiError "app client id's did not match"
        Left err -> gapiError $ "error decoding response: " ++ err 
      Nothing -> gapiError "received no response body"
        
authHandler :: AuthHandler Request Session
authHandler = mkAuthHandler $ \req -> 
  case lookup "servant-session-id" (requestHeaders req) of
    Nothing -> throwError (err401 { errBody = "Missing auth header" })
    Just sessionId -> lookupSession (SessionID $ decodeUtf8 sessionId)

-- | The context that will be made available to request handlers. We supply the
-- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
authContext :: Context (AuthHandler Request Session ': '[])
authContext = authHandler :. EmptyContext
  
-- * Helpers

gapiError :: String -> Handler a
gapiError err = 
  throwError $ err401 { errBody = BLC.pack $ "gapi-signin: " ++ err}

getGapiUser :: GoogleAuthResponse -> Handler User
getGapiUser ar = do
  mUser <- UserDB.get (authEmail ar)
  case mUser of 
    Just user -> return user
    Nothing -> UserDB.create (authEmail ar) (authName ar)

makeSession :: User -> Handler Session
makeSession u = return $ Session undefined u 

lookupSession :: SessionID -> Handler Session
lookupSession s = return $ Session s undefined  
