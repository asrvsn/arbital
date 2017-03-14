{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Arbital.Endpoints.Authenticated
  ( AuthStrategy(..)
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
import           Arbital.State
import           Arbital.Constants 
import qualified Arbital.Database.User as UserDB

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

login :: AuthStrategy -> App Session
login = \case
  GoogleTokenAuth token -> do
    let url = google_token_verify_url ++ T.unpack token
    resp <- liftIO $ get url
    case resp ^? responseBody of 
      Just body -> case eitherDecode body of 
        Right authResp -> do
          if authClientId authResp == T.pack google_app_client_id
            then getGapiUser authResp >>= startSession
            else gapiError "app client id's did not match"
        Left err -> gapiError $ "error decoding response: " ++ err 
      Nothing -> gapiError "received no response body"
        
authHandler :: AppState -> AuthHandler Request Session
authHandler r = mkAuthHandler $ \req -> 
  case lookup "servant-session-id" (requestHeaders req) of
    Nothing -> throwError (err401 { errBody = "Missing auth header" })
    Just s -> do
      mse <- runAppT r $ 
        useSession $ SessionID (decodeUtf8 s)
      case mse of
        Nothing -> throwError (err401 { errBody = "Session not found" })
        Just se -> return se

-- | The context that will be made available to request handlers. We supply the
-- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
authContext :: AppState -> Context (AuthHandler Request Session ': '[])
authContext r = authHandler r :. EmptyContext
  
-- * Helpers

gapiError :: String -> App a
gapiError err = 
  throwError $ err401 { errBody = BLC.pack $ "gapi-signin: " ++ err}

getGapiUser :: GoogleAuthResponse -> App User
getGapiUser ar = do
  mUser <- withConnection $ \c -> UserDB.get c (authEmail ar)
  case mUser of 
    Just user -> return user
    Nothing -> withConnection $ \c ->
      UserDB.create c (authEmail ar) (authName ar)
