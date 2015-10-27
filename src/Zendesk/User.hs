{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Zendesk.User
( createUser
, getUsers
, User(..)
) where

import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (MonadLogger(..))
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Data.Aeson as J (encode, object, (.=), Value(..), Object)
import qualified Data.ByteString.Lazy as LBS (toStrict)
import Data.CaseInsensitive (mk)
import Data.Conduit (Source)
import Data.Text as T (pack, Text)
import Data.Time.Clock (UTCTime(..))

import Json (deriveJSON, deriveJSON_, deriveEnumJSON)
import Network.HTTP.Conduit as HTTP ( parseUrl
                                    , RequestBody(..), requestBody
                                    , method, requestHeaders)

import Zendesk.Common

data User = User
  { userId              :: Maybe Int
  , userUrl             :: Maybe Text
  , userName            :: Text
  , userCreatedAt       :: Maybe UTCTime
  , userUpdatedAt       :: Maybe UTCTime
  , userTimeZone        :: Maybe Text
  , userEmail           :: Text
  , userPhone           :: Maybe Text
  , userLocale          :: Maybe Text
  , userLocaleId        :: Maybe Int
  , userOrganizationId  :: Maybe Int
  , userRole            :: Text
  , userVerified        :: Maybe Bool
  , userPhoto           :: Maybe Attachment
  } deriving (Show)

data Attachment = Attachment
  { attachmentId          :: Int
  , attachmentFileName    :: Text
  , attachmentContentUrl  :: Text
  , attachmentContentType :: Text
  , attachmentSize        :: Int
  , attachmentThumbnails  :: Maybe [Attachment]
  } deriving (Show)

data UserReply = UserReply
  { userReplyUser :: User
  }

newtype Name = Name String

newtype Email = Email String

createUser :: (MonadIO m, MonadLogger m) => Name -> Email -> ZendeskT m User
createUser (Name name) (Email email) = do
  initRequest <- parseUrl =<< getUsersUrl
  let request = initRequest
                { method = "POST"
                , requestHeaders = [ (mk "Content-Type", "application/json") ]
                , requestBody = RequestBodyBS $ LBS.toStrict $ encode
                                  $ object ["user" .= CreateUserRequest name email]
                }
  userReplyUser `liftM` (runRequest request)

getUsers :: (MonadIO m, MonadLogger m) => Source (ZendeskT m) User
getUsers =
  getCollection =<< (Just `liftM` (T.pack `liftM` (lift getUsersUrl)))

getUsersUrl :: Monad m => ZendeskT m String
getUsersUrl = do
  baseUrl <- asks zendeskUrl
  return $ baseUrl ++ "/api/v2/users.json"

data CreateUserRequest = CreateUserRequest
  { createUserRequestName  :: String
  , createUserRequestEmail :: String
  } deriving (Show)

instance CollectionKey User where
  collectionKey _ = "users"

deriveJSON ''CreateUserRequest
deriveJSON ''User
deriveJSON ''UserReply
deriveJSON ''Attachment
