{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Zendesk
( runZendesk
, createUser
, getUsers
, getTickets
, getTicketFields
, ZendeskConfig(..)
, runZendeskT
) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (catches, SomeException(..), Handler(..))
import Control.Failure (Failure(..))
import Control.Monad (liftM, forM)
import Control.Monad.Reader (ReaderT (..), asks)
import Control.Monad.Error (ErrorT (..), MonadError(..), Error(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (MonadLogger(..), logDebug)
import Control.Monad.Trans (lift)
import Data.Aeson as J (eitherDecode, encode, FromJSON(..), withObject, withText, (.:), object, (.=), Value(..), Object)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS (toStrict)
import Data.ByteString.Char8 as BS8 (pack, unpack)
import Data.CaseInsensitive (mk)
import Data.Default (Default(..))
import qualified Data.HashMap.Strict as M
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Data.PEM (PEM(..), pemParseBS)
import Data.Text as T (pack, Text, unpack)
import Data.Text.Encoding as T (encodeUtf8)
import Data.Time.Clock (UTCTime(..))
import Data.Traversable (Traversable(..))
import Data.X509 (HashALG(..), decodeSignedCertificate)
import Data.X509.CertificateStore (CertificateStore, makeCertificateStore)
import Data.X509.Validation (ValidationChecks(..), validate, defaultHooks, defaultChecks)
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client (getUri, applyBasicAuth)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Conduit as HTTP ( httpLbs, parseUrl, withManagerSettings
                                    , HttpException(..)
                                    , Request(..), Response(..)
                                    , responseBody, RequestBody(..), requestBody
                                    , method, requestHeaders, checkStatus)
import Network.HTTP.Types.Status (Status(..))
import Network.TLS (Credential, Credentials(..), ClientParams(..), Shared(..), ClientHooks(..), Supported(..), defaultParamsClient, credentialLoadX509FromMemory)
import Network.TLS.Extra.Cipher (ciphersuite_strong)

import Json (deriveJSON, deriveJSON_, deriveEnumJSON)

import Data.Conduit (Source, yield)

import Zendesk.Common
import Zendesk.User

getTicketsUrl :: Monad m => ZendeskT m String
getTicketsUrl = do
  baseUrl <- asks zendeskUrl
  return $ baseUrl ++ "/api/v2/tickets.json"

getTicketFieldsUrl :: Monad m => ZendeskT m String
getTicketFieldsUrl = do
  baseUrl <- asks zendeskUrl
  return $ baseUrl ++ "/api/v2/ticket_fields.json"


data TicketField = TicketField
  { ticketFieldId                  :: Maybe Int
  , ticketFieldUrl                 :: Maybe Text
  , ticketFieldType                :: Text
  , ticketFieldTitle               :: Text
  , ticketFieldDescription         :: Maybe Text
  , ticketFieldPosition            :: Maybe Int
  , ticketFieldActive              :: Maybe Bool
  , ticketFieldRequired            :: Maybe Bool
  , ticketFieldCollapsedForAgents  :: Maybe Bool
  , ticketFieldRegexpForValidation :: Maybe Text
  , ticketFieldTitleInPortal       :: Maybe Text
  , ticketFieldVisibleInPortal     :: Maybe Bool
  , ticketFieldEditableInPortal    :: Maybe Bool
  , ticketFieldRequiredInPortal    :: Maybe Bool
  , ticketFieldTag                 :: Maybe Text
  , ticketFieldCreatedAt           :: Maybe UTCTime
  , ticketFieldUpdatedAt           :: Maybe UTCTime
  --, ticketSystemFieldOptions       :: Maybe [Text] TODO Array of something
  --, ticketCustomFieldOptions       :: Maybe [Text] TODO Array of something
  , ticketFieldRemovable           :: Maybe Bool
  } deriving (Show)

data TicketFieldValue = TicketFieldValue
  { ticketFieldValueId    :: Int
  , ticketFieldValueValue :: Maybe Text
  } deriving (Show)

data Ticket = Ticket
  { ticketId :: Maybe Int
  , ticketUrl :: Maybe Text
  , ticketExternalId :: Maybe Text
  , ticketType :: Maybe Text
  , ticketSubject :: Maybe Text
  , ticketDescription :: Maybe Text
  , ticketPriority :: Maybe Text
  , ticketStatus :: Maybe Text
  , ticketRecipient :: Maybe Text
  , ticketRequesterId :: Int
  , ticketSubmitterId :: Maybe Int
  , ticketAssigneeId :: Maybe Int
  , ticketOrganizationId :: Maybe Int
  , ticketGroupId :: Maybe Int
  , ticketCollaboratorIds :: Maybe [Int]
  , ticketForumTopicId :: Maybe Int
  , ticketProblemId :: Maybe Int
  , ticketHasIncidents :: Maybe Bool
  , ticketDueAt :: Maybe Text
  , ticketTags :: Maybe [Text]
  , ticketVia :: Maybe Via
  , ticketCustomFields :: Maybe [TicketFieldValue]
  , ticketSatisfactionRaiting :: Maybe Object
  , ticketSharingAgreementIds :: Maybe [Int]
  , ticketFollowupIds :: Maybe [Int]
  , ticketTicketFormId :: Maybe Int
  , ticketBrandId :: Maybe Int
  , ticketCreatedAt :: Maybe UTCTime
  , ticketUpdatedAt :: Maybe UTCTime
  } deriving (Show)

data Via = Via
  { viaChannel :: Text
  , viaSource  :: Maybe Object
  } deriving (Show)

instance CollectionKey Ticket where
  collectionKey _ = "tickets"

instance CollectionKey TicketField where
  collectionKey _ = "ticket_fields"

deriveJSON ''Ticket
deriveJSON ''TicketField
deriveJSON ''TicketFieldValue
deriveJSON ''Via

getTickets :: (MonadIO m, MonadLogger m) => Source (ZendeskT m) Ticket
getTickets =
  getCollection =<< (Just `liftM` (T.pack `liftM` (lift getTicketsUrl)))

getTicketFields :: (MonadIO m, MonadLogger m) => Source (ZendeskT m) TicketField
getTicketFields =
  getCollection =<< (Just `liftM` (T.pack `liftM` (lift getTicketFieldsUrl)))
