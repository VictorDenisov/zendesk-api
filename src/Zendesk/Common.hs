{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Zendesk.Common
where

import Control.Applicative ((<$>), (<*>))

import Control.Exception (catches, SomeException(..), Handler(..))

import Control.Failure (Failure(..))

import Control.Monad (liftM, forM)

import Control.Monad.Error (ErrorT(..), MonadError(..), Error(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (MonadLogger(..), logDebug)
import Control.Monad.Reader (ReaderT (..), asks)
import Control.Monad.Trans (lift)

import Data.Aeson as J (eitherDecode, FromJSON(..), (.:), Value(..), Object)

import Data.ByteString.Char8 as BS8 (pack, unpack)
import Data.ByteString.Lazy (ByteString)

import Data.Conduit (Source, yield)

import Data.Default (Default(..))

import Data.Text as T (pack, Text, unpack)

import Data.X509 (HashALG(..))
import Data.X509.CertificateStore (CertificateStore)
import Data.X509.Validation (ValidationChecks(..), validate, defaultHooks, defaultChecks)

import Json (deriveJSON)

import Network.Connection (TLSSettings(..))

import Network.HTTP.Client (getUri, applyBasicAuth)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Conduit as HTTP ( httpLbs, parseUrl, withManagerSettings
                                    , HttpException(..)
                                    , Request(..), Response(..)
                                    , responseBody, requestBody
                                    , requestHeaders, checkStatus)
import Network.HTTP.Types.Status (Status(..))

import Network.TLS ( Credential, ClientHooks(..), ClientParams(..)
                   , Credentials(..), Supported(..), defaultParamsClient
                   , Shared(..))
import Network.TLS.Extra.Cipher (ciphersuite_strong)

data ZendeskError = TimeoutError
                  | NoResourceError
                  | BadRequestError String
                  | NoFreeServersError String
                  | InternalError String
                  | UnknownError String

instance Error ZendeskError where
  strMsg = UnknownError

instance Show ZendeskError where
  show NoResourceError   = "Resource not found in datacenter"
  show TimeoutError      = "Datacenter unavailable"
  show (BadRequestError s)      = "Bad request: " ++ s
  show (NoFreeServersError s) = "Not enough free servers (" ++ s ++ ")"
  show (InternalError s) = "Internal error: " ++ s
  show (UnknownError s)  = s

runZendeskT :: ZendeskConfig -> ZendeskT m a -> m (Either ZendeskError a)
runZendeskT c f = runErrorT $ runReaderT f c

type ZendeskT m = ReaderT ZendeskConfig (ErrorT ZendeskError m)

data ZendeskConfig = ZendeskConfig
  { zendeskUrl :: String -- zendesk domain url
  , zendeskUsername :: String
  , zendeskPassword :: String
  , zendeskTLS :: Maybe ZendeskTLSConfig
  }

data ZendeskTLSConfig = ZendeskTLSConfig
  { tlsCredential :: Credential
  , tlsCertificateStore :: CertificateStore
  }

class CollectionKey e where
  collectionKey :: e -> Text

data Collection e = Collection
  { collectionElements :: [e]
  , collectionCount    :: Int
  , collectionNextPage :: Maybe Text
  , collectionPrevPage :: Maybe Text
  } deriving Show

instance (CollectionKey e, FromJSON e) => FromJSON (Collection e) where
  parseJSON (Object v) =
    Collection
          <$> v .: (collectionKey (undefined :: e))
          <*> v .: "count"
          <*> v .: "next_page"
          <*> v .: "previous_page"

showRequest :: Request -> String
showRequest x = unlines
  [ "Request {"
  , "  method = " ++ show (method x)
  , "  url = \"" ++ show (getUri x) ++ "\""
  , "  headers = " ++ show (requestHeaders x)
  -- , "  body = " ++ show (requestBody x)
  , "}"
  ]

showResponse :: Response ByteString -> String
showResponse x = unlines
  [ "Response {"
  , "  status = " ++ show (statusCode $ responseStatus x)
  , "  headers = " ++ show (responseHeaders x)
  , "  body = " ++ show (responseBody x)
  , "}"
  ]

errorResponseText :: HTTP.Response ByteString -> String
errorResponseText response =
  either (\_ -> show $ responseBody response) errorReportError $ eitherDecode . responseBody $ response

handleExceptionsAndResult :: MonadIO m => IO a -> ZendeskT m a
handleExceptionsAndResult monad = do
  res <- liftIO $ (fmap Right monad) `catches` [ Handler handleHttpException
                                               , Handler handleOtherExceptions
                                               ]
  case res of
    Left v -> throwError v
    Right x -> return x

decodeResponseBody :: (Monad m, ParseResponseBody a)
                   => Response ByteString -> m (Either ZendeskError a)
decodeResponseBody response
  | statusCode (responseStatus response) `elem` [200..299] =
    case parseBody . responseBody $ response of
      Left e -> return $ Left $ InternalError $ "Json parse error: " ++ e
      Right x -> return $ Right x
  | statusCode (responseStatus response) == 404 = return $ Left NoResourceError
  | statusCode (responseStatus response) == 400 = return $ Left $ BadRequestError $ errorResponseText response
  | statusCode (responseStatus response) == 402 = return $ Left $ NoFreeServersError $ errorResponseText response
  | statusCode (responseStatus response) == 502 = return $ Left $ TimeoutError
  | statusCode (responseStatus response) `elem` [500..599] = return $ Left $ InternalError $ errorResponseText response
  | otherwise = return $ Left $ UnknownError $ errorResponseText response

handleHttpException :: HttpException -> IO (Either ZendeskError a)
handleHttpException ResponseTimeout = return $ Left TimeoutError
handleHttpException e               = return $ Left $ UnknownError $ show e

handleOtherExceptions :: SomeException -> IO (Either ZendeskError a)
handleOtherExceptions e = return $ Left $ UnknownError $ show e

instance Monad m => Failure HttpException (ZendeskT m) where
  failure e = throwError $ InternalError $ show e

class Monad m => MonadZendesk m where
  runZendesk :: ZendeskT m a -> m (Either ZendeskError a)

runRequest :: (MonadIO m, MonadLogger m, ParseResponseBody a)
           => Request -> ZendeskT m a
runRequest request = do
  username <- BS8.pack `liftM` (asks zendeskUsername)
  password <- BS8.pack `liftM` (asks zendeskPassword)
  let request' = applyBasicAuth username password $ request { checkStatus = \_ _ _ -> Nothing }

  $logDebug $ T.pack $ "Sending request to zendesk: " ++ (showRequest request')

  mTLSConfig <- asks zendeskTLS
  let tlsSettings = case mTLSConfig of
                      Nothing -> def
                      Just tlsConfig ->
                        let params = defaultParamsClient (BS8.unpack $ host request') ""
                            validationChecks = defaultChecks
                                                 { checkCAConstraints = False
                                                 , checkLeafV3 = False
                                                 , checkFQHN = False
                                                 }
                         in TLSSettings $ params
                              { clientShared = (clientShared params)
                                                 { sharedCredentials = Credentials [tlsCredential tlsConfig]
                                                 , sharedCAStore = tlsCertificateStore tlsConfig
                                                 }
                              , clientHooks = def { onCertificateRequest = \_ -> return . Just $ tlsCredential tlsConfig
                                                  , onServerCertificate = validate HashSHA256 defaultHooks validationChecks
                                                  }
                              , clientSupported = (clientSupported params)
                                                    { supportedCiphers = ciphersuite_strong }
                              }


  let httpSettings = mkManagerSettings tlsSettings Nothing

  response <- handleExceptionsAndResult $ do
    withManagerSettings httpSettings $ httpLbs request'

  $logDebug $ T.pack $ "Got response from zendesk: " ++ (showResponse response)

  result <- decodeResponseBody response
  case result of
    Left v -> throwError v
    Right x -> return x

runRequestTo :: (MonadIO m, MonadLogger m, ParseResponseBody a)
             => String -> ZendeskT m a
runRequestTo url = parseUrl url >>= runRequest

getCollection :: (CollectionKey e, MonadIO m, MonadLogger m, FromJSON e)
              => Maybe Text -> Source (ZendeskT m) e
getCollection Nothing = return ()
getCollection (Just url) = do
  collection <- lift $ runRequestTo $ T.unpack url
  forM (collectionElements collection) yield
  getCollection $ collectionNextPage collection

data None = None
  deriving (Show, Eq)

class ParseResponseBody a where
  parseBody :: ByteString -> Either String a

instance ParseResponseBody None where
  parseBody _ = Right $ None

instance FromJSON a => ParseResponseBody a where
  parseBody = eitherDecode

data ErrorReport = ErrorReport { errorReportError :: String }
  deriving Show

deriveJSON ''ErrorReport
