{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Json
  ( jsonOptionsForPrefix
  , deriveJSON
  , deriveJSON_
  , deriveJSONOptions
  , deriveEnumJSON
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import qualified Data.Aeson.TH as J (Options (..), defaultOptions, deriveJSON)
import Data.Char (toLower, isUpper)
import Data.Text as T (unpack)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (showName)

jsonOptionsForPrefix :: String -> J.Options
jsonOptionsForPrefix prefix =
  J.defaultOptions
    { J.fieldLabelModifier = underscoreIt . (drop $ length prefix) }

underscoreIt :: String -> String
underscoreIt s = go [] s
  where
    go :: String -> String -> String
    go [] (c:cs) = go [toLower c] cs
    go r (c:cs) | isUpper c = go (r ++ "_" ++ [(toLower c)]) cs
    go r (c:cs) = go (r ++ [c]) cs
    go r [] = r

deriveJSONOptions :: J.Options -> Name -> Q [Dec]
deriveJSONOptions = J.deriveJSON

deriveJSON :: Name -> Q [Dec]
deriveJSON name = deriveJSONOptions (jsonOptionsForPrefix (nameBase name)) name

deriveJSON_ :: Name -> Q [Dec]
deriveJSON_ name = deriveJSONOptions (jsonOptionsForPrefix ('_' : nameBase name)) name

deriveEnumJSON :: Name -> Q [Dec]
deriveEnumJSON name = [d|
  instance FromJSON $(conT name) where
    parseJSON (String s) = return $ read . T.unpack $ s
    parseJSON _ = fail $ "Invalid " ++ $(stringE (showName name))

  instance ToJSON $(conT name) where
    toJSON = toJSON . show
  |]
