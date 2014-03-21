module Zendesk
where

newtype Name = Name String
newtype Email = Email String

createUser :: Name -> Email -> IO ()
createUser = undefined
