import Control.Monad.Logger

import Zendesk

main = do
  us <- runStdoutLoggingT $ runZendeskT
            (ZendeskConfig
                "https://sample.zendesk.com"
                "sampleUser"
                "samplePasssword"
                Nothing)
          $ createUser (Name "testUser") (Email "testEmail@gmail.com")
  putStrLn $ show us
