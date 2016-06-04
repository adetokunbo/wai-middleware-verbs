{-# LANGUAGE
    OverloadedStrings
  , DeriveDataTypeable
  #-}

module Main where

import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Verbs
import Network.Wai.Middleware.ContentType (textOnly)
import Network.Wai.Trans
import Network.HTTP.Types
import qualified Data.Text.Lazy as LT
import Data.Monoid
import Data.Typeable
import Control.Monad.IO.Class
import Control.Monad.Catch


theEmbeddedApp :: ( MonadIO m
                  ) => LT.Text -> ApplicationT m
theEmbeddedApp v _ respond =
  respond $ textOnly (v <> " Verb accepted!") status200 []


data UploadError = UploadFailed
  deriving (Show, Typeable)

instance Exception UploadError

myListener :: ( MonadIO m
              , MonadThrow m
              ) => VerbListenerT (ApplicationT m) m ()
myListener = do
  get    $ theEmbeddedApp "GET"
  put    $ readingRequest uploadFail
         $ theEmbeddedApp "PUT" -- should never work
  post   $ readingRequest uploadSucceed
         $ theEmbeddedApp "POST"
  delete $ theEmbeddedApp "DELETE"
  where
    uploadFail    _ = throwM UploadFailed
    uploadSucceed _ = return ()

handleError :: ( MonadIO m
               ) => UploadError -> ApplicationT m
handleError UploadFailed _ respond =
  respond $ textOnly "Upload Failed!" status500 []


myApp :: ( MonadIO m
         , MonadCatch m
         ) => MiddlewareT m
myApp app req respond =
  findResponse `catch` (\e -> handleError e req respond)
  where
    findResponse = do
      xs <- execVerbListenerT myListener
      case lookupVerb (getVerb req) xs of
        Nothing -> app req respond
        Just r  -> r req respond


main :: IO ()
main = run 3000 (myApp defApp)
  where
    defApp :: Application
    defApp _ respond = respond $ textOnly "404" status404 []


