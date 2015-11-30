{-# LANGUAGE
    OverloadedStrings
  , DeriveDataTypeable
  #-}

module Main where

import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Verbs
import Network.Wai.Middleware.ContentType (textOnly)
import Network.Wai.Trans
import qualified Data.Text.Lazy as LT
import Data.Monoid
import Data.Typeable
import Control.Monad.IO.Class
import Control.Monad.Catch


theEmbeddedApp :: MonadIO m => LT.Text -> MiddlewareT m
theEmbeddedApp v _ _ respond =
  respond $ textOnly $ v <> " Verb accepted!"


data UploadError = UploadFailed
  deriving (Show, Typeable)

instance Exception UploadError

myListener :: ( MonadIO m
              , MonadThrow m
              ) => VerbListenerT (MiddlewareT m) m ()
myListener = do
  get                $ theEmbeddedApp "GET"
  put  uploadFail    $ theEmbeddedApp "PUT" -- should never work
  post uploadSucceed $ theEmbeddedApp "POST"
  delete             $ theEmbeddedApp "DELETE"
  where
    uploadFail    _ = throwM UploadFailed
    uploadSucceed _ = return ()

handleError :: UploadError -> MiddlewareT m
handleError UploadFailed _ _ respond =
  respond $ textOnly "Upload Failed!"


myApp :: ( MonadIO m
         , MonadCatch m
         ) => MiddlewareT m
myApp app req respond =
          (verbsToMiddleware myListener app req respond)
  `catch` (\e -> handleError e app req respond)


main :: IO ()
main = run 3000 (myApp defApp)
  where
    defApp :: Application
    defApp _ respond =
      respond $ textOnly "404"


