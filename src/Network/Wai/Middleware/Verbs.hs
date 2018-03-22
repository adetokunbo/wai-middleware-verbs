{-# LANGUAGE
    FlexibleContexts
  , StandaloneDeriving
  , ScopedTypeVariables
  , MultiParamTypeClasses
  , UndecidableInstances
  , GeneralizedNewtypeDeriving
  , DeriveGeneric
  , TypeSynonymInstances
  #-}

{-|
Module      : Network.Wai.Middleware.Verbs
Copyright   : (c) Athan Clark, 2015
License     : BSD-3
Maintainer  : athan.clark@gmail.com
Stability   : experimental
Portability : POSIX

This module provides everything you need to respond depending on an HTTP verbs.
The <#g:1 Combinators> section defines the 'get', 'post' and other functions you would
expect in a toolset like this. The 'VerbListenerT' object is constructed from the
combinators, and turning it directly into a WAI 'Network.Wai.Trans.MiddlewareT'
is easy with 'verbsToMiddleware'.

> myApp :: MonadIO => MiddlewareT m
> myApp = verbsToMiddleware $ do
>   get myMiddleware1
>   put uploader myMiddleware2
>   post uploader myMiddleware3
>   delete myMiddleware4
>   where
>     uploader :: MonadIO m => Request -> m ()
>     uploader req = liftIO $ print =<< getStrictRequestBody req
-}

module Network.Wai.Middleware.Verbs
  ( -- * Combinators
    get
  , post
  , put
  , delete
  , -- * Types
    VerbMap
  , Verb
  , -- ** Monad Transformer
    VerbListenerT (..)
  , execVerbListenerT
  , -- * Utilities
    getVerbFromRequest
  , verbsToMiddleware
  ) where


import           Network.Wai (Request, strictRequestBody, requestMethod)
import           Network.Wai.Trans (MiddlewareT)
import           Network.HTTP.Types (StdMethod (..), Method, methodDelete, methodPut, methodPost, methodGet)

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy                    as HM
import           Data.Monoid ((<>))
import           Data.Hashable (Hashable)
import           Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as LBS
import           Control.Applicative (Alternative)
import           Control.Monad (MonadPlus)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Trans (MonadTrans (lift))
import           Control.Monad.State (MonadState, StateT (..), modify', execStateT)
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.Writer (MonadWriter)
import           Control.Monad.Cont (MonadCont)
import           Control.Monad.Base (MonadBase)
import           Control.Monad.Catch (MonadCatch, MonadThrow, MonadMask)
import           Control.Monad.Trans.Resource (MonadResource)
import           Control.Monad.Except (MonadError)
import           Control.Monad.Logger (MonadLogger)

import           GHC.Generics (Generic)


-- * Types

-- | A map from an HTTP verb, to a result and uploading method.
type VerbMap r = HashMap Verb (Either r (LBS.ByteString -> r))

type Verb = StdMethod

deriving instance Generic Verb

instance Hashable Verb


-- | Fetches the HTTP verb from the WAI 'Network.Wai.Request' - defaults to GET.
getVerbFromRequest :: Request -> Verb
getVerbFromRequest req = fromMaybe GET $ httpMethodToMSym (requestMethod req)
  where
    httpMethodToMSym :: Method -> Maybe Verb
    httpMethodToMSym x | x == methodGet    = Just GET
                       | x == methodPost   = Just POST
                       | x == methodPut    = Just PUT
                       | x == methodDelete = Just DELETE
                       | otherwise         = Nothing

{-# INLINEABLE getVerbFromRequest #-}


-- * Verb Writer

-- | This is the monad for our DSL - where @r@ is the result type. We leave this
--   polymorphic for 'Web.Routes.Nested.ActionT' for <https://hackage.haskell.org/package/nested-routes nested-routes>.
--
--   > myListener :: MonadIO m => VerbListenerT (MiddlewareT m) m ()
--   > myListener = do
--   >   get myMiddleware1
--   >   post uploader myMiddleware2
--   >   where
--   >     uploader :: MonadIO Request -> m ()
--   >     uploader req =
--   >       liftIO $ print =<< strictRequestBody req
newtype VerbListenerT r m a = VerbListenerT
  { runVerbListenerT :: StateT (VerbMap r) m a
  } deriving ( Functor, Applicative, Alternative, Monad, MonadFix, MonadPlus
             , MonadState (VerbMap r), MonadWriter w, MonadReader r, MonadIO
             , MonadError e', MonadCont, MonadBase b, MonadThrow, MonadCatch
             , MonadMask, MonadLogger
             ) -- TODO: MonadControl

deriving instance (MonadResource m, MonadBase IO m) => MonadResource (VerbListenerT r m)

execVerbListenerT :: (Monad m) => VerbListenerT r m a -> m (VerbMap r)
execVerbListenerT xs = execStateT (runVerbListenerT xs) mempty

{-# INLINEABLE execVerbListenerT #-}


instance MonadTrans (VerbListenerT r) where
  lift = VerbListenerT . lift -- uses StateT


-- * Combinators

-- | For simple @GET@ responses
get :: ( Monad m
       , Monoid r
       ) => r -> VerbListenerT r m ()
get r = tell' (HM.singleton GET (Left r))

{-# INLINEABLE get #-}

-- | For simple @POST@ responses
post :: ( Monad m
        , Monoid r
        ) => (LBS.ByteString -> r) -> VerbListenerT r m ()
post r = tell' (HM.singleton POST (Right r))

{-# INLINEABLE post #-}

-- | For simple @PUT@ responses
put :: ( Monad m
       , Monoid r
       ) => (LBS.ByteString -> r) -> VerbListenerT r m ()
put r = tell' (HM.singleton PUT (Right r))

{-# INLINEABLE put #-}

-- | For simple @DELETE@ responses
delete :: ( Monad m
          , Monoid r
          ) => r -> VerbListenerT r m ()
delete r = tell' (HM.singleton DELETE (Left r))

{-# INLINEABLE delete #-}

tell' :: (Monoid r, MonadState (VerbMap r) m) => VerbMap r -> m ()
tell' z = modify' (\y -> HM.unionWith go y z)
  where
    go (Left x) (Left y) = Left (x <> y)
    go (Right f) (Right g) = Right (\a -> f a <> g a)
    go (Left x) (Right g) = Right (\a -> x <> g a)
    go (Right f) (Left y) = Right (\a -> f a <> y)

{-# INLINEABLE tell' #-}


verbsToMiddleware :: MonadIO m
                  => VerbListenerT (MiddlewareT m) m ()
                  -> MiddlewareT m
verbsToMiddleware vs app req resp = do
  m <- execVerbListenerT vs
  let v = getVerbFromRequest req
  case HM.lookup v m of
    Nothing -> app req resp
    Just eR -> case eR of
      Left x -> x app req resp
      Right f -> do
        body <- liftIO (strictRequestBody req)
        f body app req resp
