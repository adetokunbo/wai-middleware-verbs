{-# LANGUAGE
    TupleSections
  , FlexibleContexts
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
  , getVerb
  , -- ** Monad Transformer
    VerbListenerT (..)
  , execVerbListenerT
  , verbsToMiddleware
  , -- * Utilities
    lookupVerb
  , mapVerbs
  ) where


import           Network.Wai.Trans
import           Network.HTTP.Types

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy                    as HM
import           Data.Monoid
import           Data.Hashable
import           Control.Arrow (second)
import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.State hiding (get, put)
import qualified Control.Monad.State                  as S
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.Cont
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Trans.Resource
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Error

import           GHC.Generics


-- * Types

-- | A map from an HTTP verb, to a result and uploading method.
type VerbMap m r = HashMap Verb (Request -> m (), r)

type Verb = StdMethod

deriving instance Generic Verb

instance Hashable Verb


-- | Fetches the HTTP verb from the WAI @Request@ - defaults to GET.
getVerb :: Request -> Verb
getVerb req = fromMaybe GET $ httpMethodToMSym (requestMethod req)
  where
    httpMethodToMSym :: Method -> Maybe Verb
    httpMethodToMSym x | x == methodGet    = Just GET
                       | x == methodPost   = Just POST
                       | x == methodPut    = Just PUT
                       | x == methodDelete = Just DELETE
                       | otherwise         = Nothing

{-# INLINEABLE getVerb #-}

-- | Take the monadic partial result of @lookupVerb@, and actually h the upload.
lookupVerb :: Monad m => Request -> Verb -> VerbMap m r -> m (Maybe r)
lookupVerb req v vmap = runMaybeT $ do
  (upload, result) <- hoistMaybe $ HM.lookup v vmap
  lift (upload req)
  return result

{-# INLINEABLE lookupVerb #-}

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
  { runVerbListenerT :: StateT (VerbMap m r) m a
  } deriving ( Functor, Applicative, Alternative, Monad, MonadFix, MonadPlus
             , MonadState (VerbMap m r), MonadWriter w, MonadReader r, MonadIO
             , MonadError e', MonadCont, MonadBase b, MonadThrow, MonadCatch
             , MonadMask, MonadLogger
             )

deriving instance (MonadResource m, MonadBase IO m) => MonadResource (VerbListenerT r m)

execVerbListenerT :: Monad m => VerbListenerT r m a -> m (VerbMap m r)
execVerbListenerT xs = execStateT (runVerbListenerT xs) mempty

{-# INLINEABLE execVerbListenerT #-}

instance MonadTrans (VerbListenerT r) where
  lift = VerbListenerT . lift -- uses StateT


-- | Turn a map from HTTP verbs to middleware, into a middleware.
verbsToMiddleware :: MonadIO m =>
                     VerbListenerT (MiddlewareT m) m ()
                  -> MiddlewareT m
verbsToMiddleware vl app req respond = do
  let v = getVerb req
  vmap <- execVerbListenerT vl
  mMiddleware <- lookupVerb req v vmap
  fromMaybe (app req respond) $
    (\mid -> mid app req respond) <$> mMiddleware

{-# INLINEABLE verbsToMiddleware #-}

-- * Combinators

-- | For simple @GET@ responses
get :: ( Monad m
       ) => r -> VerbListenerT r m ()
get r = tell' $! HM.singleton GET ( const $ return ()
                                  , r
                                  )

{-# INLINEABLE get #-}

-- | For simple @POST@ responses
post :: ( Monad m
        ) => (Request -> m ()) -- Handle upload
          -> r
          -> VerbListenerT r m ()
post h r = tell' $! HM.singleton POST ( h
                                      , r
                                      )

{-# INLINEABLE post #-}

-- | For simple @PUT@ responses
put :: ( Monad m
       ) => (Request -> m ()) -- Handle upload
         -> r
         -> VerbListenerT r m ()
put h r = tell' $! HM.singleton PUT ( h
                                    , r
                                    )

{-# INLINEABLE put #-}

-- | For simple @DELETE@ responses
delete :: ( Monad m
          ) => r -> VerbListenerT r m ()
delete r = tell' $! HM.singleton DELETE ( const $ return ()
                                        , r
                                        )

{-# INLINEABLE delete #-}

tell' :: (Monoid w, MonadState w m) => w -> m ()
tell' x = modify' (<> x)

{-# INLINEABLE tell' #-}

mapVerbs :: Monad m => (r -> s) ->  VerbListenerT r m () -> VerbListenerT s m ()
mapVerbs f xs = do
  vmap <- lift $ execVerbListenerT xs
  tell' $ second f <$> vmap

{-# INLINEABLE mapVerbs #-}
