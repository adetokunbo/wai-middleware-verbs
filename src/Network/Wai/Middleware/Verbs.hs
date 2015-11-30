{-# LANGUAGE
    TupleSections
  , FlexibleContexts
  , StandaloneDeriving
  , ScopedTypeVariables
  , MultiParamTypeClasses
  , UndecidableInstances
  , GeneralizedNewtypeDeriving
  #-}

{-|
Module      : Network.Wai.Middleware.Verbs
Copyright   : (c) Athan Clark, 2015
License     : BSD-3
Maintainer  : athan.clark@gmail.com
Stability   : experimental
Portability : POSIX

This module provides everything you need to route based off different HTTP verbs.
The <#g:1 Combinators> section defines the @get@, @post@ and other functions you would
expect in a toolset like this. Likewise, we also include tools for manually
looking into the <#t:VerbListenerT VerbListenerT> object constructed from the combinators, and
turning it directly into a WAI @MiddlewareT@.
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

import           Data.Map (Map)
import qualified Data.Map                             as Map
import           Data.Monoid
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


-- * Types

-- | A map from an HTTP verb, to a result and uploading method.
type VerbMap m r = Map Verb (Request -> m (), r)

type Verb = StdMethod

-- | Fetches the HTTP verb from the WAI @Request@ - defaults to GET.
getVerb :: Request -> Verb
getVerb req = fromMaybe GET $ httpMethodToMSym $ requestMethod req
  where
    httpMethodToMSym :: Method -> Maybe Verb
    httpMethodToMSym x | x == methodGet    = Just GET
                       | x == methodPost   = Just POST
                       | x == methodPut    = Just PUT
                       | x == methodDelete = Just DELETE
                       | otherwise         = Nothing

-- | Take the monadic partial result of @lookupVerb@, and actually h the upload.
lookupVerb :: Monad m => Request -> Verb -> VerbMap m r -> m (Maybe r)
lookupVerb req v vmap = runMaybeT $ do
  (upload, result) <- hoistMaybe $ Map.lookup v vmap
  lift (upload req)
  return result


-- * Verb Writer

-- | The type variables are @r@ for the result, @e@ for the error type throwable
-- during uploading, @u@ is the sucessful upload type, and @m@ and @a@ form the
-- last two parts of the monad transformer.
newtype VerbListenerT r m a =
  VerbListenerT { runVerbListenerT :: StateT (VerbMap m r) m a }
    deriving ( Functor, Applicative, Alternative, Monad, MonadFix, MonadPlus
             , MonadState (VerbMap m r), MonadWriter w, MonadReader r, MonadIO
             , MonadError e', MonadCont, MonadBase b, MonadThrow, MonadCatch
             , MonadMask, MonadLogger
             )

deriving instance (MonadResource m, MonadBase IO m) => MonadResource (VerbListenerT r m)

execVerbListenerT :: Monad m => VerbListenerT r m a -> m (VerbMap m r)
execVerbListenerT xs = execStateT (runVerbListenerT xs) mempty


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


-- * Combinators

-- | For simple @GET@ responses
get :: ( Monad m
       ) => r -> VerbListenerT r m ()
get r = tell' $ Map.singleton GET ( const $ return ()
                                  , r
                                  )

-- | For simple @POST@ responses
post :: ( Monad m
        ) => (Request -> m ()) -- Handle upload
          -> r
          -> VerbListenerT r m ()
post h r = tell' $ Map.singleton POST ( h
                                      , r
                                      )

-- | For simple @PUT@ responses
put :: ( Monad m
       ) => (Request -> m ()) -- Handle upload
         -> r
         -> VerbListenerT r m ()
put h r = tell' $ Map.singleton PUT ( h
                                    , r
                                    )

-- | For simple @DELETE@ responses
delete :: ( Monad m
          ) => r -> VerbListenerT r m ()
delete r = tell' $ Map.singleton DELETE ( const $ return ()
                                        , r
                                        )


tell' :: (Monoid w, MonadState w m) => w -> m ()
tell' x = do
  xs <- S.get
  S.put $ xs <> x

mapVerbs :: Monad m => (r -> s) ->  VerbListenerT r m () -> VerbListenerT s m ()
mapVerbs f xs = do
  vmap <- lift $ execVerbListenerT xs
  tell' $ second f <$> vmap
