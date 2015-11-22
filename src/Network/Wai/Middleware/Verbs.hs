{-# LANGUAGE
    TupleSections
  , FlexibleContexts
  , StandaloneDeriving
  , ScopedTypeVariables
  , MultiParamTypeClasses
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
  , getReq
  , post
  , postReq
  , put
  , putReq
  , delete
  , deleteReq
  , -- * Types
    Verbs (..)
  , Verb
  , getVerb
  , ResponseSpec
  , HandleUpload
  , RespondUpload
  , supplyReq
  , -- ** Monad Transformer
    VerbListenerT (..)
  , execVerbListenerT
  , verbsToMiddleware
  , mapVerbs
  , -- * Utilities
    lookupVerb
  , lookupVerbM
  ) where


import           Network.Wai.Trans
import           Network.HTTP.Types

import           Data.Function.Syntax
import           Data.Bifunctor
import           Data.Map (Map)
import qualified Data.Map                             as Map
import           Data.Monoid
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

-- | Given a request, either throw a possibly existent error, or create some
-- upload result.
type HandleUpload e m u   = Request -> ExceptT (Maybe e) m u

-- | Given a request and the potential result of handling an upload, return a
-- response (usually @Middleware@).
type RespondUpload e u r  = Request -> Either (Maybe e) u -> r

type ResponseSpec e u m r = (HandleUpload e m u, RespondUpload e u r)

-- | A map from an HTTP verb, to a responding and uploading mechanism.
newtype Verbs e u m r = Verbs
  { unVerbs :: Map Verb (ResponseSpec e u m r)
  } deriving (Monoid)

instance Functor (Verbs e u m) where
  fmap f (Verbs xs) = Verbs $ (second (f .*)) <$> xs


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

-- | @flip ($)@ for supplying a @Request@ into the two @ResponseSpec@ functions.
supplyReq :: Request
          -> Map Verb (ResponseSpec e u m r)
          -> Map Verb (ExceptT (Maybe e) m u, Either (Maybe e) u -> r)
supplyReq req xs = bimap ($ req) ($ req) <$> xs


-- | Take a verb map and a request, and return the lookup after providing the request
-- (for upload cases).
lookupVerb :: Verb
           -> Request
           -> Verbs e u m r
           -> Maybe (ExceptT (Maybe e) m u, Either (Maybe e) u -> r)
lookupVerb v req vmap = Map.lookup v $ supplyReq req $ unVerbs vmap

-- | Take the monadic partial result of @lookupVerb@, and actually handle the upload.
lookupVerbM :: Monad m => Verb -> Request -> Verbs e u m r -> m (Maybe r)
lookupVerbM v req vmap = runMaybeT $ do
  (uploadT, result) <- hoistMaybe $ lookupVerb v req vmap
  eUpload <- lift $ runExceptT uploadT
  return $ result eUpload


-- * Verb Writer

-- | The type variables are @r@ for the result, @e@ for the error type throwable
-- during uploading, @u@ is the sucessful upload type, and @m@ and @a@ form the
-- last two parts of the monad transformer.
newtype VerbListenerT r e u m a =
  VerbListenerT { runVerbListenerT :: StateT (Verbs e u m r) m a }
    deriving ( Functor, Applicative, Alternative, Monad, MonadFix, MonadPlus
             , MonadState (Verbs e u m r), MonadWriter w, MonadReader r, MonadIO
             , MonadError e', MonadCont, MonadBase b, MonadThrow, MonadCatch
             , MonadMask, MonadLogger
             )

deriving instance (MonadResource m, MonadBase IO m) => MonadResource (VerbListenerT r e u m)

execVerbListenerT :: Monad m => VerbListenerT r e u m a -> m (Verbs e u m r)
execVerbListenerT xs = execStateT (runVerbListenerT xs) mempty


instance MonadTrans (VerbListenerT r e u) where
  lift ma = VerbListenerT $ lift ma


-- | Turn a map from HTTP verbs to middleware, into a middleware.
verbsToMiddleware :: MonadIO m =>
                     VerbListenerT (MiddlewareT m) e u m ()
                  -> MiddlewareT m
verbsToMiddleware vl app req respond = do
  let v = getVerb req
  vmap <- execVerbListenerT vl
  mMiddleware <- lookupVerbM v req vmap
  fromMaybe (app req respond) $ do
    middleware <- mMiddleware
    return $ middleware app req respond

mapVerbs :: Monad m =>
            (a -> b)
         -> VerbListenerT a e u m () -> VerbListenerT b e u m ()
mapVerbs f vl = do
  vmap <- lift $ execVerbListenerT vl
  tell' $ f <$> vmap

-- * Combinators

-- | For simple @GET@ responses
get :: ( Monad m
       ) => r -> VerbListenerT r e u m ()
get r = tell' $ Verbs $ Map.singleton GET ( const $ throwError Nothing
                                          , const $ const r
                                          )

-- | Inspect the @Request@ object supplied by WAI
getReq :: ( Monad m
          ) => (Request -> r) -> VerbListenerT r e u m ()
getReq r = tell' $ Verbs $ Map.singleton GET ( const $ throwError Nothing
                                             , const . r
                                             )


-- | For simple @POST@ responses
post :: ( Monad m
        , MonadIO m
        ) => HandleUpload e m u -> (Either (Maybe e) u -> r) -> VerbListenerT r e u m ()
post handle r = tell' $ Verbs $ Map.singleton POST ( handle
                                                   , const r
                                                   )

-- | Inspect the @Request@ object supplied by WAI
postReq :: ( Monad m
           , MonadIO m
           ) => HandleUpload e m u -> RespondUpload e u r -> VerbListenerT r e u m ()
postReq handle r = tell' $ Verbs $ Map.singleton POST ( handle
                                                      , r
                                                      )


-- | For simple @PUT@ responses
put :: ( Monad m
       , MonadIO m
       ) => HandleUpload e m u -> (Either (Maybe e) u -> r) -> VerbListenerT r e u m ()
put handle r = tell' $ Verbs $ Map.singleton PUT ( handle
                                                 , const r
                                                 )

-- | Inspect the @Request@ object supplied by WAI
putReq :: ( Monad m
          , MonadIO m
          ) => HandleUpload e m u -> RespondUpload e u r -> VerbListenerT r e u m ()
putReq handle r = tell' $ Verbs $ Map.singleton PUT ( handle
                                                    , r
                                                    )


-- | For simple @DELETE@ responses
delete :: ( Monad m
          ) => r -> VerbListenerT r e u m ()
delete r = tell' $ Verbs $ Map.singleton DELETE ( const $ throwError Nothing
                                                , const $ const r
                                                )

-- | Inspect the @Request@ object supplied by WAI
deleteReq :: ( Monad m
             ) => (Request -> r) -> VerbListenerT r e u m ()
deleteReq r = tell' $ Verbs $ Map.singleton DELETE ( const $ throwError Nothing
                                                   , const . r
                                                   )


tell' :: (Monoid w, MonadState w m) => w -> m ()
tell' x = do
  xs <- S.get
  S.put $ xs <> x
