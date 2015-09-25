{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  , MultiParamTypeClasses
  , TupleSections
  #-}

module Network.Wai.Middleware.Verbs
  ( Verbs (..)
  , Verb
  , getVerb
  , HandleUpload
  , Respond
  , ResponseSpec
  , supplyReq
  , lookupVerb
  , lookupVerbM
  , VerbListenerT (..)
  , execVerbListenerT
  , verbsToMiddleware
  , get
  , getReq
  , post
  , postReq
  , put
  , putReq
  , delete
  , deleteReq
  ) where


import           Network.Wai.Trans
import           Network.HTTP.Types

import           Data.Function.Syntax
import           Data.Bifunctor
import           Data.Map (Map)
import qualified Data.Map                             as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.State hiding (get, put)
import qualified Control.Monad.State                  as S
import           Control.Error.Util


-- * Verb Map

newtype Verbs u m r = Verbs
  { unVerbs :: Map Verb (ResponseSpec u m r)
  } deriving (Monoid)

type Verb = StdMethod

getVerb :: Request -> Verb
getVerb req = fromMaybe GET $ httpMethodToMSym $ requestMethod req
  where
    httpMethodToMSym :: Method -> Maybe Verb
    httpMethodToMSym x | x == methodGet    = Just GET
                       | x == methodPost   = Just POST
                       | x == methodPut    = Just PUT
                       | x == methodDelete = Just DELETE
                       | otherwise        = Nothing

type HandleUpload m u   = Request -> m (Maybe u)
type Respond u r        = Request -> Maybe u -> r
type ResponseSpec u m r = (HandleUpload m u, Respond u r)


supplyReq :: Request
          -> Map Verb (ResponseSpec u m r)
          -> Map Verb (m (Maybe u), Maybe u -> r)
supplyReq req xs = bimap ($ req) ($ req) <$> xs

instance Functor (Verbs u m) where
  fmap f (Verbs xs) = Verbs $ (second (f .*)) <$> xs


-- | Take a verb map and a request, and return the lookup after providing the request
-- (for upload cases).
lookupVerb :: Verb -> Request -> Verbs u m r -> Maybe (m (Maybe u), Maybe u -> r)
lookupVerb v req vmap = Map.lookup v $ supplyReq req $ unVerbs vmap


lookupVerbM :: Monad m => Verb -> Request -> Verbs u m r -> m (Maybe r)
lookupVerbM v req vmap = runMaybeT $ do
  (mUM, mUtoResult) <- hoistMaybe $ lookupVerb v req vmap
  mU <- lift mUM
  return $ mUtoResult mU


-- * Verb Writer

newtype VerbListenerT r u m a =
  VerbListenerT { runVerbListenerT :: StateT (Verbs u m r) m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState (Verbs u m r)
             , MonadIO
             )

execVerbListenerT :: Monad m => VerbListenerT r u m a -> m (Verbs u m r)
execVerbListenerT xs = execStateT (runVerbListenerT xs) mempty


instance MonadTrans (VerbListenerT r u) where
  lift ma = VerbListenerT $ lift ma



verbsToMiddleware :: MonadIO m =>
                     VerbListenerT (MiddlewareT m) u m ()
                  -> MiddlewareT m
verbsToMiddleware vl app req respond = do
  let v = getVerb req
  vmap <- execVerbListenerT vl
  mMiddleware <- lookupVerbM v req vmap
  fromMaybe (app req respond) $ do
    middleware <- mMiddleware
    return $ middleware app req respond


-- * Combinators

-- | For simple @GET@ responses
get :: ( Monad m
       ) => r -> VerbListenerT r u m ()
get r = tell' $ Verbs $ Map.singleton GET ( const $ return Nothing
                                         , const $ const r
                                         )

-- | Inspect the @Request@ object supplied by WAI
getReq :: ( Monad m
          ) => (Request -> r) -> VerbListenerT r u m ()
getReq r = tell' $ Verbs $ Map.singleton GET ( const $ return Nothing
                                            , const . r)


-- | For simple @POST@ responses
post :: ( Monad m
        , MonadIO m
        ) => HandleUpload m u -> (Maybe u -> r) -> VerbListenerT r u m ()
post handle r = tell' $ Verbs $ Map.singleton POST ( handle
                                                  , const r
                                                  )

-- | Inspect the @Request@ object supplied by WAI
postReq :: ( Monad m
           , MonadIO m
           ) => HandleUpload m u -> (Request -> Maybe u -> r) -> VerbListenerT r u m ()
postReq handle r = tell' $ Verbs $ Map.singleton POST ( handle
                                                     , r
                                                     )


-- | For simple @PUT@ responses
put :: ( Monad m
       , MonadIO m
       ) => HandleUpload m u -> (Maybe u -> r) -> VerbListenerT r u m ()
put handle r = tell' $ Verbs $ Map.singleton PUT ( handle
                                                , const r
                                                )

-- | Inspect the @Request@ object supplied by WAI
putReq :: ( Monad m
          , MonadIO m
          ) => HandleUpload m u -> (Request -> Maybe u -> r) -> VerbListenerT r u m ()
putReq handle r = tell' $ Verbs $ Map.singleton PUT ( handle
                                                   , r
                                                   )


-- | For simple @DELETE@ responses
delete :: ( Monad m
          ) => r -> VerbListenerT r u m ()
delete r = tell' $ Verbs $ Map.singleton DELETE ( const $ return Nothing
                                               , const $ const r
                                               )

-- | Inspect the @Request@ object supplied by WAI
deleteReq :: ( Monad m
             ) => (Request -> r) -> VerbListenerT r u m ()
deleteReq r = tell' $ Verbs $ Map.singleton DELETE ( const $ return Nothing
                                                  , const . r
                                                  )


tell' :: (Monoid w, MonadState w m) => w -> m ()
tell' x = do
  xs <- S.get
  S.put $ xs <> x
