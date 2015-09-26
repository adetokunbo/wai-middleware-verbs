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
  , RespondUpload
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
import           Data.Monoid
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.State hiding (get, put)
import qualified Control.Monad.State                  as S
import           Control.Monad.Except
import           Control.Error


-- * Verb Map

type HandleUpload e m u   = Request -> ExceptT (Maybe e) m u
type RespondUpload e u r        = Request -> Either (Maybe e) u -> r
type ResponseSpec e u m r = (HandleUpload e m u, RespondUpload e u r)

newtype Verbs e u m r = Verbs
  { unVerbs :: Map Verb (ResponseSpec e u m r)
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

supplyReq :: Request
          -> Map Verb (ResponseSpec e u m r)
          -> Map Verb (ExceptT (Maybe e) m u, Either (Maybe e) u -> r)
supplyReq req xs = bimap ($ req) ($ req) <$> xs

instance Functor (Verbs e u m) where
  fmap f (Verbs xs) = Verbs $ (second (f .*)) <$> xs


-- | Take a verb map and a request, and return the lookup after providing the request
-- (for upload cases).
lookupVerb :: Verb
           -> Request
           -> Verbs e u m r
           -> Maybe (ExceptT (Maybe e) m u, Either (Maybe e) u -> r)
lookupVerb v req vmap = Map.lookup v $ supplyReq req $ unVerbs vmap


lookupVerbM :: Monad m => Verb -> Request -> Verbs e u m r -> m (Maybe r)
lookupVerbM v req vmap = runMaybeT $ do
  (uploadT, result) <- hoistMaybe $ lookupVerb v req vmap
  eUpload <- lift $ runExceptT uploadT
  return $ result eUpload


-- * Verb Writer

newtype VerbListenerT r e u m a =
  VerbListenerT { runVerbListenerT :: StateT (Verbs e u m r) m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState (Verbs e u m r)
             , MonadIO
             )

execVerbListenerT :: Monad m => VerbListenerT r e u m a -> m (Verbs e u m r)
execVerbListenerT xs = execStateT (runVerbListenerT xs) mempty


instance MonadTrans (VerbListenerT r e u) where
  lift ma = VerbListenerT $ lift ma



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
