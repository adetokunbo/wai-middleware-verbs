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
  , -- ** Monad Transformer
    VerbListenerT (..)
  , execVerbListenerT
  , -- * Utilities
    lookupVerb
  , getVerb
  ) where


import           Network.Wai (Request (..))
import           Network.HTTP.Types

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy                    as HM
import           Data.Monoid
import           Data.Hashable
import           Control.Applicative
import           Control.Monad.Trans
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
type VerbMap r = HashMap Verb r

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
lookupVerb :: Verb -> VerbMap r -> Maybe r
lookupVerb = HM.lookup

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
get r = tell' $ HM.singleton GET r

{-# INLINEABLE get #-}

-- | For simple @POST@ responses
post :: ( Monad m
        , Monoid r
        ) => r -> VerbListenerT r m ()
post r = tell' $ HM.singleton POST r

{-# INLINEABLE post #-}

-- | For simple @PUT@ responses
put :: ( Monad m
       , Monoid r
       ) => r -> VerbListenerT r m ()
put r = tell' $ HM.singleton PUT r

{-# INLINEABLE put #-}

-- | For simple @DELETE@ responses
delete :: ( Monad m
          , Monoid r
          ) => r -> VerbListenerT r m ()
delete r = tell' $ HM.singleton DELETE r

{-# INLINEABLE delete #-}

tell' :: (Monoid r, MonadState (VerbMap r) m) => VerbMap r -> m ()
tell' x = modify' (\y -> HM.unionWith (<>) y x)

{-# INLINEABLE tell' #-}
