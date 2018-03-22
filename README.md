wai-middleware-verbs
====================

Route to different Wai middleware based on the incoming HTTP verb
detected.

## Usage

This library depends on the [wai-transformers](https://hackage.haskell.org/package/wai-transformers)
package - we expect middleware to already be lifted to `MiddlewareT m`.

As an example, here could be your application:

```haskell
import Network.Wai.Trans
import Network.Wai.Middleware.Verbs


myMid1 :: MiddlewareT (ReaderT Env m)
myMid2 :: Middleware

uploadResponse :: ByteString -> MiddlewareT m
uploadResponse _ = liftMiddleware myMid2

verbRoutes :: VerbListenerT (MiddlewareT m) m ()
verbRoutes = do
  get myMid1
  post uploadResponse
```

Then, to use your newly assembled verb-router, turn the Verbs into a Middleware:

```haskell
verbMid :: MiddlewareT (ReaderT Env m)
verbMid = verbsToMiddleware verbRoutes
```

From there, you can deconstruct your monolithic monad stack back down to `IO`,
and plug-it-in to your existing middleware.
