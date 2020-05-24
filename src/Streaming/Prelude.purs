module Streaming.Prelude ( 
  -- Types 
    module Stream
  , module Of 

  -- producers 
  , yield 

  -- consumers 
  , print
  )
  where 

import Prelude

import Data.Functor.Of (Of, (:>))
import Data.Functor.Of (Of(..)) as Of
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Streaming.Internal (Stream(..))
import Streaming.Internal (Stream) as Stream


yield :: forall a m. Monad m => a -> Stream (Of a) m Unit 
yield a = Step (a :> Return unit)

print :: forall m a r. MonadEffect m => Show a => Stream (Of a) m r -> m r
print = loop 
  where
    loop stream = case stream of
      Return r         -> pure r
      Effect m         -> m >>= loop
      Step (a :> rest) -> do
        Console.logShow a
        loop rest