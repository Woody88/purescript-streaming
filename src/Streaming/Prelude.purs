module Streaming.Prelude ( 
  -- * Types 
    module Stream
  , module Of 

  -- * producers 
  , yield 

  -- * consumers 
  , print

  -- * sum and compose manipulation
  , sum
  , sum_
  )
  where 

import Prelude

import Data.Functor.Of (Of(..)) as Of
import Data.Functor.Of (Of, (:>))
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

sum :: forall m a r.  Monad m => Semiring a => Stream (Of a) m r -> m (Of a r)
sum = fold (+) zero identity

sum_ :: forall m a.  Monad m => Semiring a => Stream (Of a) m Unit -> m a
sum_ = fold_ (+) zero identity

fold :: forall m r x a b. Monad m => (x -> a -> x) -> x -> (x -> b) -> Stream (Of a) m r -> m (Of b r)
fold step begin done str =  fold_eval str begin
  where 
    fold_eval stream x = case stream of 
      Return r         -> pure (done x :> r) 
      Effect m         -> m >>= \stream' -> fold_eval stream' x 
      Step (a :> rest) -> fold_eval rest $ step x a  

fold_ :: forall m r x a b. Monad m => (x -> a -> x) -> x -> (x -> b) -> Stream (Of a) m r -> m b
fold_ step begin done = map (\(a :> _) -> a) <<< fold step begin done