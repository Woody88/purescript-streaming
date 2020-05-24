module Streaming.Prelude ( 
  -- * Types 
    module Stream
  , module Of 

  -- * producers 
  , yield 
  , nats
  , unfoldr

  -- * consumers 
  , print

  -- * sum and compose manipulation
  , sum
  , sum_

  -- * stream transformers
  , take 

  -- * folds
  , fold
  , fold_ 
  )
  where 

import Prelude

import Data.Either (Either(..))
import Data.Functor.Of (Of(..)) as Of
import Data.Functor.Of (Of, (:>))
import Data.Tuple.Nested (type (/\), (/\))
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

unfoldr :: forall m s a r. 
  Monad m 
  => (s -> m (Either r (a /\ s))) 
  -> s -> Stream (Of a) m r
unfoldr step = loop 
  where
    loop x = Effect do
      e <- step x
      case e of
        Left r      -> pure $ Return r
        Right (a /\ s) -> pure $ Step (a :> loop s)

take :: forall m f r. 
  Monad m 
  => Functor f 
  => Int 
  -> Stream f m r 
  -> Stream f m Unit
take n0 _ | n0 <= 0 = pure unit 
take n0 stream = eval n0 stream where
  eval 0 _ = pure unit 
  eval n p =
    case p of
      Step fas -> Step $ map (eval (n-1)) fas
      Effect m -> Effect $ map (eval n) m
      Return _ -> Return unit 

nats :: forall m r. Monad m => Stream (Of Int) m r 
nats = do 
  flip unfoldr 0 \n -> do 
    pure $ Right (n /\ (n + 1))
