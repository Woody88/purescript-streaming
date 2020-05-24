module Streaming.Internal where

import Prelude

data Stream f m r 
  = Step (f (Stream f m r))
  | Effect (m (Stream f m r))
  | Return r

instance functorStream :: (Functor f, Monad m) => Functor (Stream f m) where 
  map f = eval 
    where 
      eval stream = case stream of 
        Return r -> Return (f r)
        Effect m -> Effect $ m >>= \stream' -> pure $ eval stream' 
        Step g   -> Step $ map eval g

instance applyStream :: (Functor f, Monad m) => Apply (Stream f m) where 
  apply streamf streamx = do 
    f <- streamf
    x <- streamx 
    pure $ f x

instance applicativeStream :: (Functor f, Monad m) => Applicative (Stream f m) where 
  pure = Return 

instance bindStream :: (Functor f, Monad m) => Bind (Stream f m) where 
  bind s f = eval s 
    where 
      eval stream = case stream of 
        Return r -> f r
        Effect m -> Effect $ map eval m
        Step g   -> Step $ map eval g

instance monadStream :: (Functor f, Monad m) => Monad (Stream f m)

instance semigroupStream :: (Functor f, Monad m, Semigroup r) => Semigroup (Stream f m r) where
  append a b = a >>= \r -> map (r <> _) b 

instance monoidStream :: (Functor f, Monad m, Monoid r) => Monoid (Stream f m r) where
  mempty = pure mempty  