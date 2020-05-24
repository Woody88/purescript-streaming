module Data.Functor.Of where

import Prelude

import Data.Bifunctor (class Bifunctor)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)

data Of a b = Of a b

infixr 6 Of as :>

derive instance genericOf :: Generic (Of a b) _

instance semiGroupOf :: (Semigroup a, Semigroup b) => Semigroup (Of a b) where 
  append (m :> w) (m' :> w') = append m m' :> append w w' 

instance monoidOf :: (Monoid a, Monoid b) => Monoid (Of a b) where 
  mempty = mempty :> mempty 

instance functorOf :: Functor (Of a) where 
  map f (a :> x) = a :> f x 

instance bifunctorOf :: Bifunctor Of where 
  bimap f g (a :> x) = f a :> g x

instance applyOf :: (Monoid a) => Apply (Of a) where 
  apply (m :> f) (m' :> x) = append m m' :> f x 

instance applicativeOf :: (Monoid a) => Applicative (Of a) where 
  pure x = mempty :> x 

instance bindOf :: (Monoid a) => Bind (Of a) where 
  bind (m :> x) f = let (m' :> y) = f x in append m m' :> y

instance monadOf :: (Monoid a) => Monad (Of a) 

derive instance eqOf :: (Eq a, Eq b) => Eq (Of a b)
derive instance ordOf :: (Ord a, Ord b) => Ord (Of a b) 

instance foldableOf :: Foldable (Of a) where 
  foldr f z (_ :> x) = f x z
  foldl f z (_ :> x) = f z x
  foldMap f (_ :> x) = f x