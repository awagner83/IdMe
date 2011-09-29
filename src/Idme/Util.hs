module Idme.Util (condA, constF, pairOf, (>>-)) where

import Control.Applicative (Applicative, pure)
import Data.Functor ((<$))


-- | inject into function, apply effect, and return itself operator
(>>-) :: (Functor m, Monad m) => m a -> (a -> m b) -> m a
(>>-) x f = x >>= constF f
infixl 8 >>-

-- | const for Functors
constF :: Functor f => (a -> f b) -> a -> f a
constF f a = a <$ f a

-- | Duplicate single item to pair of itself
pairOf :: a -> (a, a)
pairOf a = (a, a)

-- | Inspect given value and optionally execute action
condA :: Applicative a => b -> (b -> Bool) -> (b -> a c) -> a b
condA v t a = if t v then constF a v else pure v

