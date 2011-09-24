module Idme.Util (constF, (>>-)) where

import Data.Functor ((<$))


-- | inject into function, apply effect, and return itself operator
(>>-) :: (Functor m, Monad m) => m a -> (a -> m b) -> m a
(>>-) x f = x >>= constF f
infixl 8 >>-

-- | const for Functors
constF :: Functor f => (a -> f b) -> a -> f a
constF f a = a <$ f a

