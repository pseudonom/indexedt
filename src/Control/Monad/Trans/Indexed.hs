{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}

module Control.Monad.Trans.Indexed where

import Control.Monad hiding ((>>), (>>=), (=<<))
import Prelude hiding ((>>), (>>=), (=<<))

newtype IndexedT i o m a = IndexedT { unIndexedT :: m (i -> o, a) } deriving Functor

runIndexedT :: Functor m => IndexedT i o m a -> m a
runIndexedT = fmap snd . unIndexedT

passThrough :: (forall b. m b -> t b) -> IndexedT i o m a -> IndexedT i o t a
passThrough f (IndexedT m) = IndexedT $ f m

-- |
-- = `>>=`, etc.
-- We have to provide our own bind here because indexed monads don't fit the type class definition.
-- (i.e. `>>=` expects the input and output monad to be the same but `m x y` doesn't look like `m y z` to GHC.)
-- The convention in all that follows is that a `.` on one side of an operator means that side of the operator expects an indexed monad.
-- e.g. `.>>=` expects to take an indexed monadic value and feed it into a function which returns an unindexed monadic value

infixl 1 >>=
(>>=) :: Monad m => IndexedT x y m a -> (a -> IndexedT y z m b) -> IndexedT x z m b
IndexedT m >>= f =
  IndexedT $ do
    (g, a) <- m
    (h, b) <- unIndexedT $ f a
    return (h . g, b)

infixr 1 =<<
(=<<) :: Monad m => (a -> IndexedT y z m b) -> IndexedT x y m a -> IndexedT x z m b
(=<<) = flip (>>=)

infixl 1 >>
(>>) :: Monad m => IndexedT x y m a -> IndexedT y z m b -> IndexedT x z m b
a >> b = a >>= const b
