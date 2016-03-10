{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Trans.Indexed.Log
  ( module Control.Monad.Trans.Indexed.Log
  , IndexedLogT()
  )
  where

import Control.Monad hiding ((>>), (>>=), (=<<))
import Data.HList.HList
import Prelude hiding ((>>), (>>=), (=<<))

import Control.Monad.Trans.Indexed (IndexedT(..), runIndexedT)
import qualified Control.Monad.Trans.Indexed as I
import Control.Monad.Trans.Indexed.Log.Internal

runIndexedLogT :: (Functor m) => IndexedLogT '[] o m a -> m a
runIndexedLogT = runIndexedT . unIndexedLogT

data Unknown = Unknown

-- |
-- = `:::` and `ConsCollapse`
-- This machinery allows us to collapse multiple adjacent actions of the same type
-- e.g. Instead of `'[Unknown, Unknown, DBTX, TeacherAuth, Unknown]`,
-- we'll have `'[Unknown, DBTX, TeacherAuth, Unknown]`

type family Elem (a :: k) (as :: [k]) :: Bool where
  Elem a '[] = 'False
  Elem a (a ': as) = 'True
  Elem a (b ': as) = Elem a as

infixr 5 :::
type family (a :: k) ::: (as :: [k]) :: [k] where
  a ::: a ': as = a ': as
  a ::: as = a ': as
type Performs p i m a = (ConsCollapse p i) => IndexedLogT i (p ::: i) m a

class ConsCollapse a b where
  consCollapse :: a -> HList b -> HList (a ::: b)
instance {-# OVERLAPPING #-} ConsCollapse a (a ': as) where
  consCollapse _ as = as
instance {-# OVERLAPPABLE #-} ((b ::: (a ': as)) ~ (b ': a ': as)) => ConsCollapse b (a ': as) where
  consCollapse a as = a `HCons` as
instance ConsCollapse b ('[]) where
  consCollapse a as = a `HCons` as

logLift :: (ConsCollapse w i) => Applicative m => w -> m a -> Performs w i m a
logLift w ma = IndexedLogT . IndexedT $ (,) <$> pure (consCollapse w) <*> ma

infixl 1 >>=
(>>=) :: Monad m => IndexedLogT x y m a -> (a -> IndexedLogT y z m b) -> IndexedLogT x z m b
IndexedLogT m >>= f = IndexedLogT $ m I.>>= unIndexedLogT . f

infixr 1 =<<
(=<<) :: Monad m => (a -> IndexedLogT y z m b) -> IndexedLogT x y m a -> IndexedLogT x z m b
(=<<) = flip (>>=)

infixl 1 >>
(>>) :: Monad m => IndexedLogT x y m a -> IndexedLogT y z m b -> IndexedLogT x z m b
a >> b = a >>= const b


infixl 1 .>>=
(.>>=)
  :: (Monad m, ConsCollapse Unknown o)
  => IndexedLogT i o m a -> (a -> m b) -> IndexedLogT i (Unknown ::: o) m b
ima .>>= f = ima >>= logLift Unknown . f

infixl 1 >>=.
(>>=.)
  :: (Monad m, ConsCollapse Unknown i)
  => m a -> (a -> IndexedLogT (Unknown ::: i) o m b) -> IndexedLogT i o m b
ma >>=. f = logLift Unknown ma >>= f

infixr 1 .=<<
(.=<<)
  :: (Monad m, ConsCollapse Unknown i)
  => (a -> IndexedLogT (Unknown ::: i) o m b) -> m a -> IndexedLogT i o m b
(.=<<) = flip (>>=.)

infixr 1 =<<.
(=<<.)
  :: (Monad m, ConsCollapse Unknown o)
  => (a -> m b) -> IndexedLogT i o m a -> IndexedLogT i (Unknown ::: o) m b
(=<<.) = flip (.>>=)

infixl 1 .>>
(.>>)
  :: (Monad m, ConsCollapse Unknown o)
  => IndexedLogT i o m a -> m b -> IndexedLogT i (Unknown ::: o) m b
a .>> b = a .>>= const b

infixl 1 >>.
(>>.)
  :: (Monad m, ConsCollapse Unknown i)
  => m a -> IndexedLogT (Unknown ::: i) o m b -> IndexedLogT i o m b
a >>. b = a >>=. const b
