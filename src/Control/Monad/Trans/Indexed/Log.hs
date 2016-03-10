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
import Control.Monad.Trans.Indexed.Log.Internal

runIndexedLogT :: (Functor m) => IndexedLogT m '[] o a -> m a
runIndexedLogT = runIndexedT . unIndexedLogT

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
type Performs p m i a = (ConsCollapse p i) => IndexedLogT m i (p ::: i) a

class ConsCollapse a b where
  consCollapse :: a -> HList b -> HList (a ::: b)
instance {-# OVERLAPPING #-} ConsCollapse a (a ': as) where
  consCollapse _ as = as
instance {-# OVERLAPPABLE #-} ((b ::: (a ': as)) ~ (b ': a ': as)) => ConsCollapse b (a ': as) where
  consCollapse a as = a `HCons` as
instance ConsCollapse b ('[]) where
  consCollapse a as = a `HCons` as

logLift :: (ConsCollapse w i) => Applicative m => w -> m a -> Performs w m i a
logLift w ma = IndexedLogT . IndexedT $ (,) Prelude.<$> pure (consCollapse w) Prelude.<*> ma

