{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module Control.Monad.Trans.Indexed.Log.Internal where

import Control.Monad.Trans.Indexed (IndexedT(..))
import Data.HList.HList

newtype IndexedLogT i o m a = IndexedLogT { unIndexedLogT :: IndexedT (HList i) (HList o) m a } deriving Functor
