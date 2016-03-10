{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module Control.Monad.Trans.Indexed.Log.Internal where

import Control.Monad.Indexed
import Control.Monad.Indexed.Trans
import Control.Monad.Trans.Indexed (IndexedT(..))
import Data.HList.HList

newtype IndexedLogT m i o a = IndexedLogT { unIndexedLogT :: IndexedT m (HList i) (HList o) a } deriving Functor

instance (Functor m) => IxFunctor (IndexedLogT m) where
  imap = fmap
instance (Applicative m) => IxPointed (IndexedLogT m) where
  ireturn = IndexedLogT . ireturn
instance (Applicative m) => IxApplicative (IndexedLogT m) where
  iap (IndexedLogT f) (IndexedLogT a) = IndexedLogT $ f `iap` a
instance (Monad m) => IxMonad (IndexedLogT m) where
  ibind f (IndexedLogT m) = IndexedLogT $ ibind (unIndexedLogT . f) m
instance IxMonadTrans IndexedLogT where
  ilift ma = IndexedLogT $ ilift ma
