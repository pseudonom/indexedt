{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.Trans.Indexed where

import Control.Monad hiding ((>>), (>>=), (=<<))
import Control.Monad.Indexed
import Control.Monad.Indexed.Trans
import Prelude hiding ((>>), (>>=), (=<<))

newtype IndexedT m i o a = IndexedT { unIndexedT :: m (i -> o, a) } deriving Functor

runIndexedT :: Functor m => IndexedT m i o a -> m a
runIndexedT = fmap snd . unIndexedT

mapLift :: (forall b. m b -> t b) -> IndexedT m i o a -> IndexedT t i o a
mapLift f (IndexedT m) = IndexedT $ f m

instance (Functor m) => IxFunctor (IndexedT m) where
  imap = fmap
instance (Applicative m) => IxPointed (IndexedT m) where
  ireturn a = IndexedT $ pure (id, a)
instance (Applicative m) => IxApplicative (IndexedT m) where
  iap (IndexedT f) (IndexedT a) = IndexedT $ (\(iToJ, aToB) (jToK, a') -> (jToK . iToJ, aToB a')) <$> f <*> a
instance (Monad m) => IxMonad (IndexedT m) where
  f `ibind` IndexedT m =
   IndexedT $ do
      (g, a) <- m
      (h, b) <- unIndexedT $ f a
      return (h . g, b)
instance IxMonadTrans IndexedT where
  ilift ma = IndexedT $ (id,) <$> ma
