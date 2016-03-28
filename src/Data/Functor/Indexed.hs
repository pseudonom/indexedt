-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Indexed
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Reiner Pope <reiner.pope@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
{-# LANGUAGE PolyKinds #-}
module Data.Functor.Indexed
  ( IxFunctor(..)
  , IxCopointed(..)
  , IxPointed(..)
  , IxApplicative(..)
  ) where

class IxFunctor (f :: i -> i -> * -> *) where
  imap :: (a -> b) -> f j k a -> f j k b

class IxPointed m => IxApplicative (m :: l -> l -> * -> *) where
  iap :: m i j (a -> b) -> m j k a -> m i k b

class IxFunctor m => IxPointed (m :: l -> l -> * -> *) where
  ireturn :: a -> m i i a

class IxFunctor w => IxCopointed (w :: k -> k -> * -> *) where
  iextract :: w i i a -> a

{-# RULES
"iextract/ireturn" iextract . ireturn = id
 #-}
