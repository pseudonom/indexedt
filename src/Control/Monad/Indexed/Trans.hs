-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Indexed.Trans
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Reiner Pope <reiner.pope@gmail.com>
-- Stability   :  experimental 
-- Portability :  portable
--
-- TODO: figure out a meaningful way for indexed monads to transform indexed 
-- monads
----------------------------------------------------------------------------
{-# LANGUAGE PolyKinds #-}
module Control.Monad.Indexed.Trans where

class IxMonadTrans (t :: (* -> *) -> j -> j -> * -> *) where
  ilift :: Monad m => m a -> t m i i a
