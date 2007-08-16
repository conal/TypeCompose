{-# OPTIONS #-}

----------------------------------------------------------------------
-- |
-- Module      :  Data.CxMonoid
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  ???
-- 
-- Context-dependent monoids
----------------------------------------------------------------------

module Data.CxMonoid (CxMonoid(..),MonoidDict) where

import Data.Monoid (Monoid(..))

-- | Type of context-dependent monoid.  Includes an explicit dictionary.
newtype CxMonoid a =
  CxMonoid { unCxMonoid :: MonoidDict a -> a }

-- | Dictionary for 'CxMonoid'.
type MonoidDict a = (a, a -> a -> a)

instance Monoid (CxMonoid a) where
  mempty = CxMonoid (\ (e,_) -> e)
  CxMonoid f `mappend` CxMonoid g  =
    CxMonoid (\ md@(_,op) -> f md `op` g md)
