{-# LANGUAGE TypeSynonymInstances #-}

----------------------------------------------------------------------
-- |
-- Module      :  Data.CxMonoid
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  synonym instance
-- 
-- Context-dependent monoids
----------------------------------------------------------------------

module Data.CxMonoid (CxMonoid(..),CxMonoid',MonoidDict) where

import Data.Monoid (Monoid(..))

import Data.Adorn

-- | Type of context-dependent monoid.  Includes an explicit dictionary.
newtype CxMonoid a = CxMonoid { unCxMonoid :: CxMonoid' a }

-- | Unadorned 'CxMonoid'
type CxMonoid' a = MonoidDict a -> a

instance Adorn (CxMonoid' a) (CxMonoid a) where
  { adorn = CxMonoid ; unadorn = unCxMonoid }

-- | Dictionary for 'CxMonoid'.
type MonoidDict a = (a, a -> a -> a)

instance Monoid (CxMonoid a) where
  mempty = CxMonoid (\ (e,_) -> e)
  CxMonoid f `mappend` CxMonoid g  =
    CxMonoid (\ md@(_,op) -> f md `op` g md)

