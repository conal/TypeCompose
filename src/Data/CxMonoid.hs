{-# LANGUAGE TypeSynonymInstances, TypeOperators, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.CxMonoid
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  synonym instance
-- 
-- Context-dependent monoids
----------------------------------------------------------------------

module Data.CxMonoid (MonoidDict, CxMonoid(..), biCxMonoid) where

import Data.Monoid (Monoid(..))
import qualified Data.Semigroup as Sem

import Data.Bijection
import Data.Title

-- | Dictionary for 'CxMonoid'.
type MonoidDict a = (a, a -> a -> a)

-- | Type of context-dependent monoid.  Includes an explicit dictionary.
newtype CxMonoid a = CxMonoid { unCxMonoid :: MonoidDict a -> a }

-- | @newtype@ bijection
biCxMonoid :: (MonoidDict a -> a) :<->: CxMonoid a
biCxMonoid = Bi CxMonoid unCxMonoid

instance Sem.Semigroup (CxMonoid a) where
  CxMonoid f <> CxMonoid g  =
    CxMonoid (\ md@(_,op) -> f md `op` g md)

instance Monoid (CxMonoid a) where
  mempty = CxMonoid (\ (e,_) -> e)
  mappend = (Sem.<>)

-- Exploit the function instance of 'Title'
instance Title a => Title (CxMonoid a) where
  title str = inBi biCxMonoid $ title str
