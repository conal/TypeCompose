{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------
-- |
-- Module      :  Data.Bijection
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  ???
-- 
-- Bijections
----------------------------------------------------------------------

module Data.Bijection
  ( (:<->:)(..)
  , idb, inverse, bimap, (--->)
  , inBi
  ) where

import Control.Arrow


infix 8 :<->:
infixr 2 --->

-- | A type of bijections
data a :<->: b = Bi { biTo :: a -> b, biFrom :: b -> a }

idb :: a :<->: a
idb = Bi id id

inverse :: a :<->: b -> b :<->: a
inverse (Bi ab ba) = Bi ba ab

instance Arrow (:<->:) where
  arr = error "No arr for (:<->:)."
  Bi ab ba >>> Bi bc cb = Bi (ab >>> bc) (cb >>> ba)
  first  (Bi ab ba) = Bi (first  ab) (first  ba)
  second (Bi ab ba) = Bi (second ab) (second ba)
  Bi ab ba *** Bi cd dc = Bi (ab *** cd) (ba *** dc)
  -- Can't really define (&&&) unless we have a way to merge two @a@ values.
  -- Bi ab ba &&& Bi ac ca = Bi (ab &&& ac) (ba &&& ???)

-- Most but not all DeepArrow operations can be defined.  No fstA, sndA.

-- The (***) operator creates bijections on pairs.  Here are some similar tools.

-- Bijections on functors
bimap :: Functor f => (a :<->: b) -> (f a :<->: f b)
bimap (Bi ab ba) = Bi (fmap ab) (fmap ba)

-- Bijections on functions.  Not sure how best to orient the arguments.
-- Flip around first argument?
(--->) :: (a :<->: b) -> (c :<->: d) -> ((a -> c) :<->: (b -> d))
Bi ab ba ---> Bi cd dc = Bi (\ ac -> ba>>>ac>>>cd) (\ bd -> ab>>>bd>>>dc)


-- | Apply a function in an alternative (monomorphic) representation.
inBi :: (a :<->: b) -> (a -> a) -> (b -> b)
inBi (Bi to from) = (to .).(. from)
