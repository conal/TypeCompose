{-# OPTIONS -fglasgow-exts #-}

-- TypeOperators

----------------------------------------------------------------------
-- |
-- Module      :  Data.Bijection
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  TypeOperators
-- 
-- Bijections.  See also [1], which provides a more general setting.
-- 
--  [1]: /There and Back Again: Arrows for Invertible Programming/,
--  <http://citeseer.ist.psu.edu/alimarine05there.html>.
-- 
-- 
----------------------------------------------------------------------

module Data.Bijection
  (
    Bijection(..),(:<->:)
  , idb, inverse, bimap, (--->)
  , inBi
  ) where

import Control.Arrow


infix 8 :<->:
infixr 2 --->

-- | A type of bijective arrows
data Bijection (~>) a b = Bi { biTo :: a ~> b, biFrom :: b ~> a }

-- | Bijective functions
type a :<->: b = Bijection (->) a b

-- | Bijective identity arrow.  Warning: uses 'arr' on @(~>)@.  If you
-- have no 'arr', but you have a @DeepArrow@, you can instead use @Bi idA
-- idA@.
idb :: Arrow (~>) => Bijection (~>) a a
idb = Bi idA idA where idA = arr id

-- | Inverse bijection
inverse :: Bijection (~>) a b -> Bijection (~>) b a
inverse (Bi ab ba) = Bi ba ab

instance Arrow (~>) => Arrow (Bijection (~>)) where
  arr = error "No arr for (:<->:)."
  Bi ab ba >>> Bi bc cb = Bi (ab >>> bc) (cb >>> ba)
  first  (Bi ab ba) = Bi (first  ab) (first  ba)
  second (Bi ab ba) = Bi (second ab) (second ba)
  Bi ab ba *** Bi cd dc = Bi (ab *** cd) (ba *** dc)
  (&&&) = error "No (***) for (:<->:)"
  -- Can't really define (&&&) unless we have a way to merge two @a@ values.
  -- Bi ab ba &&& Bi ac ca = Bi (ab &&& ac) (ba &&& ???)

-- Most but not all DeepArrow operations can be defined.  No @fstA@, @sndA@.

-- The '(***)' operator creates bijections on pairs.  Here are some similar tools.

-- | Bijections on functors
bimap :: Functor f => (a :<->: b) -> (f a :<->: f b)
bimap (Bi ab ba) = Bi (fmap ab) (fmap ba)

-- | Bijections on arrows.
(--->) :: Arrow (~>) => Bijection (~>) a b -> Bijection (~>) c d
       -> (a ~> c) :<->: (b ~> d)
Bi ab ba ---> Bi cd dc = Bi (\ ac -> ba>>>ac>>>cd) (\ bd -> ab>>>bd>>>dc)

-- | Apply a function in an alternative (monomorphic) representation.
inBi :: Arrow (~>) => Bijection (~>) a b -> (a ~> a) -> (b ~> b)
inBi (Bi to from) aa = from >>> aa >>> to
