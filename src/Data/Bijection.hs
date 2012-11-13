{-# LANGUAGE TypeOperators, CPP #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.Bijection
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  TypeOperators
-- 
-- Bijections.  For a more general setting, see also [1]
-- /There and Back Again: Arrows for Invertible Programming/,
-- <http://citeseer.ist.psu.edu/alimarine05there.html>.
----------------------------------------------------------------------

module Data.Bijection
  (
    Bijection(..),(:<->:)
  , idb, inverse, bimap, (--->)
  , inBi
  ) where

#if __GLASGOW_HASKELL__ >= 609
import Control.Category
import Prelude hiding ((.), id)
#endif
import Control.Arrow


infix 8 :<->:
infixr 2 --->

-- | A type of bijective arrows
data Bijection j a b = Bi { biTo :: a `j` b, biFrom :: b `j` a }

-- | Bijective functions
type a :<->: b = Bijection (->) a b

-- | Bijective identity arrow.  Warning: uses 'arr' on @(~>)@.  If you
-- have no 'arr', but you have a @DeepArrow@, you can instead use @Bi idA
-- idA@.
idb :: Arrow j => Bijection j a a
idb = Bi idA idA where idA = arr id

-- | Inverse bijection
inverse :: Bijection j a b -> Bijection j b a
inverse (Bi ab ba) = Bi ba ab

#if __GLASGOW_HASKELL__ >= 609
instance Category j => Category (Bijection j) where
  id = Bi id id
  Bi bc cb . Bi ab ba = Bi (bc . ab) (ba . cb)
#endif

instance Arrow j => Arrow (Bijection j) where
#if __GLASGOW_HASKELL__ < 609
  Bi ab ba >>> Bi bc cb = Bi (ab >>> bc) (cb >>> ba)
#endif
  arr = error "No arr for (:<->:)."
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
(--->) :: Arrow j => Bijection j a b -> Bijection j c d
       -> (a `j` c) :<->: (b `j` d)
Bi ab ba ---> Bi cd dc = Bi (\ ac -> ba>>>ac>>>cd) (\ bd -> ab>>>bd>>>dc)

-- TODO: Rewrite (--->) via (~>).  Currently would cause a module cycle
-- 
-- Bi ab ba ---> Bi cd dc = Bi (ac ~> cd) (ab ~> dc)


-- | Apply a function in an alternative (monomorphic) representation.
inBi :: Arrow j => Bijection j a b -> (a `j` a) -> (b `j` b)
inBi (Bi to from) aa = from >>> aa >>> to
