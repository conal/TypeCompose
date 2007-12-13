{-# LANGUAGE TypeSynonymInstances #-}

----------------------------------------------------------------------
-- |
-- Module      :  Data.Partial
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
-- 
-- A monoid 'Partial' of partial values.  See the [Teaser] and [Solution] blog
-- posts.
-- 
--   [Teaser]:   <http://conal-elliott.blogspot.com/2007/07/type-for-partial-values.html>
--   [Solution]: <http://conal-elliott.blogspot.com/2007/07/implementing-type-for-partial-values.html>
-- 
-- Also defines a 'FunAble' instance, so that @FunA Partial@ is an arrow.
----------------------------------------------------------------------

module Data.Partial
  (
  -- * Partial values
    Partial, PartialX, valp, pval
  , pUnElt, pUnArg, pUnRes, pUnSrc
  -- * Support for arrow partial value arrow
  -- via 'FunAble' instance
  ) where

import Data.Monoid
import Control.Arrow

import Control.Compose (FunAble(..),inEndo)

import Data.Pair

-- | Partial value.  Represented an endomorphism, which is a 'Monoid'
-- under 'id' and '(.)'.  Then 'mempty' is the completely undefined value,
-- and in @u `@'mappend'@` v@, @v@ selectively replaces parts of @u@.  The
-- 'Endo' instances for 'Pair', 'Unpair', 'Copair', 'Unfun', and 'Cofun'
-- are all very useful on partial values.
type Partial = Endo

type PartialX a b = Partial a -> Partial b

-- | Treat a full value as a partial one.  Fully overrides any
-- \"previous\" (earlier argument to @mappend@) partial value.
valp :: c -> Partial c
valp c = Endo (const c)

-- | Force a partial value into a full one, filling in bottom for any
-- missing parts.
pval :: Partial c -> c
pval (Endo f) = f (error "Partial: absent info")


-- | Inverse to \"element\" access, on all elements.  A way to inject some
-- info about every element.  For @f@, consider '[]', @(->) a@,
-- @Event@, etc.

pUnElt :: Functor f => PartialX a (f a)
pUnElt = inEndo fmap

-- | Provide in info about a function argument
pUnArg :: PartialX u (u -> v)
pUnArg = inEndo (flip (.))            -- \ uv -> \ u -> uv (uu u)

-- | Provide info about a function result
pUnRes :: PartialX v (u -> v)
pUnRes = inEndo (.)

-- | Inject a partial argument-source into a partial function-sink.
pUnSrc :: PartialX a ((a -> b) -> o)
pUnSrc = pUnArg . pUnArg


{----------------------------------------------------------
    'FunA' support for arrows on partial values
----------------------------------------------------------}

instance FunAble Partial where
  arrFun    = pArr
  firstFun  = pFirst
  secondFun = pSecond

-- I don't think we can define @arr f@ unless we can invert @f@.  Same
-- problem exists in "There and back again: arrows for invertible
-- programming".  Suggests refactoring Arrow.

pArr :: (a->b) -> PartialX a b
pArr f = inEndo $ (f .) . (. inv f)
 where
   inv :: (a->b) -> (b->a)
   inv = error "inv -- can't do it."

-- Since @pArr == fmap@, nor can we make a @Functor@ instance of
-- @PartialFun a@.

pFirst  :: PartialX a a' -> PartialX (a,b) (a',b)
pFirst  f = uncurry pair . first f . unpair

pSecond :: PartialX b b' -> PartialX (a,b) (a,b')
pSecond g = uncurry pair . second g . unpair

-- The following is not quite equivalent, since mappend doesn't commute.
-- 
-- pSecond g ab = pUnSnd (g b) `mappend` pUnFst a
--   where (a,b) = dsPPair ab


-- TODO: DeepArrow instance for PartialFun (perhaps in the DeepArrow
-- library) .  Some methods are easy, and some hard or impossible.
