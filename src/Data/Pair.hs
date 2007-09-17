{-# LANGUAGE Rank2Types, TypeOperators, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

----------------------------------------------------------------------
-- |
-- Module      :  Data.Pair
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  GHC
-- 
-- Pair-related type constructor classes.  See "Data.Fun" for similar classes.
----------------------------------------------------------------------

module Data.Pair
  (
  -- * Pairings
    PairTy, Pair(..)
  , apPair, ppPair, arPair
  -- * Unpairings
  , UnpairTy, Unpair(..)
  -- * Dual unpairings
  , Copair(..), copair
  ) where


import Data.Monoid
import Control.Arrow
import Control.Applicative

import Control.Compose


{----------------------------------------------------------
    Pairings
----------------------------------------------------------}

-- | Type of 'pair' method
type PairTy f = forall a b. f a -> f b -> f (a,b)

-- | Type constructor class for pair-like things.  Generalizes 'zip'.
-- Here are some standard instance templates you can fill in.  They're not
-- defined in the general forms below, because they would lead to a lot of
-- overlap.
-- 
-- @
--   instance Applicative f => Pair f where
--       pair = liftA2 (,)
--   instance (Applicative h, Pair f) => Pair (h `O` f) where
--       pair = apPair
--   instance (Functor g, Pair g, Pair f) => Pair (g `O` f)
--       where pair = ppPair
--   instance (Arrow (~>), Unpair f, Pair g) => Pair (Arrw (~>) f g) where
--       pair = arPair
--   instance (Monoid_f h, Copair h) => Pair h where
--       pair = copair
-- @

class Pair f where
  pair :: PairTy f         -- ^ Form a pair-like value (generalizes 'zip')

-- Standard instances (Applicative f)
instance Monoid u => Pair ((,)  u) where pair = liftA2 (,)
instance             Pair ((->) u) where pair = liftA2 (,)
instance             Pair IO       where pair = liftA2 (,)

instance Monoid o => Pair (Const o) where
  pair = inConst2 mappend

instance Pair Id where Id a `pair` Id b = Id (a,b)

-- Standard instance, e.g., (~>) = (->)
-- This one requires UndecidableInstances.  Alternatively, specialize to
-- (->) and other arrows as desired.
instance (Arrow (~>), Monoid_f (Flip (~>) o)) =>
  Pair (Flip (~>) o) where pair = copair

-- | Handy for 'Pair' instances
apPair :: (Applicative h, Pair f) => PairTy (h `O` f)
apPair = inO2 (liftA2 pair)

-- | Handy for 'Pair' instances
ppPair :: (Functor g, Pair g, Pair f) => PairTy (g `O` f)
ppPair = inO2 $ \ gfa gfb -> fmap (uncurry pair) (gfa `pair` gfb)

-- | Pairing of 'Arrw' values.  /Warning/: definition uses 'arr', so only
-- use if your arrow has a working 'arr'.
arPair :: (Arrow (~>), Unpair f, Pair g) => PairTy (Arrw (~>) f g)
arPair = inArrw2 $ \ fga fgb ->
  arr unpair >>> fga***fgb >>> arr (uncurry pair)

-- Standard instance
instance (Arrow (~>), Unpair f, Pair g) => Pair (Arrw (~>) f g)
  where pair = arPair

instance (Pair f, Pair g) => Pair (f :*: g) where
  pair = inProd2 (pair ***# pair)


{----------------------------------------------------------
    Unpairings
----------------------------------------------------------}

-- | Type of 'unpair' method.  Generalizes 'unzip'.
type UnpairTy f = forall a b. f (a,b) -> (f a, f b)

-- | Dissectable as pairs.  Minimal instance definition: either (a)
-- 'unpair' /or/ (b) both of 'pfst' /and/ 'psnd'.
-- A standard template to substitute any 'Functor' @f.@  But watch out for
-- effects!
-- 
-- @
--   instance Functor f => Unpair f where {pfst = fmap fst; psnd = fmap snd}
-- @
class Unpair f where
  unpair :: UnpairTy f                  -- ^ Deconstruct pair-like value
  pfst   :: f (a,b) -> f a              -- ^ First part of pair-like value
  psnd   :: f (a,b) -> f b              -- ^ Second part of pair-like value

  unpair = pfst &&& psnd
  pfst   = fst.unpair
  psnd   = snd.unpair

instance Unpair (Const a) where
  unpair (Const a) = (Const a, Const a)

instance Unpair Id where
  unpair (Id (a,b)) = (Id a, Id b)

-- Standard instance
instance Unpair [] where { pfst = fmap fst; psnd = fmap snd }


{----------------------------------------------------------
    Dual unpairings
----------------------------------------------------------}

-- | Dual to 'Unpair'.
-- Especially handy for contravariant functors ('Cofunctor') .  Use this
-- template (filling in @f@) :
-- 
-- @
--   instance Cofunctor f => Copair f where
--     { cofst = cofmap fst ; cosnd = cofmap snd }
-- @
class Copair f where
  cofst :: f a -> f (a,b)               -- ^ Pair-like value from first part
  cosnd :: f b -> f (a,b)               -- ^ Pair-like value from second part

instance Copair (Const e) where
  cofst = inConst id
  cosnd = inConst id

-- Standard instance for contravariant functors
instance Arrow (~>) => Copair (Flip (~>) o) where
  { cofst = cofmap fst ; cosnd = cofmap snd }

instance (Functor h, Copair f) => Copair (h `O` f) where
  cofst = inO (fmap cofst)
  cosnd = inO (fmap cosnd)

instance (Copair f, Copair g) => Copair (f :*: g) where
  cofst = inProd (cofst *** cofst)
  cosnd = inProd (cosnd *** cosnd)

-- | Pairing of 'Copair' values.  Combines contribution of each.
copair :: (Copair f, Monoid_f f) => PairTy f
fa `copair` fb = cofst fa `mappend_f` cosnd fb

-- Control.Applicative.Endo
-- Handy for "partial values" <http://haskell.org/haskellwiki/Partial>

instance Unpair Endo where  -- Parital == Endo
  pfst = inEndo $ (fst .) . (. (\ a -> (a, undefined)))
  psnd = inEndo $ (snd .) . (. (\ b -> (undefined, b)))

instance Copair Endo where  -- Parital == Endo
  cofst = inEndo first
  cosnd = inEndo second

-- Standard instance for (Monoid_f h, Copair h)
instance Pair Endo where pair = copair
