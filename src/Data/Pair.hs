{-# OPTIONS -fglasgow-exts -cpp #-}

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
    PairTy, Pair(..)
  , UnpairTy, Unpair(..), Copair(..), copair
  , apPair, ppPair, arPair
  ) where


import Data.Monoid
import Control.Arrow
import Control.Applicative

import Control.Compose

-- | Type of 'pair' method
type PairTy f = forall a b. f a -> f b -> f (a,b)

-- | Type constructor class for pair-like things
class Pair f where pair :: PairTy f

instance Pair Id where
  Id a `pair` Id b = Id (a,b)

-- Schema: Substitute any Applicative f
-- 
-- instance Applicative f => Pair f where pair = liftA2 (,)

-- e.g.,

instance Monoid u => Pair ((,)  u) where pair = liftA2 (,)
instance             Pair ((->) u) where pair = liftA2 (,)
instance             Pair IO       where pair = liftA2 (,)


-- | Type of 'unpair' method.
type UnpairTy f = forall a b. f (a,b) -> (f a, f b)

-- | Dissectable as pairs.  Minimal instance definition: either (a)
-- 'unpair' /or/ (b) both of 'pfst' /and/ 'psnd'.
class Unpair f where
  unpair :: UnpairTy f
  pfst   :: f (a,b) -> f a
  psnd   :: f (a,b) -> f b

  unpair = pfst &&& psnd
  pfst   = fst.unpair
  psnd   = snd.unpair

instance Unpair (Const a) where
  unpair (Const a) = (Const a, Const a)

instance Unpair Id where
  unpair (Id (a,b)) = (Id a, Id b)


-- | Dual to 'Unpair'
class Copair f where
  cofst :: f a -> f (a,b)
  cosnd :: f b -> f (a,b)

instance Copair (Flip (->) o) where
  cofst = inFlip (. fst)
  cosnd = inFlip (. snd)

instance (Functor h, Copair f) => Copair (h `O` f) where
  cofst = inO (fmap cofst)
  cosnd = inO (fmap cosnd)

instance (Copair f, Copair g) => Copair (f :*: g) where
  cofst = inProd (cofst *** cofst)
  cosnd = inProd (cosnd *** cosnd)

-- | Pairing of 'Copair' values.  Combines contribution of each.
copair :: (Copair f, Monoid_f f) => PairTy f
fa `copair` fb = cofst fa `mappend_f` cosnd fb


-- Schema.  Fill in h.
-- 
-- instance (Monoid_f h, Copair h) => Pair h where pair = copair

instance Monoid o => Pair (Flip (->) o) where pair = copair


apPair :: (Applicative h, Pair f) => PairTy (h `O` f)
apPair = inO2 (liftA2 pair)

-- Schema.  Fill in h & f.
-- 
-- instance (Applicative h, Pair f) => Pair (h `O` f) where
--   pair = apPair


ppPair :: (Functor g, Pair g, Pair f) => PairTy (g `O` f)
ppPair = inO2 $ \ gfa gfb -> fmap (uncurry pair) (gfa `pair` gfb)

-- Schema.  Fill in h & f.
-- 
-- instance (Functor g, Pair g, Pair f) => Pair (g `O` f) where
--   pair = ppPair

-- | Pairing of 'Arrw' values.  /Warning/: definition uses 'arr', so only
-- use if your arrow has a working 'arr'.
arPair :: (Arrow (~>), Unpair f, Pair g) => PairTy (Arrw (~>) f g)
arPair = inArrw2 $ \ fga fgb ->
  arr unpair >>> fga***fgb >>> arr (uncurry pair)

instance (Arrow (~>), Unpair f, Pair g) => Pair (Arrw (~>) f g)
  where pair = arPair

instance (Pair f, Pair g) => Pair (f :*: g) where
  pair = inProd2 (pair ***# pair)



---- Control.Applicative.Endo

instance Unpair Endo where  -- Parital == Endo
  pfst = inEndo $ (fst .) . (. (\ a -> (a, undefined)))
  psnd = inEndo $ (snd .) . (. (\ b -> (undefined, b)))

instance Copair Endo where  -- Parital == Endo
  cofst = inEndo first
  cosnd = inEndo second

-- Standard instance for (Monoid_f h, Copair h)
instance Pair Endo where pair = copair
