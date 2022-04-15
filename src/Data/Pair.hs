{-# LANGUAGE FlexibleContexts, Rank2Types, TypeOperators, UndecidableInstances, CPP #-}
{-# OPTIONS_GHC -Wall #-}
#if __GLASGOW_HASKELL__ < 610
{-# OPTIONS_GHC -frewrite-rules #-}
#else
{-# OPTIONS_GHC -fenable-rewrite-rules #-}
#endif

----------------------------------------------------------------------
-- |
-- Module      :  Data.Pair
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  GHC
--
-- Pair-related type constructor classes.
--
-- This module is similar to @Control.Functor.Pair@ in the
-- @category-extras@ package, but it does not require a 'Functor'
-- superclass.
--
-- Temporarily, there is also Data.Zip, which contains the same
-- functionality with different naming.  I'm unsure which I prefer.
----------------------------------------------------------------------

module Data.Pair
  (
  -- * Pairpings
    PairTy, Pair(..)
  , apPair, ppPair, arPair
  -- * Unpairings
  , UnpairTy, Unpair(..)
  -- * Dual unpairings
  , Copair(..), copair
  -- * Misc
  , pairEdit, pairEditM
  ) where


import Data.Monoid
import Control.Arrow
import Control.Applicative
import Control.Monad                    -- for pairEdit

import Control.Compose


{----------------------------------------------------------
    Pairpings
----------------------------------------------------------}

-- | Type of 'pair' method
type PairTy f = forall a b. f a -> f b -> f (a,b)

-- | Type constructor class for 'pair'-like things.
-- Here are some standard instance templates you can fill in.  They're not
-- defined in the general forms below, because they would lead to a lot of
-- overlap.
--
-- >    instance Applicative f => Pair f where
-- >        pair = liftA2 (,)
-- >    instance (Applicative h, Pair f) => Pair (h :. f) where
-- >        pair = apPair
-- >    instance (Functor g, Pair g, Pair f) => Pair (g :. f)
-- >        where pair = ppPair
-- >    instance (Arrow (~>), Unpair f, Pair g) => Pair (Arrw (~>) f g) where
-- >        pair = arPair
-- >    instance (Monoid_f h, Copair h) => Pair h where
-- >        pair = copair
--
-- Also, if you have a type constructor that's a 'Functor' and a 'Pair',
-- here is a way to define '(<*>)' for 'Applicative':
--
-- >    (<*>) = pairWith ($)
--
-- Minimum definitions for instances.

class Pair f where
  pair :: PairTy f                   -- ^ Generalized 'pair'

-- Standard instances (Applicative f)
instance             Pair []       where pair = zip
instance Monoid u => Pair ((,)  u) where pair = liftA2 (,)
instance             Pair ((->) u) where pair = liftA2 (,)
instance             Pair IO       where pair = liftA2 (,)

instance Monoid o => Pair (Const o) where
  pair = inConst2 mappend

instance Pair Id where Id a `pair` Id b = Id (a,b)

-- Standard instance, e.g., (~>) = (->)
-- This one requires UndecidableInstances.  Alternatively, specialize to
-- (->) and other arrows as desired.
instance (Arrow j, Monoid_f (Flip j o)) =>
  Pair (Flip j o) where pair = copair

-- | Handy for 'Pair' instances
apPair :: (Applicative h, Pair f) => PairTy (h :. f)
apPair = inO2 (liftA2 pair)

-- | Handy for 'Pair' instances
ppPair :: (Functor g, Pair g, Pair f) => PairTy (g :. f)
ppPair = inO2 $ \ gfa gfb -> uncurry pair <$> (gfa `pair` gfb)

-- | Pairing of 'Arrw' values.  /Warning/: definition uses 'arr', so only
-- use if your arrow has a working 'arr'.
arPair :: (Arrow j, Unpair f, Pair g) => PairTy (Arrw j f g)
arPair = inArrw2 $ \ fga fgb ->
  arr unpair >>> fga***fgb >>> arr (uncurry pair)

-- Standard instance
instance (Arrow j, Unpair f, Pair g) => Pair (Arrw j f g)
  where pair = arPair

instance (Pair f, Pair g) => Pair (f :*: g) where
  pair = inProd2 (pair ***# pair)


{----------------------------------------------------------
    Unpairings
----------------------------------------------------------}

-- | Type of 'unpair' method.  Generalizes 'unpair'.
type UnpairTy f = forall a b. f (a,b) -> (f a, f b)

-- | Unpairpable.  Minimal instance definition: either (a) 'unpair' /or/ (b)
-- both of 'fsts' /and/ 'snds'.  A standard template to substitute any
-- 'Functor' @f.@ But watch out for effects!
--
-- >     instance Functor f => Unpair f where {fsts = fmap fst; snds = fmap snd}

class Unpair f where
  unpair :: UnpairTy f                    -- ^ generalized unpair
  fsts   :: f (a,b) -> f a              -- ^ First part of pair-like value
  snds   :: f (a,b) -> f b              -- ^ Second part of pair-like value

  unpair = fsts &&& snds
  fsts   = fst.unpair
  snds   = snd.unpair

instance Unpair [] where
  unpair = unzip       -- single pass. don't use default
  fsts  = fmap fst
  snds  = fmap snd

-- Some standard instances for functors
instance Unpair ((->) a)  where { fsts = fmap fst; snds = fmap snd }
instance Unpair ((,)  a)  where { fsts = fmap fst; snds = fmap snd }
instance Unpair (Const a) where { fsts = fmap fst; snds = fmap snd }
instance Unpair Id        where { fsts = fmap fst; snds = fmap snd }


{----------------------------------------------------------
    Dual unpairings
----------------------------------------------------------}

-- | Dual to 'Unpair'.
-- Especially handy for contravariant functors ('ContraFunctor') .  Use this
-- template (filling in @f@) :
--
--
-- >    instance ContraFunctor f => Copair f where
-- >      { cofsts = cofmap fst ; cosnds = cofmap snd }

class Copair f where
  cofsts :: f a -> f (a,b)               -- ^ Pair-like value from first part
  cosnds :: f b -> f (a,b)               -- ^ Pair-like value from second part

instance Copair (Const e) where
  cofsts = inConst id
  cosnds = inConst id

-- Standard instance for contravariant functors
instance Arrow j => Copair (Flip j o) where
  { cofsts = contraFmap fst ; cosnds = contraFmap snd }

instance (Functor h, Copair f) => Copair (h :. f) where
  cofsts = inO (fmap cofsts)
  cosnds = inO (fmap cosnds)

instance (Copair f, Copair g) => Copair (f :*: g) where
  cofsts = inProd (cofsts *** cofsts)
  cosnds = inProd (cosnds *** cosnds)

-- | Pairing of 'Copair' values.  Combines contribution of each.
copair :: (Copair f, Monoid_f f) => PairTy f
fa `copair` fb = cofsts fa `mappend_f` cosnds fb

-- Control.Applicative.Endo
-- Handy for "partial values" <http://haskell.org/haskellwiki/Partial>

instance Unpair Endo where  -- Parital == Endo
  fsts = inEndo $ (fst .) . (. (\ a -> (a, undefined)))
  snds = inEndo $ (snd .) . (. (\ b -> (undefined, b)))

instance Copair Endo where  -- Parital == Endo
  cofsts = inEndo first
  cosnds = inEndo second

-- Standard instance for (Monoid_f h, Copair h)
instance Pair Endo where pair = copair



{----------------------------------------------------------
    Misc
----------------------------------------------------------}

-- | Turn a pair of sources into a source of pair-editors.  See
-- <http://conal.net/blog/posts/pairs-sums-and-reactivity/>.
-- 'Functor'\/'Monoid' version.  See also 'pairEditM'.

pairEdit :: (Functor m, Monoid (m ((c,d) -> (c,d)))) =>
            (m c,m d) -> m ((c,d) -> (c,d))
pairEdit (ce,de) =
  fmap (first.const) ce `mappend` fmap (second.const) de


-- | Turn a pair of sources into a source of pair-editors.  See
-- <http://conal.net/blog/posts/pairs-sums-and-reactivity/>.
-- Monad version.  See also 'pairEdit'.
pairEditM :: MonadPlus m => (m c,m d) -> m ((c,d) -> (c,d))
pairEditM (ce,de) =
  liftM (first.const) ce `mplus` liftM (second.const) de
