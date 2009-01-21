{-# LANGUAGE Rank2Types, TypeOperators, UndecidableInstances, CPP #-}
{-# OPTIONS_GHC -Wall #-}
#if __GLASGOW_HASKELL__ < 610
{-# OPTIONS_GHC -frewrite-rules #-}
#else
{-# OPTIONS_GHC -fenable-rewrite-rules #-}
#endif

----------------------------------------------------------------------
-- |
-- Module      :  Data.Zip
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  GHC
-- 
-- Zip-related type constructor classes.
-- 
-- This module is similar to @Control.Functor.Zip@ in the
-- @category-extras@ package, but it does not require a 'Functor'
-- superclass.
-- 
-- This module defines generalized 'zip' and 'unzip', so if you use it,
-- you'll have to
--
-- @
--    import Prelude hiding (zip,zipWith,zipWith3,unzip)
-- @
----------------------------------------------------------------------

module Data.Zip
  (
  -- * Zippings
    ZipTy, Zip(..), zipWith, zipWith3
  , apZip, ppZip, arZip
  -- * Unzipings
  , UnzipTy, Unzip(..)
  -- * Dual unzipings
  , Cozip(..), cozip
  -- * Misc
  , pairEdit, pairEditM
  ) where


import Prelude hiding (zip,zipWith,zipWith3,unzip)
import qualified Prelude

import Data.Monoid
import Control.Arrow
import Control.Applicative
import Control.Monad                    -- for pairEdit

import Control.Compose


{----------------------------------------------------------
    Zippings
----------------------------------------------------------}

-- | Type of 'zip' method
type ZipTy f = forall a b. f a -> f b -> f (a,b)

-- | Type constructor class for 'zip'-like things.
-- Here are some standard instance templates you can fill in.  They're not
-- defined in the general forms below, because they would lead to a lot of
-- overlap.
-- 
-- >    instance Applicative f => Zip f where
-- >        zip = liftA2 (,)
-- >    instance (Applicative h, Zip f) => Zip (h :. f) where
-- >        zip = apZip
-- >    instance (Functor g, Zip g, Zip f) => Zip (g :. f)
-- >        where zip = ppZip
-- >    instance (Arrow (~>), Unzip f, Zip g) => Zip (Arrw (~>) f g) where
-- >        zip = arZip
-- >    instance (Monoid_f h, Cozip h) => Zip h where
-- >        zip = cozip
-- 
-- Also, if you have a type constructor that's a 'Functor' and a 'Zip',
-- here is a way to define '(<*>)' for 'Applicative':
-- 
-- >    (<*>) = zipWith ($)
-- 
-- Minimum definitions for instances.

class Zip f where
  zip :: ZipTy f                   -- ^ Generalized 'Prelude.zip'

-- Oh yeah!  I don't want Zip to assume Functor.  See
-- <http://conal.net/blog/posts/more-beautiful-fold-zipping/>.

-- | Generalized 'Prelude.zipWith'
zipWith  :: (Functor f, Zip f) =>
            (a -> b -> c)
         -> (f a -> f b -> f c)
zipWith h = (fmap.fmap.fmap) (uncurry h) zip

-- | Generalized 'Prelude.zipWith'
zipWith3 :: (Functor f, Zip f) =>
            (a -> b -> c -> d)
         -> (f a -> f b -> f c -> f d)
zipWith3 h as bs cs = zipWith (uncurry h) (as `zip` bs) cs


{-# RULES
"zipWith  prelude" zipWith  = Prelude.zipWith
"zipWith3 prelude" zipWith3 = Prelude.zipWith3
  #-}

-- Standard instances (Applicative f)
instance             Zip []       where zip = Prelude.zip
instance Monoid u => Zip ((,)  u) where zip = liftA2 (,)
instance             Zip ((->) u) where zip = liftA2 (,)
instance             Zip IO       where zip = liftA2 (,)

instance Monoid o => Zip (Const o) where
  zip = inConst2 mappend

instance Zip Id where Id a `zip` Id b = Id (a,b)

-- Standard instance, e.g., (~>) = (->)
-- This one requires UndecidableInstances.  Alternatively, specialize to
-- (->) and other arrows as desired.
instance (Arrow (~>), Monoid_f (Flip (~>) o)) =>
  Zip (Flip (~>) o) where zip = cozip

-- | Handy for 'Zip' instances
apZip :: (Applicative h, Zip f) => ZipTy (h :. f)
apZip = inO2 (liftA2 zip)

-- | Handy for 'Zip' instances
ppZip :: (Functor g, Zip g, Zip f) => ZipTy (g :. f)
ppZip = inO2 $ \ gfa gfb -> uncurry zip <$> (gfa `zip` gfb)

-- | Ziping of 'Arrw' values.  /Warning/: definition uses 'arr', so only
-- use if your arrow has a working 'arr'.
arZip :: (Arrow (~>), Unzip f, Zip g) => ZipTy (Arrw (~>) f g)
arZip = inArrw2 $ \ fga fgb ->
  arr unzip >>> fga***fgb >>> arr (uncurry zip)

-- Standard instance
instance (Arrow (~>), Unzip f, Zip g) => Zip (Arrw (~>) f g)
  where zip = arZip

instance (Zip f, Zip g) => Zip (f :*: g) where
  zip = inProd2 (zip ***# zip)


{----------------------------------------------------------
    Unzipings
----------------------------------------------------------}

-- | Type of 'unzip' method.  Generalizes 'Prelude.unzip'.
type UnzipTy f = forall a b. f (a,b) -> (f a, f b)

-- | Unzippable.  Minimal instance definition: either (a) 'unzip' /or/ (b)
-- both of 'fsts' /and/ 'snds'.  A standard template to substitute any
-- 'Functor' @f.@ But watch out for effects!
-- 
-- >     instance Functor f => Unzip f where {fsts = fmap fst; snds = fmap snd}

class Unzip f where
  unzip :: UnzipTy f                    -- ^ generalized unzip
  fsts   :: f (a,b) -> f a              -- ^ First part of pair-like value
  snds   :: f (a,b) -> f b              -- ^ Second part of pair-like value

  unzip = fsts &&& snds
  fsts   = fst.unzip
  snds   = snd.unzip

instance Unzip [] where
  unzip = Prelude.unzip       -- single pass. don't use default
  fsts  = fmap fst
  snds  = fmap snd 

-- Some standard instances for functors
instance Unzip ((->) a)  where { fsts = fmap fst; snds = fmap snd }
instance Unzip ((,)  a)  where { fsts = fmap fst; snds = fmap snd }
instance Unzip (Const a) where { fsts = fmap fst; snds = fmap snd }
instance Unzip Id        where { fsts = fmap fst; snds = fmap snd }


{----------------------------------------------------------
    Dual unzipings
----------------------------------------------------------}

-- | Dual to 'Unzip'.
-- Especially handy for contravariant functors ('Cofunctor') .  Use this
-- template (filling in @f@) :
-- 
-- 
-- >    instance Cofunctor f => Cozip f where
-- >      { cofsts = cofmap fst ; cosnds = cofmap snd }

class Cozip f where
  cofsts :: f a -> f (a,b)               -- ^ Zip-like value from first part
  cosnds :: f b -> f (a,b)               -- ^ Zip-like value from second part

instance Cozip (Const e) where
  cofsts = inConst id
  cosnds = inConst id

-- Standard instance for contravariant functors
instance Arrow (~>) => Cozip (Flip (~>) o) where
  { cofsts = cofmap fst ; cosnds = cofmap snd }

instance (Functor h, Cozip f) => Cozip (h :. f) where
  cofsts = inO (fmap cofsts)
  cosnds = inO (fmap cosnds)

instance (Cozip f, Cozip g) => Cozip (f :*: g) where
  cofsts = inProd (cofsts *** cofsts)
  cosnds = inProd (cosnds *** cosnds)

-- | Ziping of 'Cozip' values.  Combines contribution of each.
cozip :: (Cozip f, Monoid_f f) => ZipTy f
fa `cozip` fb = cofsts fa `mappend_f` cosnds fb

-- Control.Applicative.Endo
-- Handy for "partial values" <http://haskell.org/haskellwiki/Partial>

instance Unzip Endo where  -- Parital == Endo
  fsts = inEndo $ (fst .) . (. (\ a -> (a, undefined)))
  snds = inEndo $ (snd .) . (. (\ b -> (undefined, b)))

instance Cozip Endo where  -- Parital == Endo
  cofsts = inEndo first
  cosnds = inEndo second

-- Standard instance for (Monoid_f h, Cozip h)
instance Zip Endo where zip = cozip



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
