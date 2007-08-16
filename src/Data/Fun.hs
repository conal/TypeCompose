{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------
-- |
-- Module      :  Data.Fun
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  multi-parameter type classes
-- 
-- Some function-related classes.  See "Data.Pair" for similar classes.
----------------------------------------------------------------------

module Data.Fun (FunTy, Fun(..), Unfun(..), Cofun(..)) where


import Data.Monoid (Endo)
import Control.Applicative (Applicative,liftA2)
import Control.Arrow

import Control.Compose

-- | Type of 'fun' method
type FunTy dom ran = forall a b. dom a -> ran b -> ran (a -> b)

-- | Type constructor class for function-like things.
class Fun dom ran where fun :: FunTy dom ran

instance Fun Id (Flip (->) o) where
  fun (Id a) (Flip bo) = Flip (\ ab -> bo (ab a))


-- | Applicative functor version 
apFun :: (Applicative f, Fun dom ran) => FunTy (f `O` dom) (f `O` ran)
apFun = inO2 (liftA2 fun)

-- Template for standard Fun instance.  Instantiate at will.  The general
-- rule conflicts.

-- instance (Applicative f, Fun dom ran) => Fun (f `O` dom) (f `O` ran) where
--     fun = apFun

instance Applicative f => Fun f (f `O` Flip (->) o) where
  -- Map f a -> (f `O` Id) a, and appeal to the O/O and Id/Flip instances
  fun dom ran = apFun (O (fmap Id dom)) ran


instance (Fun dom ran, Fun dom' ran')
  => Fun (dom :*: dom') (ran :*: ran') where
    fun = inProd2 (fun ***# fun)

-- -- | 'fun' with 'Arrw'.  /Warning/: definition uses 'arr', so only
-- -- use if your arrow has a working 'arr'.
arFun :: (Arrow (~>), Unfun f f', Fun g g')
      => FunTy (Arrw (~>) f g) (Arrw (~>) f' g')
arFun = inArrw2 $ \ fga fgb ->
  arr unfun >>> fga***fgb >>> arr (uncurry fun)

instance (Arrow (~>), Unfun f f', Fun g g') => Fun (Arrw (~>) f g) (Arrw (~>) f' g')
  where fun = arFun


-- | Like @Unpair@, but for functions.  Minimal instance definition: either (a)
-- 'unfun' /or/ (b) both of 'fsrc' /and/ 'fres'.

-- I'm unsure about this fundep.  It lets the unfun & fres defaults type-check.
class Unfun dom ran | ran -> dom where
  unfun :: ran (a -> b) -> (dom a, ran b)
  fsrc  :: ran (a -> b) -> dom a
  fres  :: ran (a -> b) -> ran b
  unfun = fsrc &&& fres
  fsrc  = fst.unfun
  fres  = snd.unfun

-- | Like @Copair@, but for functions
class Cofun f where
  cores :: f b -> f (a -> b)

instance Unfun Endo Endo where
  fres = inEndo $ (($) undefined) . (. const)

instance Cofun Endo where
  cores = inEndo (.)

