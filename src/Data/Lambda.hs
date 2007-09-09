{-# LANGUAGE Rank2Types, TypeOperators, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

----------------------------------------------------------------------
-- |
-- Module      :  Data.Lambda
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  multi-parameter type classes
-- 
-- Some function-like classes, having lambda-like construction.
-- See 'LambdaTy' for why "lambda".
-- See "Data.Pair" for similar classes.
----------------------------------------------------------------------

module Data.Lambda
  (
  -- * Make function-like things
    LambdaTy
  , Lambda(..)
  -- * Dissect function-like things
  , Unlambda(..)
  -- * Dual dissections
  , Colambda(..)
  ) where

import Data.Monoid (Endo)
import Control.Applicative
import Control.Arrow

import Control.Compose
import Data.Bijection

{----------------------------------------------------------
    Make function-like things
----------------------------------------------------------}

-- | Type of 'lambda' method.  Think of @dom@ as the bound variable (or
-- pattern) part of a lambda and @ran@ as the expression part.  They
-- combine to form a function-typed expression. 
-- Instance template:
-- 
-- @
--   instance (Applicative f, Lambda dom ran)
--     => Lambda (f `O` dom) (f `O` ran) where
--       lambda = apLambda
-- @
type LambdaTy dom ran = forall a b. dom a -> ran b -> ran (a -> b)

-- | Type constructor class for function-like things having lambda-like construction.
class Lambda dom ran where
  lambda :: LambdaTy dom ran            -- ^ Form a function-like value

-- | Handy for 'Applicative' functor instances of 'Lambda'
apLambda :: (Applicative f, Lambda dom ran) => LambdaTy (f `O` dom) (f `O` ran)
apLambda = inO2 (liftA2 lambda)

-- Helper
apLambda' :: Applicative f => f a -> (f b -> o) -> (f (a->b) -> o)
apLambda' a bo ab = bo (ab <*> a)

---- Other instances

instance Lambda Id (Flip (->) o) where
  lambda (Id a) (Flip bo) = Flip (\ ab -> bo (ab a))

instance Lambda IO OI where
  lambda geta (Flip snkb) = Flip (\ f -> fmap f geta >>= snkb)

-- f a & f (b -> o)
instance Applicative f => Lambda f (f `O` Flip (->) o) where
  -- Map f a -> (f `O` Id) a, and appeal to the O/O and Id/Flip instances
  lambda dom ran = apLambda (O (fmap Id dom)) ran

-- f a & (f b -> o)
instance Applicative f => Lambda f (Flip (->) o `O` f) where
  lambda a b = biTo bi (apLambda' a (biFrom bi b))
   where
     bi = coconvO biFlip idb

-- Different wrapping of above
instance Applicative f => Lambda f (f :->: Const o) where
  lambda a b = biTo bi (apLambda' a (biFrom bi b))
   where
     bi = convFun idb biConst

instance (Lambda dom ran, Lambda dom' ran')
  => Lambda (dom :*: dom') (ran :*: ran') where
    lambda = inProd2 (lambda ***# lambda)

-- | 'lambda' with 'Arrw'.  /Warning/: definition uses 'arr', so only
-- use if your arrow has a working 'arr'.
arLambda :: (Arrow (~>), Unlambda f f', Lambda g g')
      => LambdaTy (Arrw (~>) f g) (Arrw (~>) f' g')
arLambda = inArrw2 $ \ fga fgb ->
  arr unlambda >>> fga***fgb >>> arr (uncurry lambda)

instance (Arrow (~>), Unlambda f f', Lambda g g')
    => Lambda (Arrw (~>) f g) (Arrw (~>) f' g')
  where lambda = arLambda


{----------------------------------------------------------
    Dissect function-like things
----------------------------------------------------------}

-- | Like @Unpair@, but for functions.  Minimal instance definition: either (a)
-- 'unlambda' /or/ (b) both of 'fsrc' /and/ 'fres'.
class Unlambda dom ran | ran -> dom where
  -- | Deconstruct pair-like value
  unlambda :: ran (a -> b) -> (dom a, ran b)
  -- | First part of pair-like value
  fsrc     :: ran (a -> b) -> dom a
  -- | Second part of pair-like value
  fres     :: ran (a -> b) -> ran b
  unlambda = fsrc &&& fres
  fsrc     = fst.unlambda
  fres     = snd.unlambda


{----------------------------------------------------------
    Dual dissections
----------------------------------------------------------}

-- | Like @Copair@, but for functions
class Colambda f where
  cores :: f b -> f (a -> b)

-- Handy for partial values <http://haskell.org/haskellwiki/Partial>
instance Unlambda Endo Endo where
  fres = inEndo $ (($) undefined) . (. const)

instance Colambda Endo where
  cores = inEndo (.)

