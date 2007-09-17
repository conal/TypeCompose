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

-- | Type of 'lambda' method.  Think of @src@ as the bound variable (or
-- pattern) part of a lambda and @snk@ as the expression part.  They
-- combine to form a function-typed expression. 
-- Instance template:
-- 
-- @
--   instance (Applicative f, Lambda src snk)
--     => Lambda (f `O` src) (f `O` snk) where
--       lambda = apLambda
-- @
type LambdaTy src snk = forall a b. src a -> snk b -> snk (a -> b)

-- | Type constructor class for function-like things having lambda-like construction.
class Lambda src snk where
  lambda :: LambdaTy src snk            -- ^ Form a function-like value

-- | Handy for 'Applicative' functor instances of 'Lambda'
apLambda :: (Applicative f, Lambda src snk) => LambdaTy (f `O` src) (f `O` snk)
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
  lambda src snk = apLambda (O (fmap Id src)) snk

-- f a & (f b -> o)
instance Applicative f => Lambda f (Flip (->) o `O` f) where
  lambda a b = biTo bi (apLambda' a (biFrom bi b))
   where
     bi = coconvO biFlip idb

-- Different wrapping of above
instance Applicative f => Lambda f (f :->: Const o) where
  lambda a b = biTo bi (apLambda' a (biFrom bi b))
   where
     bi = idb `convFun` biConst

instance (Lambda src snk, Lambda dom' ran')
  => Lambda (src :*: dom') (snk :*: ran') where
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
class Unlambda src snk | snk -> src where
  -- | Deconstruct pair-like value
  unlambda :: snk (a -> b) -> (src a, snk b)
  -- | First part of pair-like value
  fsrc     :: snk (a -> b) -> src a
  -- | Second part of pair-like value
  fres     :: snk (a -> b) -> snk b
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

