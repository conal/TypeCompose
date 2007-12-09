{-# OPTIONS_GHC -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  Control.Instances
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Some (orphan) instances that belong elsewhere (where they wouldn't be orphans).
-- Add the following line to get these instances
-- 
-- > import Control.Instances ()
-- 
----------------------------------------------------------------------

module Control.Instances () where

import Data.Monoid
import Control.Applicative


-- Standard instance: Applicative functor applied to monoid
instance Monoid o => Monoid (IO o) where 
  mempty  = pure   mempty
  mappend = liftA2 mappend
