{-# OPTIONS #-}

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
import Control.Monad.Reader
import Control.Monad


-- Standard instance: Applicative functor applied to monoid
instance Monoid a => Monoid (IO a) where { mempty = pure mempty; mappend = (*>) }

-- standard Applicative instance for Monad
instance Monad m => Applicative (ReaderT r m) where { pure = return; (<*>) = ap }

