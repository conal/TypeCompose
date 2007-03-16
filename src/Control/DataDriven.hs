{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------
-- |
-- Module      :  Control.DataDriven
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Data-driven computations
----------------------------------------------------------------------

module Control.DataDriven
  (
  -- * Plumbing for \"events\" and subscription 
    Sink, Updater, News
  -- * Data-driven computations
  , DataDrivenG, dd, mapSrc
  , DataDriven, runDD, joinDD
  ) where

import Control.Applicative
import Control.Monad (join)
import Control.Arrow (second)

import Data.Monoid

import Control.Compose


{----------------------------------------------------------
    Plumbing for event publishing
----------------------------------------------------------}

-- | Sinks (consumers) of values
type Sink src a = a -> Updater src

-- | Updaters (actions)
type Updater src = src ()

-- | News publisher -- somewhere to register updaters to be executed
-- when events occur.
type News src = Sink src (Updater src)


{----------------------------------------------------------
    Data-driven computations
----------------------------------------------------------}

-- | The general type of data-driven computations.  Represented as a
-- /news/ publisher (@news@) and a source of new values (@src@).  Clients
-- interested in the value subscribe to @news@ and extract a new value
-- from @src@ when notified that the value may have changed.  When @news@
-- is a monoid and @src@ is an applicative functor, @DataDriven news src@
-- is an applicative functor also.  The applicative property is very
-- convenient for composition.  See the more specific type 'DataDriven'.

type DataDrivenG news src = Compose ((,) news) src

-- | Construct a data-driven computation from a subscription service
-- (@Monoid@) and a value source subscriber (@Applicative@).
dd :: news -> src a -> DataDrivenG news src a
dd = curry Comp

-- | Modify the source part of a 'DataDriven' computation.
mapSrc :: (src a -> src b) -> (DataDrivenG news src a -> DataDrivenG news src b)
mapSrc f = onComp (second f)


-- | Data driven with news publisher
type DataDriven src = DataDrivenG (News src) src


-- | Run a unit-valued 'DataDriven' computation.  Causes the source to be
-- executed /and/ registered with the subscriber.
runDD :: (Monoid (Updater src), Applicative src)
      => DataDriven src () -> Updater src
runDD (Comp (news,src)) = news src `mappend` src

-- | Apply 'join' to a source
joinDD :: Monad src => DataDriven src (src a) -> DataDriven src a
joinDD = mapSrc join

-- runDDJoin :: (Monad src, Applicative src, Monoid (Updater src))
--           => DataDriven src (Updater src) -> Updater src
-- runDDJoin = runDD . joinDD

