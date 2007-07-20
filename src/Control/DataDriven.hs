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
    Sink, Action, News
  -- * Data-driven computations
  , DataDrivenG, dd, mapCur
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
type Sink cur a = a -> Action cur

-- | Actions
type Action cur = cur ()

-- | News publisher -- somewhere to register updaters to be executed
-- when events occur.
type News cur = Sink cur (Action cur)


{----------------------------------------------------------
    Data-driven computations
----------------------------------------------------------}

-- | The general type of data-driven computations.  Represented as a
-- /news/ publisher (@news@) and a way to get new values (@cur@).  Clients
-- interested in the value subscribe to @news@ and extract a new value
-- from @cur@ when notified that the value may have changed.  When @news@
-- is a monoid and @cur@ is an applicative functor, @DataDrivenG news cur@
-- is an applicative functor also.  The applicative property is very
-- convenient for composition.  See the more specific type 'DataDriven'.
--
-- Nicer, but Haddock chokes on the infix op:
--   type DataDrivenG news cur = ((,) news) `O` cur

type DataDrivenG news cur = O ((,) news) cur

-- More tersely :
-- type DataDrivenG news = O ((,) news)

-- | Construct a data-driven computation from a subscription service
-- (@Monoid@) and a value source subscriber (@Applicative@).
dd :: cur a -> news -> DataDrivenG news cur a
dd = flip (curry O)

-- | Modify the source part of a 'DataDriven' computation.
mapCur :: (cur a -> cur b) -> (DataDrivenG news cur a -> DataDrivenG news cur b)
mapCur f = inO (second f)


-- | Data driven with news publisher
type DataDriven cur = DataDrivenG (News cur) cur


-- | Run a unit-valued 'DataDriven' computation.  Causes the source to be
-- executed /and/ registered with the subscriber.
runDD :: (Monoid (Action cur), Applicative cur)
      => DataDriven cur () -> Action cur
runDD (O (news,cur)) = news cur `mappend` cur

-- | Apply 'join' to a source
joinDD :: Monad cur => DataDriven cur (cur a) -> DataDriven cur a
joinDD = mapCur join

-- runDDJoin :: (Monad cur, Applicative cur, Monoid (Action cur))
--           => DataDriven cur (Action cur) -> Action cur
-- runDDJoin = runDD . joinDD

