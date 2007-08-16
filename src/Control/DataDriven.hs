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
    Action, Sink, News
  -- * Data-driven computations
  , SourceG, dd, mapCur
  , Source, runDD, joinDD, runDDJoin
  -- * IO specializations
  , Src, Act, Snk, Nws   -- sorry about these ones.  suggestions?
  ) where

import Control.Applicative
import Control.Monad (join)
import Control.Arrow (second)

import Data.Monoid

import Control.Compose


{----------------------------------------------------------
    Plumbing for event publishing
----------------------------------------------------------}

-- | Actions
type Action cur = cur ()

-- | Sinks (consumers) of values
type Sink cur a = a -> Action cur

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
-- is a monoid and @cur@ is an applicative functor, @SourceG news cur@
-- is an applicative functor also.  The applicative property is very
-- convenient for composition.  See the more specific type 'Source'.
--
-- Nicer, but Haddock chokes on the infix op:
--   type SourceG news cur = ((,) news) `O` cur

type SourceG news cur = O ((,) news) cur

-- Standard instance
instance Functor cur => Functor (SourceG news cur) where
  fmap h (O gf) = O (fmap (fmap h) gf)


-- More tersely :
-- type SourceG news = O ((,) news)

instance (Monoid news, Applicative cur, Monoid o)
          => Monoid (SourceG news cur o) where
  mempty  = pure mempty
  mappend = liftA2 mappend

-- Note no Monoid_f instance, because of the @Monoid o@ constraint.

-- | Construct a data-driven computation from a subscription service
-- (@Monoid@) and a value source subscriber (@Applicative@).
dd :: cur a -> news -> SourceG news cur a
dd = flip (curry O)

-- | Modify the source part of a 'Source' computation.
mapCur :: (cur a -> cur b) -> (SourceG news cur a -> SourceG news cur b)
mapCur f = inO (second f)


-- | Data driven with news publisher
type Source cur = SourceG (News cur) cur


-- | Run a unit-valued data-driven computation.  Causes the source to be
-- executed /and/ registered with the subscriber.  See 'runDDJoin' for a
-- convenient alternative.
runDD :: (Monoid (Action cur), Applicative cur)
      => Source cur () -> Action cur
runDD (O (news,cur)) = news cur `mappend` cur

-- | 'join' a data-driven computation
joinDD :: Monad cur => Source cur (cur a) -> Source cur a
joinDD = mapCur join

-- | Like 'runDD', but works one action-valued data-driven computations.
-- More convenient and more specialized.
runDDJoin :: (Monad cur, Applicative cur, Monoid (Action cur))
          => Source cur (Action cur) -> Action cur
runDDJoin = runDD . joinDD


---- IO specializations

-- | IO-based 'Source' computation
type Src = Source IO

-- | IO-based 'Action'
type Act = Action IO -- == IO ()

-- | IO-based 'Sink'
type Snk a = Sink IO a

-- | IO-based 'News' publisher
type Nws = News IO
