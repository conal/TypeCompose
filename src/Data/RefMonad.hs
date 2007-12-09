{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-- -- For ghc 6.6 compatibility
-- {-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------
-- |
-- Module      :  Data.RefMonad
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  MPTC
-- 
-- Monads with references, taken from John Hughes's [Global].
-- 
--  [Global] <http://citeseer.ist.psu.edu/473734.html>
--           "Global Variables in Haskell"


----------------------------------------------------------------------

module Data.RefMonad (RefMonad(..), modifyRef) where

import Data.IORef       (IORef, newIORef, readIORef, writeIORef)
import Data.STRef       (STRef, newSTRef, readSTRef, writeSTRef)
import Control.Monad.ST (ST)

-------------------------------------------------------------------------------

-- | Class of monads with references.
class Monad m => RefMonad m r | m -> r, r -> m where
    newRef   :: a -> m (r a)
    readRef  :: r a -> m a
    writeRef :: r a -> a -> m ()

instance RefMonad IO IORef where
    newRef   =  newIORef
    readRef  =  readIORef
    writeRef =  writeIORef

instance RefMonad (ST s) (STRef s) where
    newRef   =  newSTRef
    readRef  =  readSTRef
    writeRef =  writeSTRef

-- | Change the contents of a ref
modifyRef :: RefMonad m r => r a -> (a -> a) -> m ()
modifyRef ref f = readRef ref >>= writeRef ref . f
