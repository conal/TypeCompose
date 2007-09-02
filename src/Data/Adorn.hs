{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}

----------------------------------------------------------------------
-- |
-- Module      :  Data.Adorn
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  multi-parameter type classes
-- 
-- Adorn with standard type constructors (or whatever you want) and unadorn.
----------------------------------------------------------------------

module Data.Adorn (Adorn(..)) where

import Control.Applicative
import Control.Arrow ((***))
import Data.Monoid (Endo(..))

-- | Adorn with standard type constructors (or whatever you want) and unadorn.
class Adorn  a b | b -> a where
  adorn   :: a -> b
  unadorn :: b -> a

-- Adorn functions.   Works on curried functions!
-- Coverage condition fails, hence -fallow-undecidable-instances
instance (Adorn a a', Adorn b b') => Adorn (a -> b) (a' -> b') where
  adorn   = (adorn   .) . (. unadorn)
  unadorn = (unadorn .) . (. adorn  )

instance (Adorn a a', Adorn b b') => Adorn (a,b) (a',b') where
  adorn   = adorn   *** adorn
  unadorn = unadorn *** unadorn

-- Would contradict the functional dependency, so use the following
-- instance as a template.
-- 
--   instance Adorn a a where { adorn = id; unadorn = id }
-- 
-- e.g.,

instance Adorn (IO a) (IO a) where { adorn = id; unadorn = id }

instance Adorn  b        (Const b a) where { adorn = Const ; unadorn = getConst }
instance Adorn  (a -> a) (Endo a)    where { adorn = Endo  ; unadorn = appEndo }