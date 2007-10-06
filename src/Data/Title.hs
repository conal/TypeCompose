-- {-# LANGUAGE FlexibleInstances, OverlappingInstances, TypeOperators, TypeSynonymInstances #-}
-- Temp, for ghc 6.6 compatibility
{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

----------------------------------------------------------------------
-- |
-- Module      :  Data.Title
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Titling class.  Used in [TV], but provided here, since I want packages
-- like [Phooey] to provide an instance without depending on TV.
-- 
--   [TV]:     <http://haskell.org/haskellwiki/TV>     "Tangible values"
--   [Phooey]: <http://haskell.org/haskellwiki/Phooey> "Functional UIs"
----------------------------------------------------------------------

module Data.Title (Title(..),Title_f(..)) where

import Control.Compose (Flip(..),inFlip,O,inO)

-- | Provide a title on a value.  If you can title polymorphically, please
-- instantiate 'Title_f' instead of Title.  Then you'll automatically
-- get a 'Title' for each type instance, thanks to this rule.
-- 
-- @
--   instance Title_f f => Title (f a) where title = title_f
-- @
-- 
-- To handle ambiguity for types like @([] Char)@ -- aka 'String', this
-- module is compiled with @OverlappingInstances@ and
-- @UndecidableInstances@.  The more specific instance (yours) wins.
-- 
-- In defining your instance, you might want to use the String instance,
-- e.g., @title ttl \"\"@.
class Title u where title :: String -> u -> u

-- Polymorphic version of 'Title'.  See 'Title' doc.
class Title_f f where
  -- | 'title' for all applications of @f@
  title_f :: String -> f a -> f a

instance Title_f g => Title_f (g `O` f) where title_f str = inO (title_f str)

instance Title_f f => Title (f a) where title = title_f

instance Title String where
  title ttl str = (ttl ++ suffix ++ str)
   where
     suffix | null ttl || final `elem` " \n" = ""
            | final `elem` ".?:"             = " "
            | otherwise                      = ": "
       where
         final = last ttl

instance Title_f IO where
  title_f ttl = (putStr (title ttl "") >> )

instance Title b => Title (a -> b) where
  title str f = title str . f

-- Combining the two previous instances
instance Title o => Title_f (Flip (->) o) where
  title_f str = inFlip (title str)

-- Equivalently,
-- 
--   title_f str (Flip snk) = Flip (title str snk)
