{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------
-- |
-- Module      :  Control.Compose
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Various type constructor compositions and instances for them.
-- References:
-- [1] \"Applicative Programming with Effects\"
-- <http://www.soi.city.ac.uk/~ross/papers/Applicative.html>
----------------------------------------------------------------------

module Control.Compose
  ( Cofunctor(..)
  , O(..), inO
  , StaticArrow(..)
  , Flip(..), inFlip, inFlip2
  , ArrowAp(..)
  , App(..)
  ) where

import Control.Applicative
import Control.Arrow hiding (pure)
import Data.Monoid

-- | Often useful for /acceptors/ (consumers, sinks) of values.
class Cofunctor acc where
  cofmap :: (a -> b) -> (acc b -> acc a)

-- | Composition of type constructors: unary & unary.  Called \"@g . f@\" in
-- [1], section 5, but GHC won't parse that, nor will it parse any infix
-- type operators in an export list.  Haddock won't parse any type infixes
-- at all.  Meant to be used infix when Haddock is up to it or not involved.
newtype O g f a = O { unO :: g (f a) }

-- | Apply a function within the 'O' constructor.
inO :: (g (f a) -> g' (f' a')) -> ((O g f) a -> (O g' f') a')
inO h = O . h . unO

instance (Functor g, Functor f) => Functor (O g f) where
  fmap h (O gf) = O (fmap (fmap h) gf)

instance (Applicative g, Applicative f) => Applicative (O g f) where
  pure x            = O (pure (pure x))
  O getf <*> O getx = O (liftA2 (<*>) getf getx)

-- instance (Functor g, Cofunctor f) => Cofunctor (O g f) where
--   cofmap h (O gf) = O (fmap (cofmap h) gf)

-- Or this alternative.  Having both yields "Duplicate instance
-- declarations".
instance (Cofunctor g, Functor f) => Cofunctor (O g f) where
  cofmap h (O gf) = O (cofmap (fmap h) gf)

-- We can also make functors by composing /cofunctors/.  GHC would
-- consider such a declaration to be in conflict with the the
-- composition-of-functors instance, because it doesn't take contexts into
-- account.  Too bad.

-- standard Monoid instance for Applicative applied to Monoid
instance (Applicative (O g f), Monoid a) => Monoid (O g f a) where
  { mempty = pure mempty; mappend = (*>) }

-- | Composition of type constructors: unary with binary.
newtype StaticArrow f (~>) a b = Static { unStatic :: f (a ~> b) }

instance (Applicative f, Arrow (~>)) => Arrow (StaticArrow f (~>)) where
  arr                   = Static . pure . arr
  Static g >>> Static h = Static (liftA2 (>>>) g h)
  first (Static g)      = Static (liftA first g)

-- For instance, /\ a b. f (a -> m b) =~ StaticArrow f Kleisli m


-- | Composition of type constructors: binary with unary.

newtype ArrowAp (~>) f a b = ArrowAp {unArrowAp :: f a ~> f b}

instance (Arrow (~>), Applicative f) => Arrow (ArrowAp (~>) f) where
  arr                     = ArrowAp . arr . liftA
  ArrowAp g >>> ArrowAp h = ArrowAp (g >>> h)
  first (ArrowAp a)       =
    ArrowAp (arr splitA >>> first a >>> arr mergeA)

instance (ArrowLoop (~>), Applicative f) => ArrowLoop (ArrowAp (~>) f) where
  -- loop :: UI (b,d) (c,d) -> UI b c
  loop (ArrowAp k) =
    ArrowAp (loop (arr mergeA >>> k >>> arr splitA))

-- Wolfgang Jeltsch pointed out a problem with these definitions: 'splitA'
-- and 'mergeA' are not inverses.  The definition of 'first', e.g.,
-- violates the \"extension\" law and causes repeated execution.  Look for
-- a reformulation or a clarification of required properties of the
-- applicative functor @f@.
-- 
-- See also "Arrows and Computation", which notes that the following type
-- is "almost an arrow" (http://www.soi.city.ac.uk/~ross/papers/fop.html).
-- 
-- > newtype ListMap i o = LM ([i] -> [o])

mergeA :: Applicative f => (f a, f b) -> f (a,b)
mergeA ~(fa,fb) = liftA2 (,) fa fb

splitA :: Applicative f => f (a,b) -> (f a, f b)
splitA fab = (liftA fst fab, liftA snd fab)


-- | Flip type arguments
newtype Flip (~>) b a = Flip { unFlip :: a ~> b }

inFlip :: ((a~>b) -> (a' ~~> b')) -> (Flip (~>) b a -> Flip (~~>) b' a')
inFlip f (Flip ar) = Flip (f ar)

inFlip2 :: ((a~>b) -> (a' ~~> b') -> (a'' ~~~> b''))
        -> (Flip (~>) b a -> Flip (~~>) b' a' -> Flip (~~~>) b'' a'')
inFlip2 f (Flip ar) (Flip ar') = Flip (f ar ar')

instance Arrow (~>) => Cofunctor (Flip (~>) b) where
  cofmap h (Flip f) = Flip (arr h >>> f)

-- | Type application
newtype App f a = App { unApp :: f a }

-- Example: App IO ()
instance (Applicative f, Monoid m) => Monoid (App f m) where
  mempty = App (pure mempty)
  App a `mappend` App b = App (a *> b)

{-
-- We can also drop the App constructor, but then we overlap with many
-- other instances, like [a].
instance (Applicative f, Monoid a) => Monoid (f a) where
  mempty = pure mempty
  mappend = (*>)
-}
