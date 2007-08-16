{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

----------------------------------------------------------------------
-- |
-- Module      :  Control.Compose
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  infix type ops, undecidable instances
-- 
-- Various type constructor compositions and instances for them.
-- Some come from 
-- [1] \"Applicative Programming with Effects\"
-- <http://www.soi.city.ac.uk/~ross/papers/Applicative.html>
----------------------------------------------------------------------

module Control.Compose
  ( Cofunctor(..)
  , O(..), inO, inO2, inO3
  , fmapFF, fmapCC, cofmapFC, cofmapCF
  , OO(..)
  , Monoid_f(..)
  , Flip(..), inFlip, inFlip2, inFlip3
  , ArrowAp(..)
  , App(..)
  , Id(..)
  , (:*:)(..), (***#), ($*), inProd, inProd2, inProd3
  , Arrw(..), (:->:) , inArrw, inArrw2, inArrw3
  , inConst, inConst2, inConst3
  , inEndo
  ) where

import Control.Applicative
import Control.Arrow hiding (pure)
import Data.Monoid

-- import Test.QuickCheck -- for Endo

import Data.Adorn

infixl 9 `O`
infixl 7 :*:
infixr 1 :->:

infixl 0 $*
infixr 3 ***#


-- | Often useful for /acceptors/ (consumers, sinks) of values.
class Cofunctor acc where
  cofmap :: (a -> b) -> (acc b -> acc a)


{- |

Composition of unary type constructors

There are (at least) two useful 'Monoid' instances, so you'll have to
pick one and type-specialize it (filling in all or parts of @g@ and\/or @f@).

@
    -- standard Monoid instance for Applicative applied to Monoid
    instance (Applicative (O g f), Monoid a) => Monoid (O g f a) where
      { mempty = pure mempty; mappend = liftA2 mappend }
    -- Especially handy when g is a Monoid_f.
    instance Monoid (g (f a)) => Monoid (O g f a) where
      { mempty = O mempty; mappend = inO2 mappend }
@

Corresponding to the first and second definitions above,

@
    instance (Applicative g, Monoid_f f) => Monoid_f (O g f) where
      { mempty_f = O (pure mempty_f); mappend_f = inO2 (liftA2 mappend_f) }
    instance Monoid_f g => Monoid_f (O g f) where
      { mempty_f = O mempty_f; mappend_f = inO2 mappend_f }
@

Similarly, there are two useful 'Functor' instances and two useful
'Cofunctor' instances.  Mix & match as you like.

@
    instance (  Functor g,   Functor f) => Functor (O g f) where fmap = fmapFF
    instance (Cofunctor g, Cofunctor f) => Functor (O g f) where fmap = fmapCC

    instance (Functor g, Cofunctor f) => Cofunctor (O g f) where cofmap = cofmapFC
    instance (Cofunctor g, Functor f) => Cofunctor (O g f) where cofmap = cofmapCF
@

-}

-- TODO: change prefix O uses ("O g f") to infix ("g `O` f") throughout,
-- once I'm running on the new Haddock.

newtype (g `O` f) a = O { unO :: g (f a) }

instance (Functor h, Adorn b (f a)) => Adorn (h b) ((h `O` f) a) where
  adorn   = O . fmap adorn
  unadorn = fmap unadorn . unO 

-- | Apply a function within the 'O' constructor.
inO :: (g (f a) -> g' (f' a')) -> ((O g f) a -> (O g' f') a')
inO h = O . h . unO

inO2 :: (g (f a)   -> g' (f' a')   -> g'' (f'' a''))
     -> ((O g f) a -> (O g' f') a' -> (O g'' f'') a'')
inO2 h (O gfa) (O gfa') = O (h gfa gfa')

inO3 :: (g (f a)   -> g' (f' a')   -> g'' (f'' a'')   -> g''' (f''' a'''))
     -> ((O g f) a -> (O g' f') a' -> (O g'' f'') a'' -> (O g''' f''') a''')
inO3 h (O gfa) (O gfa') (O gfa'') = O (h gfa gfa' gfa'')

-- | Used for the Functor `O` Functor instance of Functor
fmapFF :: (  Functor g,   Functor f) => (a -> b) -> O g f a -> O g f b
fmapFF h = inO $ fmap (fmap h)

-- | Used for the Cofunctor `O` Cofunctor instance of Functor
fmapCC :: (Cofunctor g, Cofunctor f) => (a -> b) -> O g f a -> O g f b
fmapCC h = inO $ cofmap (cofmap h)

-- | Used for the Functor `O` Cofunctor instance of Functor
cofmapFC :: (Functor g, Cofunctor f) => (b -> a) -> O g f a -> O g f b
cofmapFC h (O gf) = O (fmap (cofmap h) gf)

-- | Used for the Cofunctor `O` Functor instance of Functor
cofmapCF :: (Cofunctor g, Functor f) => (b -> a) -> O g f a -> O g f b
cofmapCF h (O gf) = O (cofmap (fmap h) gf)


instance ( Functor (O g f)
         , Applicative g, Applicative f) => Applicative (O g f) where
  pure x            = O (pure (pure x))
  O getf <*> O getx = O (liftA2 (<*>) getf getx)




-- | Composition of type constructors: unary with binary.  Called
-- "StaticArrow" in [1].
newtype OO f (~>) a b = OO { unOO :: f (a ~> b) }

instance (Applicative f, Arrow (~>)) => Arrow (OO f (~>)) where
  arr           = OO . pure . arr
  OO g >>> OO h = OO (liftA2 (>>>) g h)
  first (OO g)  = OO (liftA first g)

-- For instance, /\ a b. f (a -> m b) =~ OO f Kleisli m


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


-- | Simulates universal constraint @forall a. Monoid (f a)@.
-- 
-- See Simulating Quantified Class Constraints
-- (<http://flint.cs.yale.edu/trifonov/papers/sqcc.pdf>)
--  Instantiate this schema wherever necessary:
-- 
--   instance Monoid_f f where { mempty_f = mempty ; mappend_f = mappend }
-- 
class Monoid_f m where
  mempty_f  :: forall a. m a
  mappend_f :: forall a. m a -> m a -> m a

-- instance Monoid_f g => Monoid_f (O g f) where
--   mempty_f  = mempty
--   mappend_f = mappend

--  e.g.,
instance Monoid_f [] where { mempty_f = mempty ; mappend_f = mappend }


-- instance Monoid (g (f a)) => Monoid (O g f a) where
--   mempty  = O mempty
--   mappend = inO2 mappend


-- | Flip type arguments
newtype Flip (~>) b a = Flip { unFlip :: a ~> b }

instance Adorn (a -> o) (Flip (->) o a) where
  adorn   = Flip
  unadorn = unFlip

-- Apply unary function inside of a 'Flip' representation.
inFlip :: ((a~>b) -> (a' ~~> b')) -> (Flip (~>) b a -> Flip (~~>) b' a')
inFlip f (Flip ar) = Flip (f ar)

-- Apply binary function inside of a 'Flip' representation.
inFlip2 :: ((a~>b) -> (a' ~~> b') -> (a'' ~~~> b''))
        -> (Flip (~>) b a -> Flip (~~>) b' a' -> Flip (~~~>) b'' a'')
inFlip2 f (Flip ar) (Flip ar') = Flip (f ar ar')

-- Apply ternary function inside of a 'Flip' representation.
inFlip3 :: ((a~>b) -> (a' ~~> b') -> (a'' ~~~> b'') -> (a''' ~~~~> b'''))
        -> (Flip (~>) b a -> Flip (~~>) b' a' -> Flip (~~~>) b'' a'' -> Flip (~~~~>) b''' a''')
inFlip3 f (Flip ar) (Flip ar') (Flip ar'') = Flip (f ar ar' ar'')

instance Arrow (~>) => Cofunctor (Flip (~>) b) where
  cofmap h (Flip f) = Flip (arr h >>> f)

-- Useful for (~>) = (->).  Maybe others.
instance (Applicative ((~>) a), Monoid o) => Monoid (Flip (~>) o a) where
  mempty  = Flip (pure mempty)
  mappend = inFlip2 (liftA2 mappend)

-- TODO: generalize (->) to (~>) with Applicative_f (~>)
instance Monoid o => Monoid_f (Flip (->) o) where
  { mempty_f = mempty ; mappend_f = mappend }

-- | Type application
newtype App f a = App { unApp :: f a }

-- Example: App IO ()
instance (Applicative f, Monoid m) => Monoid (App f m) where
  mempty = App (pure mempty)
  App a `mappend` App b = App (liftA2 mappend a b)


{-
-- We can also drop the App constructor, but then we overlap with many
-- other instances, like [a].
instance (Applicative f, Monoid a) => Monoid (f a) where
  mempty = pure mempty
  mappend = (*>)
-}


-- | Identity type constructor.  Until there's a better place to find it.
newtype Id a = Id { unId :: a }


-- | Pairing of type constructors
newtype (f :*: g) a = Prod { unProd :: (f a, g a) }

instance (Adorn b (f a), Adorn b' (g a)) => Adorn (b,b') ((f :*: g) a) where
    adorn   = Prod . (adorn *** adorn)
    unadorn = (unadorn *** unadorn) . unProd

-- | Apply unary function inside of @f :*: g@ representation.
inProd :: ((f a, g a) -> (f' a', g' a'))
       -> ((f :*: g) a -> (f' :*: g') a')
inProd   h (Prod p) = Prod (h p)

-- | Apply binary function inside of @f :*: g@ representation.
inProd2 :: ((f a, g a) -> (f' a', g' a') -> (f'' a'', g'' a''))
        -> ((f :*: g) a -> (f' :*: g') a' -> (f'' :*: g'') a'')
inProd2 h (Prod p) (Prod p') = Prod (h p p')

-- | Apply ternary function inside of @f :*: g@ representation.
inProd3 :: ((f a, g a) -> (f' a', g' a') -> (f'' a'', g'' a'')
                       -> (f''' a''', g''' a'''))
        -> ((f :*: g) a -> (f' :*: g') a' -> (f'' :*: g'') a''
                        -> (f''' :*: g''') a''')
inProd3 h (Prod p) (Prod p') (Prod p'') = Prod (h p p' p'')

($*) :: (a -> b, a' -> b') -> (a,a') -> (b,b')
($*) = uncurry (***)

(***#) :: (a -> b -> c) -> (a' -> b' -> c')
       -> (a, a') -> (b, b') -> (c, c')
h ***# h' = \ as bs -> (h,h') $* as $* bs
            -- (uncurry (***)) . (h *** h')
            -- \ as bs -> uncurry (***) ((h *** h') as) bs
            -- \ as bs -> (h *** h') as $* bs
            -- \ (a,a') (b,b') -> (h a b, h' a' b')

-- instance (Monoid a, Monoid b) => Monoid (a,b) where
-- 	mempty = (mempty, mempty)
-- 	mappend = mappend ***# mappend

instance (Monoid_f f, Monoid_f g) => Monoid_f (f :*: g) where
  mempty_f  = Prod (mempty_f,mempty_f)
  mappend_f = inProd2 (mappend_f ***# mappend_f)


-- | Arrow-like type between type constructors (doesn't enforce @Arrow
-- (~>)@ here).
newtype Arrw (~>) f g a = Arrw { unArrw :: f a ~> g a }

-- Coverage condition fails, hence -fallow-undecidable-instances
instance (Arrow (~>), Adorn b (f a), Adorn c (g a))
  => Adorn (b ~> c) (Arrw (~>) f g a) where
    adorn   = Arrw . ((arr unadorn >>>) . (>>> arr adorn))
    unadorn = ((arr adorn >>>) . (>>> arr unadorn)) . unArrw

-- | Apply unary function inside of @f :*: g@ representation.
inArrw :: ((f a ~> g a) -> (f' a' ~> g' a'))
       -> ((Arrw (~>) f g) a -> (Arrw (~>) f' g') a')
inArrw   h (Arrw p) = Arrw (h p)

-- | Apply binary function inside of @Arrw (~>) f g@ representation.
inArrw2 :: ((f a ~> g a) -> (f' a' ~> g' a') -> (f'' a'' ~> g'' a''))
        -> (Arrw (~>) f g a -> Arrw (~>) f' g' a' -> Arrw (~>) f'' g'' a'')
inArrw2 h (Arrw p) (Arrw p') = Arrw (h p p')

-- | Apply ternary function inside of @Arrw (~>) f g@ representation.
inArrw3 :: ((f a ~> g a) -> (f' a' ~> g' a') -> (f'' a'' ~> g'' a'') -> (f''' a''' ~> g''' a'''))
        -> ((Arrw (~>) f g) a -> (Arrw (~>) f' g') a' -> (Arrw (~>) f'' g'') a'' -> (Arrw (~>) f''' g''') a''')
inArrw3 h (Arrw p) (Arrw p') (Arrw p'') = Arrw (h p p' p'')

{-

  fga :: f a ~> g a
  fgb :: f b ~> g b
  fga *** fgb :: (f a, f b) ~> (g a, g b)

  ??? :: f (a,b) ~> g (a,b)

  uncurry pair :: (f a, f b) -> f (a,b)
  arr (uncurry pair) :: (f a, f b) ~> f (a,b)

-}

-- Functor & Cofunctor instances.  Beware use of 'arr', which is not
-- available for some of my favorite arrows.

instance (Arrow (~>), Cofunctor f, Functor g) => Functor (Arrw (~>) f g) where
  fmap h = inArrw $ \ fga -> arr (cofmap h) >>> fga >>> arr (fmap h)

instance (Arrow (~>), Functor f, Cofunctor g) => Cofunctor (Arrw (~>) f g) where
  cofmap h = inArrw $ \ fga -> arr (fmap h) >>> fga >>> arr (cofmap h)

-- Restated,
-- 
--   cofmap h = inArrw $ (arr (fmap h) >>>) . (>>> arr (cofmap h))


-- 'Arrw' specialized to functions.  
type (:->:) = Arrw (->)

---- Control.Applicative.Const

inConst :: (a -> b) -> Const a u -> Const b v
inConst f (Const a) = Const (f a)

inConst2 :: (a -> b -> c) -> Const a u -> Const b v -> Const c w
inConst2 f (Const a) (Const b) = Const (f a b)

inConst3 :: (a -> b -> c -> d)
         -> Const a u -> Const b v -> Const c w -> Const  d x
inConst3 f (Const a) (Const b) (Const c) = Const (f a b c)


---- Control.Applicative.Endo


instance Monoid_f Endo where { mempty_f = mempty; mappend_f = mappend }

-- | Convenience for partial-manipulating functions
inEndo :: ((a->a) -> (a'->a')) -> (Endo a -> Endo a')
inEndo f = Endo . f . appEndo

-- -- | Dual for 'inEndo'
-- outEndo :: (Endo a -> Endo a') -> ((a->a) -> (a'->a'))
-- outEndo g = appEndo . g . Endo

-- -- Missing from Control.Applicative
-- instance Arbitrary a => Arbitrary (Endo a) where
--   arbitrary   = fmap Endo arbitrary
--   coarbitrary = coarbitrary . appEndo

-- -- Simple show instance.  Better: show an arbitrary sampling of the function.
-- instance Show (Endo a) where show _ = "Endo <function>"
