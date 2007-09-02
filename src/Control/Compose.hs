{-# LANGUAGE Rank2Types, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, TypeSynonymInstances #-}

-- TypeOperators

----------------------------------------------------------------------
-- |
-- Module      :  Control.Compose
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  see LANGUAGE pragma
-- 
-- Various type constructor compositions and instances for them.
-- Some come from 
-- [1] \"Applicative Programming with Effects\"
-- <http://www.soi.city.ac.uk/~ross/papers/Applicative.html>
----------------------------------------------------------------------

module Control.Compose
  ( Unop, Binop
  , Cofunctor(..)
  , O(..), inO, inO2, inO3
  , fmapFF, fmapCC, cofmapFC, cofmapCF
  , OO(..)
  , Monoid_f(..)
  , Flip(..), inFlip, inFlip2, inFlip3, OI, ToOI(..)
  , ArrowAp(..)
  , FunA(..), inFunA, inFunA2, FunAble(..)
  , App(..), inApp, inApp2
  , Id(..), inId
  , (:*:)(..), (***#), ($*), inProd, inProd2, inProd3
  , (::*::)(..), inProdd, inProdd2
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


type Unop  a = a -> a                   -- ^ Unary functions
type Binop a = a -> a -> a              -- ^ Binary functions

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
'Cofunctor' instances.

@
    instance (  Functor g,   Functor f) => Functor (O g f) where fmap = fmapFF
    instance (Cofunctor g, Cofunctor f) => Functor (O g f) where fmap = fmapCC

    instance (Functor g, Cofunctor f) => Cofunctor (O g f) where cofmap = cofmapFC
    instance (Cofunctor g, Functor f) => Cofunctor (O g f) where cofmap = cofmapCF
@

However, it's such a bother to define the Functor instances per
composition type, I've left the fmapFF case in.  If you want the fmapCC
one, you're out of luck for now.  I'd love to hear a good solution.  Maybe
someday Haskell will do Prolog-style search for instances, subgoaling the
constraints, rather than just matching instance heads.

-}

-- TODO: change prefix O uses ("O g f") to infix ("g `O` f") throughout,
-- once I'm running on the new Haddock.

newtype (g `O` f) a = O { unO :: g (f a) }

instance (Functor h, Adorn b (f a)) => Adorn (h b) ((h `O` f) a) where
  adorn   = O . fmap adorn
  unadorn = fmap unadorn . unO 

-- Or this more symmetric variant

-- instance (Functor h, Adorn b (f a), Adorn c (h b)) =>
--     Adorn c ((h `O` f) a) where
--   adorn   = O . fmap adorn . adorn
--   unadorn = unadorn . fmap unadorn . unO 


-- Here it is, as promised.
instance (  Functor g,   Functor f) => Functor (O g f) where fmap = fmapFF


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


-- | Composition of type constructors: binary with unary.  See also
-- 'FunA', which specializes from arrows to functions.
-- 
-- Warning: Wolfgang Jeltsch pointed out a problem with these definitions:
-- 'splitA' and 'mergeA' are not inverses.  The definition of 'first',
-- e.g., violates the \"extension\" law and causes repeated execution.
-- Look for a reformulation or a clarification of required properties of
-- the applicative functor @f@.
-- 
-- See also "Arrows and Computation", which notes that the following type
-- is "almost an arrow" (http://www.soi.city.ac.uk/~ross/papers/fop.html).
-- 
-- > newtype ListMap i o = LM ([i] -> [o])


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

mergeA :: Applicative f => (f a, f b) -> f (a,b)
mergeA ~(fa,fb) = liftA2 (,) fa fb

splitA :: Applicative f => f (a,b) -> (f a, f b)
splitA fab = (liftA fst fab, liftA snd fab)


-- Hm.  See warning above for 'ArrowAp'

-- | Common pattern for 'Arrow's.
newtype FunA h a b = FunA (h a -> h b)

-- | Apply unary function in side a 'FunA' representation.
inFunA :: ((h a -> h b) -> (h' a' -> h' b'))
       -> (FunA h a b -> FunA h' a' b')
inFunA q (FunA f) = FunA (q f)

-- | Apply binary function in side a 'FunA' representation.
inFunA2 :: ((h a -> h b) -> (h' a' -> h' b') -> (h'' a'' -> h'' b''))
       -> (FunA h a b -> FunA h' a' b' -> FunA h'' a'' b'')
inFunA2 q (FunA f) (FunA g) = FunA (q f g)

-- | Support needed for a 'FunA' to be an 'Arrow'.
class FunAble h where
  arrFun    :: (a -> b) -> (h a -> h b)
  firstFun  :: (h a -> h a') -> (h (a,b) -> h (a',b))
  secondFun :: (h b -> h b') -> (h (a,b) -> h (a,b'))
  (***%)    :: (h a -> h b) -> (h a' -> h b') -> (h (a,a') -> h (b,b'))
  (&&&%)    :: (h a -> h b) -> (h a  -> h b') -> (h a -> h (b,b'))


instance FunAble h => Arrow (FunA h) where
  arr p  = FunA    (arrFun p)
  (>>>)  = inFunA2 (>>>)
  first  = inFunA  firstFun
  second = inFunA  secondFun
  (***)  = inFunA2 (***%)
  (&&&)  = inFunA2 (&&&%)



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

-- | (-> IO ()) as a 'Flip'.  A Cofunctor.
type OI = Flip (->) (IO ())

-- | Convert to an 'OI'.
class ToOI sink where toOI :: sink b -> OI b

instance ToOI OI where toOI = id

-- | Type application
newtype App f a = App { unApp :: f a }

-- Apply unary function inside of an 'App representation.
inApp :: (f a -> f' a') -> (App f a -> App f' a')
inApp f (App ar) = App (f ar)

-- Apply binary function inside of a 'App' representation.
inApp2 :: (f a -> f' a' -> f'' a'') -> (App f a -> App f' a' -> App f'' a'')
inApp2 h (App fa) (App fa') = App (h fa fa')

-- Example: App IO ()
instance (Applicative f, Monoid m) => Monoid (App f m) where
  mempty = App (pure mempty)
  App a `mappend` App b = App (liftA2 mappend a b)


{-
-- We can also drop the App constructor, but then we overlap with many
-- other instances, like [a].
instance (Applicative f, Monoid a) => Monoid (f a) where
  mempty  = pure mempty
  mappend = liftA2 mappend
-}


-- | Identity type constructor.  Until there's a better place to find it.
-- I'd use "Control.Monad.Identity", but I don't want to introduce a
-- dependency on mtl just for Id.
newtype Id a = Id { unId :: a }

inId :: (a -> b) -> (Id a -> Id b)
inId f (Id a) = Id (f a)

-- | Pairing of unary type constructors
newtype (f :*: g) a = Prod { unProd :: (f a, g a) }
  -- deriving (Show, Eq, Ord)

-- In GHC 6.7, deriving no longer works on types like :*:.  So:

instance (Show (f a, g a)) => Show ((f :*: g) a) where
  show (Prod p) = "Prod " ++ show p

instance (Eq (f a, g a)) => Eq ((f :*: g) a) where
  Prod p == Prod q = p == q

instance (Ord (f a, g a)) => Ord ((f :*: g) a) where
  Prod p <= Prod q = p <= q
  Prod p `compare` Prod q = p `compare` q

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

-- | A handy combining form.  See '(***#)' for an sample use.
($*) :: (a -> b, a' -> b') -> (a,a') -> (b,b')
($*) = uncurry (***)

-- | Combine two binary functions into a binary function on pairs
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

instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap h = inProd (fmap h *** fmap h)

------

-- | Pairing of binary type constructors
newtype (f ::*:: g) a b = Prodd { unProdd :: (f a b, g a b) }
  -- deriving (Show, Eq, Ord)

instance (Show (f a b, g a b)) => Show ((f ::*:: g) a b) where
  show (Prodd p) = "Prod " ++ show p

instance (Eq (f a b, g a b)) => Eq ((f ::*:: g) a b) where
  Prodd p == Prodd q = p == q

instance (Ord (f a b, g a b)) => Ord ((f ::*:: g) a b) where
  Prodd p < Prodd q = p < q

-- | Apply binary function inside of @f :*: g@ representation.
inProdd :: ((f a b, g a b) -> (f' a' b', g' a' b'))
        -> ((f ::*:: g) a b -> (f' ::*:: g') a' b')
inProdd h (Prodd p) = Prodd (h p)

-- | Apply binary function inside of @f :*: g@ representation.
inProdd2 :: ((f a b, g a b) -> (f' a' b', g' a' b') -> (f'' a'' b'', g'' a'' b''))
         -> ((f ::*:: g) a b -> (f' ::*:: g') a' b' -> (f'' ::*:: g'') a'' b'')
inProdd2 h (Prodd p) (Prodd p') = Prodd (h p p')

instance (Arrow f, Arrow f') => Arrow (f ::*:: f') where
  arr    = Prodd .  (arr    &&&  arr   )
  (>>>)  = inProdd2 ((>>>)  ***# (>>>) )
  first  = inProdd  (first  ***  first )
  second = inProdd  (second ***  second)
  (***)  = inProdd2 ((***)  ***# (***) )
  (&&&)  = inProdd2 ((&&&)  ***# (&&&) )


------

-- | Arrow-like type between type constructors (doesn't enforce @Arrow
-- (~>)@ here).
newtype Arrw (~>) f g a = Arrw { unArrw :: f a ~> g a }

-- Coverage condition fails, hence -fallow-undecidable-instances
instance (Arrow (~>), Adorn b (f a), Adorn c (g a))
  => Adorn (b ~> c) (Arrw (~>) f g a) where
    adorn   = Arrw . ((arr unadorn >>>) . (>>> arr adorn))
    unadorn = ((arr adorn >>>) . (>>> arr unadorn)) . unArrw

-- | Apply unary function inside of @Arrw@ representation.
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


---- For Control.Applicative Const

inConst :: (a -> b) -> Const a u -> Const b v
inConst f (Const a) = Const (f a)

inConst2 :: (a -> b -> c) -> Const a u -> Const b v -> Const c w
inConst2 f (Const a) (Const b) = Const (f a b)

inConst3 :: (a -> b -> c -> d)
         -> Const a u -> Const b v -> Const c w -> Const  d x
inConst3 f (Const a) (Const b) (Const c) = Const (f a b c)


---- For Control.Applicative.Endo

instance Monoid_f Endo where { mempty_f = mempty; mappend_f = mappend }

-- | Convenience for partial-manipulating functions
inEndo :: (Unop a -> Unop a') -> (Endo a -> Endo a')
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

