{-# LANGUAGE Rank2Types, FlexibleInstances, MultiParamTypeClasses
           , FlexibleContexts, UndecidableInstances, TypeSynonymInstances
           , TypeOperators, GeneralizedNewtypeDeriving, StandaloneDeriving
           , CPP
  #-}
-- For ghc 6.6 compatibility
-- {-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

----------------------------------------------------------------------
-- |
-- Module      :  Control.Compose
-- Copyright   :  (c) Conal Elliott 2007-2012
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  see LANGUAGE pragma
-- 
-- Various type constructor compositions and instances for them.
-- Some come from 
-- \"Applicative Programming with Effects\"
-- <http://www.soi.city.ac.uk/~ross/papers/Applicative.html>
----------------------------------------------------------------------

module Control.Compose
  ( 
  -- * Value transformers
    Unop, Binop
  -- * Specialized semantic editor combinators
  , result, argument, (~>), (~>*), (<~), (*<~)
  -- * Contravariant functors
  , ContraFunctor(..), bicomap
  -- * Unary\/unary composition
  , (:.)(..), O, unO, biO, convO, coconvO, inO, inO2, inO3
  , oPure, oFmap, oLiftA2, oLiftA3
  , fmapFF, fmapCC, contraFmapFC, contraFmapCF
  -- , DistribM(..), joinMM
  , joinMMT, joinComposeT
  -- * Type composition
  -- ** Unary\/binary
  , OO(..)
--   -- * Binary\/unary
--   , ArrowAp(..),
  -- ** (->)\/unary
  , FunA(..), inFunA, inFunA2, FunAble(..)
  -- * Monoid constructors
  , Monoid_f(..)
  -- * Flip a binary constructor's type arguments
  , Flip(..), biFlip, inFlip, inFlip2, inFlip3, OI, ToOI(..)
  -- * Type application
  , (:$)(..), App, biApp, inApp, inApp2
  -- * Identity
  , Id(..),unId, biId, inId, inId2
  -- * Constructor pairing
  -- ** Unary
  , (:*:)(..),(*:*), biProd, convProd, (***#), ($*), inProd, inProd2, inProd3
  -- * Binary
  , (::*::)(..), (*::*), inProdd, inProdd2
  -- * Arrow between /two/ constructor applications
  , Arrw(..), (:->:)
  , biFun, convFun, inArrw, inArrw2, inArrw3
  -- * Augment other modules
  , biConst, inConst, inConst2, inConst3
  , biEndo, inEndo
  ) where

#if __GLASGOW_HASKELL__ >= 609
import Control.Category
import Prelude hiding ((.), id)
#endif

import Control.Arrow
#if __GLASGOW_HASKELL__ < 610
                      hiding (pure)
#endif

import Data.Monoid
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Control.Monad (join)

-- import Test.QuickCheck -- for Endo

import Data.Bijection

infixl 9 :. -- , `O`
infixl 7 :*:
infixr 1 :->:
infixr 0 :$

infixl 0 $*
infixr 3 ***#


{----------------------------------------------------------
    Value transformers
----------------------------------------------------------}

-- | Unary functions
type Unop  a = a -> a
-- | Binary functions
type Binop a = a -> a -> a


{--------------------------------------------------------------------
    Semantic editor combinators, specialized to functions.
    See http://conal.net/blog/posts/semantic-editor-combinators/.
    Also the DeepArrow package.
--------------------------------------------------------------------}

-- | Add pre-processing
-- argument :: (a' -> a) -> ((a -> b) -> (a' -> b))
argument :: Category cat => (a' `cat` a) -> ((a `cat` b) -> (a' `cat` b))
argument = flip (.)

-- | Add post-processing
result :: Category cat => (b `cat` b') -> ((a `cat` b) -> (a `cat` b'))
result = (.)

infixr 1 ~>, ~>*
infixl 1 <~, *<~

-- | Add pre- and post processing
(~>) :: Category cat =>
        (a' `cat` a) -> (b `cat` b') -> ((a `cat` b) -> (a' `cat` b'))
-- (f ~> h) g = h . g . f
f ~> h = result h . argument f

(<~) :: Category cat =>
        (b `cat` b') -> (a' `cat` a) -> ((a `cat` b) -> (a' `cat` b'))
(<~) = flip (~>)

-- If I add argument back to DeepArrow, we can get a different generalization:
-- 
-- (~>) :: DeepArrow cat => (a' `cat` a) -> (b `cat` b') -> ((a -> b) `cat` (a' -> b'))

-- | Like '(~>)' but specialized to functors and functions.
(~>*) :: (Functor p, Functor q) => 
         (a' -> a) -> (b -> b') -> (p a -> q b) -> (p a' -> q b')
f ~>* g = fmap f ~> fmap g

(*<~) :: (Functor p, Functor q) => 
         (b -> b') -> (a' -> a) -> (p a -> q b) -> (p a' -> q b')
(*<~) = flip (~>*)

-- (~>*) and (*<~) could be generalized to other categories (beside functions)
-- if we use a more general Functor, as in the "categories" package.

{----------------------------------------------------------
    Contravariant functors
----------------------------------------------------------}

-- | Contravariant functors.  often useful for /acceptors/ (consumers,
-- sinks) of values.
class ContraFunctor h where
  contraFmap :: (a -> b) -> (h b -> h a)

-- | Bijections on contravariant functors
bicomap :: ContraFunctor f => (a :<->: b) -> (f a :<->: f b)
bicomap (Bi ab ba) = Bi (contraFmap ba) (contraFmap ab)


{----------------------------------------------------------
    Type composition
----------------------------------------------------------}

{- |

Composition of unary type constructors

There are (at least) two useful 'Monoid' instances, so you'll have to
pick one and type-specialize it (filling in all or parts of @g@ and\/or @f@).

>     -- standard Monoid instance for Applicative applied to Monoid
>     instance (Applicative (g :. f), Monoid a) => Monoid ((g :. f) a) where
>       { mempty = pure mempty; mappend = liftA2 mappend }
>     -- Especially handy when g is a Monoid_f.
>     instance Monoid (g (f a)) => Monoid ((g :. f) a) where
>       { mempty = O mempty; mappend = inO2 mappend }

Corresponding to the first and second definitions above,

>     instance (Applicative g, Monoid_f f) => Monoid_f (g :. f) where
>       { mempty_f = O (pure mempty_f); mappend_f = inO2 (liftA2 mappend_f) }
>     instance Monoid_f g => Monoid_f (g :. f) where
>       { mempty_f = O mempty_f; mappend_f = inO2 mappend_f }

Similarly, there are two useful 'Functor' instances and two useful
'ContraFunctor' instances.

>     instance (      Functor g,       Functor f) => Functor (g :. f) where fmap = fmapFF
>     instance (ContraFunctor g, ContraFunctor f) => Functor (g :. f) where fmap = fmapCC
> 
>     instance (      Functor g, ContraFunctor f) => ContraFunctor (g :. f) where contraFmap = contraFmapFC
>     instance (ContraFunctor g,       Functor f) => ContraFunctor (g :. f) where contraFmap = contraFmapCF

However, it's such a bother to define the Functor instances per
composition type, I've left the fmapFF case in.  If you want the fmapCC
one, you're out of luck for now.  I'd love to hear a good solution.  Maybe
someday Haskell will do Prolog-style search for instances, subgoaling the
constraints, rather than just matching instance heads.

-}
newtype (g :. f) a = O (g (f a)) deriving (Eq,Show)

-- newtype (g :. f) a = O { unO :: g (f a) } deriving Show

-- | Unwrap a '(:.)'.
unO :: (g :. f) a -> g (f a)
unO (O gfa) = gfa

-- | Compatibility synonym
type O = (:.)

-- Here it is, as promised.

instance (Functor g, Functor f) => Functor (g :. f) where fmap = fmapFF

-- or
-- 
--   deriving instance (Functor g, Functor f) => Functor (g :. f)

-- These next two instances are based on suggestions from Creighton Hogg: 

instance (Foldable g, Foldable f, Functor g) => Foldable (g :. f) where
  -- foldMap f = fold . fmap (foldMap f) . unO
  foldMap f = foldMap (foldMap f) . unO
  -- fold (O gfa) = fold (fold <$> gfa)
  -- fold = fold . fmap fold . unO
  fold = foldMap fold . unO
  -- I could let fold default

instance (Traversable g, Traversable f) => Traversable (g :. f) where
  -- sequenceA = fmap O . sequenceA . fmap sequenceA . unO
  -- sequenceA = fmap O . traverse sequenceA . unO
  -- sequenceA = (unO ~> fmap O) (traverse sequenceA)
  -- traverse f = fmap O . traverse (traverse f) . unO
  traverse = (unO ~> fmap O) . traverse . traverse

-- traverse f
-- sequenceA . fmap f
-- sequenceA . (inO.fmap.fmap) f
-- sequenceA . inO (fmap (fmap f))
-- sequenceA . O . fmap (fmap f) . unO
-- fmap O . traverse sequenceA . unO . O . fmap (fmap f) . unO 
-- fmap O . traverse sequenceA . fmap (fmap f) . unO 
-- fmap O . traverse (sequenceA .  fmap f) . unO 
-- fmap O . traverse (traverse f) . unO 

-- instance (Functor g, Functor f) => Functor (g :. f) where
--   fmap = inO.fmap.fmap

-- | @newtype@ bijection
biO :: g (f a) :<->: (g :. f) a
biO = Bi O unO

-- | Compose a bijection, Functor style
convO :: Functor g => (b :<->: g c) -> (c :<->: f a) -> (b :<->: (g :. f) a)
convO biG biF = biG >>> bimap biF >>> Bi O unO

-- | Compose a bijection, ContraFunctor style
coconvO :: ContraFunctor g => (b :<->: g c) -> (c :<->: f a) -> (b :<->: (g :. f) a)
coconvO biG biF = biG >>> bicomap biF >>> Bi O unO


-- | Apply a unary function within the 'O' constructor.
inO :: (g (f a) -> g' (f' a')) -> ((g :. f) a -> (g' :. f') a')
inO = unO ~> O

-- | Apply a binary function within the 'O' constructor.
inO2 :: (g (f a)   -> g' (f' a')   -> g'' (f'' a''))
     -> ((g :. f) a -> (g' :. f') a' -> (g'' :. f'') a'')
inO2 = unO ~> inO

-- | Apply a ternary function within the 'O' constructor.
inO3 :: (g (f a)   -> g' (f' a')   -> g'' (f'' a'')   -> g''' (f''' a'''))
     -> ((g :. f) a -> (g' :. f') a' -> (g'' :. f'') a'' -> (g''' :. f''') a''')
inO3 = unO ~> inO2


-- | Handy combination of 'O' and 'pure'.
oPure   :: (Applicative g) => f a -> (g :. f) a

-- | Handy combination of 'inO' and 'fmap'.
oFmap   :: (Functor g') =>
           (f a -> f' a') -> (g' :. f) a -> (g' :. f') a'

-- | Handy combination of 'inO2' and 'liftA2'.
oLiftA2 :: (Applicative g'') =>
           (f a -> f' a' -> f'' a'')
        -> (g'' :. f) a -> (g'' :. f') a' -> (g'' :. f'') a''

-- | Handy combination of 'inO3' and 'liftA3'.
oLiftA3 :: (Applicative g''') =>
           (f a -> f' a' -> f'' a'' -> f''' a''')
        -> (g''' :. f) a
        -> (g''' :. f') a'
        -> (g''' :. f'') a''
        -> (g''' :. f''') a'''

oPure   = O    . pure
oFmap   = inO  . fmap
oLiftA2 = inO2 . liftA2
oLiftA3 = inO3 . liftA3


-- | Used for the @Functor :. Functor@ instance of 'Functor'
fmapFF :: (  Functor g,   Functor f) => (a -> b) -> (g :. f) a -> (g :. f) b
fmapFF = inO.fmap.fmap

-- | Used for the @ContraFunctor :. ContraFunctor@ instance of 'Functor'
fmapCC :: (ContraFunctor g, ContraFunctor f) => (a -> b) -> (g :. f) a -> (g :. f) b
fmapCC = inO.contraFmap.contraFmap

-- | Used for the @Functor :. ContraFunctor@ instance of 'Functor'
contraFmapFC :: (Functor g, ContraFunctor f) => (b -> a) -> (g :. f) a -> (g :. f) b
contraFmapFC = inO.fmap.contraFmap

-- contraFmapFC h (O gf) = O (fmap (contraFmap h) gf)

-- | Used for the @ContraFunctor :. Functor@ instance of 'Functor'
contraFmapCF :: (ContraFunctor g, Functor f) => (b -> a) -> (g :. f) a -> (g :. f) b
contraFmapCF = inO.contraFmap.fmap

-- contraFmapCF h (O gf) = O (contraFmap (fmap h) gf)

instance (Applicative g, Applicative f) => Applicative (g :. f) where
  pure  = O . pure . pure
  (<*>) = (inO2.liftA2) (<*>)

-- Possible Alternative instances:

-- instance (Applicative g, Alternative f) => Alternative (g :. f) where
--   empty = O (pure empty)
--   (<|>) = inO2 (liftA2 (<|>))

-- instance (Alternative g, Applicative f) => Alternative (g :. f) where
--   empty = O empty
--   (<|>) = inO2 (<|>)


-- Possible Monoid instances:

-- instance (Applicative g, Applicative f, Monoid a) => Monoid ((g :. f) a) where
--   mempty  = pure mempty
--   mappend = liftA2 mappend

-- instance Monoid (g (f a)) => Monoid ((g :. f) a) where
--   mempty  = O mempty
--   mappend = inO2 mappend



{-

-- A first pass at monad composition.  But now I've read "Composing
-- Monads", and I know there's more to it.  At least four different ways,
-- all with conflicting Monad instances.

-- | Monad distributivity.
-- 
-- TODO: what conditions are required so that @(m :. n)@ satisfies the monad laws?
class DistribM m n where
  distribM :: n (m a) -> m (n a)

instance (Monad m, Monad n, DistribM m n) => Monad (m :. n) where
  return  = O . return . return
  e >>= f = joinMM (liftM f e)

-- | 'join' for @(m :. n)@
joinMM :: (Monad m, Monad n, DistribM m n) =>
          (m :. n) ((m :. n) a) -> (m :. n) a
joinMM = O . liftM join . join . liftM distribM . unO . liftM unO

-- Derivation:
-- 
--       (m :. n) ((m :. n) a)
--   --> m (n (m (n a)))      -- liftM unO
--   --> m (n ((m :. n) a))   -- unO
--   --> m (m (n (n a)))      -- liftM distribM
--   --> m (n (n a))          -- join
--   --> m (n a)              -- liftM join
--   --> (m :. n) a           -- O

-}

-- | 'join'-like function for implicitly composed monads
joinMMT :: (Monad m, Monad n, Traversable n, Applicative m) =>
           m (n (m (n a))) -> m (n a)
joinMMT = fmap join . join . fmap sequenceA

-- | 'join'-like function for explicitly composed monads
joinComposeT :: (Monad m, Monad n, Traversable n, Applicative m) =>
                (m :. n) ((m :. n) a) -> (m :. n) a
joinComposeT = O . joinMMT . unO . fmap unO


{----------------------------------------------------------
    Unary\/binary composition
----------------------------------------------------------}

-- | Composition of type constructors: unary with binary.  Called
-- "StaticArrow" in [1].
newtype OO f j a b = OO { unOO :: f (a `j` b) }


#if __GLASGOW_HASKELL__ >= 609
instance (Applicative f, Category cat) => Category (OO f cat) where
  id          = OO (pure id)
  OO g . OO h = OO (liftA2 (.) g h)
#endif

instance (Applicative f, Arrow arr) => Arrow (OO f arr) where
#if __GLASGOW_HASKELL__ < 609
  OO g >>> OO h = OO (liftA2 (>>>) g h)
#endif
  arr           = OO . pure . arr
  first (OO g)  = OO (liftA first g)

-- For instance, /\ a b. f (a -> m b) =~ OO f Kleisli m


{-

{----------------------------------------------------------
    Binary\/unary composition.  * Not currently exported *
----------------------------------------------------------}

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
-- is "almost an arrow" (<http://www.soi.city.ac.uk/~ross/papers/fop.html>).
-- 
-- >   newtype ListMap i o = LM ([i] -> [o])
--
-- http://www.cse.unsw.edu.au/~dons/haskell-1990-2006/msg16550.html

-- | 

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

-}


{----------------------------------------------------------
    (->)\/unary composition
----------------------------------------------------------}

-- Hm.  See warning above for 'ArrowAp'

-- | Common pattern for 'Arrow's.
newtype FunA h a b = FunA { unFunA :: h a -> h b }

-- | Apply unary function in side a 'FunA' representation.
inFunA :: ((h a -> h b) -> (h' a' -> h' b'))
       -> (FunA h a b -> FunA h' a' b')
inFunA = unFunA ~> FunA

-- | Apply binary function in side a 'FunA' representation.
inFunA2 :: ((h a -> h b) -> (h' a' -> h' b') -> (h'' a'' -> h'' b''))
       -> (FunA h a b -> FunA h' a' b' -> FunA h'' a'' b'')
inFunA2 q (FunA f) = inFunA (q f)

-- | Support needed for a 'FunA' to be an 'Arrow'.
class FunAble h where
  arrFun    :: (a -> b) -> (h a -> h b) -- ^ for 'arr'
  firstFun  :: (h a -> h a') -> (h (a,b) -> h (a',b)) -- for 'first'
  secondFun :: (h b -> h b') -> (h (a,b) -> h (a,b')) -- for 'second'
  (***%)    :: (h a -> h b) -> (h a' -> h b') -> (h (a,a') -> h (b,b')) -- for '(***)'
  (&&&%)    :: (h a -> h b) -> (h a  -> h b') -> (h a -> h (b,b')) -- for '(&&&)'

  -- In direct imitation of Arrow defaults:
  f ***% g = firstFun f >>> secondFun g
  f &&&% g = arrFun (\b -> (b,b)) >>> f ***% g


#if __GLASGOW_HASKELL__ >= 609
instance FunAble h => Category (FunA h) where
  id  = FunA (arrFun id)
  (.) = inFunA2 (.)
#endif


instance FunAble h => Arrow (FunA h) where
  arr p  = FunA    (arrFun p)
#if __GLASGOW_HASKELL__ < 609
  (>>>)  = inFunA2 (>>>)
#endif
  first  = inFunA  firstFun
  second = inFunA  secondFun
  (***)  = inFunA2 (***%)
  (&&&)  = inFunA2 (&&&%)



{----------------------------------------------------------
    Monoid constructors
----------------------------------------------------------}

-- | Simulates universal constraint @forall a. Monoid (f a)@.
-- 
-- See Simulating Quantified Class Constraints
-- (<http://flint.cs.yale.edu/trifonov/papers/sqcc.pdf>)
--  Instantiate this schema wherever necessary:
--
-- >    instance Monoid_f f where { mempty_f = mempty ; mappend_f = mappend }
class Monoid_f m where
  mempty_f  :: forall a. m a
  mappend_f :: forall a. m a -> m a -> m a

--  e.g.,
instance Monoid_f [] where { mempty_f = mempty ; mappend_f = mappend }



{----------------------------------------------------------
    Flip a binary constructor's type arguments
----------------------------------------------------------}

-- | Flip type arguments
newtype Flip j b a = Flip { unFlip :: a `j` b }

-- | @newtype@ bijection
biFlip :: (a `j` b) :<->: Flip j b a
biFlip = Bi Flip unFlip

-- Apply unary function inside of a 'Flip' representation.
inFlip :: ((a `j` b) -> (a' `k` b')) -> (Flip j b a -> Flip k b' a')
inFlip = unFlip ~> Flip

-- Apply binary function inside of a 'Flip' representation.
inFlip2 :: ((a `j` b) -> (a' `k` b') -> (a'' `l` b''))
        -> (Flip j b a -> Flip k b' a' -> Flip l b'' a'')
inFlip2 f (Flip ar) = inFlip (f ar)

-- Apply ternary function inside of a 'Flip' representation.
inFlip3 :: ((a `j` b) -> (a' `k` b') -> (a'' `l` b'') -> (a''' `m` b'''))
        -> (Flip j b a -> Flip k b' a' -> Flip l b'' a'' -> Flip m b''' a''')
inFlip3 f (Flip ar) = inFlip2 (f ar)

instance Arrow arr => ContraFunctor (Flip arr b) where
  contraFmap h (Flip f) = Flip (arr h >>> f)

-- Useful for (~>) = (->).  Maybe others.
instance (Applicative (j a), Monoid o) => Monoid (Flip j o a) where
  mempty  = Flip (pure mempty)
  mappend = inFlip2 (liftA2 mappend)

-- TODO: generalize (->) to (~>) with Applicative_f (~>)
instance Monoid o => Monoid_f (Flip (->) o) where
  { mempty_f = mempty ; mappend_f = mappend }

-- | (-> IO ()) as a 'Flip'.  A ContraFunctor.
type OI = Flip (->) (IO ())

-- | Convert to an 'OI'.
class ToOI sink where toOI :: sink b -> OI b

instance ToOI OI where toOI = id

{----------------------------------------------------------
    Type application
----------------------------------------------------------}

-- | Type application
-- We can also drop the @App@ constructor, but then we overlap with many
-- other instances, like @[a]@.  Here's a template for @App@-free
-- instances.
-- 
-- >    instance (Applicative f, Monoid a) => Monoid (f a) where
-- >      mempty  = pure mempty
-- >      mappend = liftA2 mappend
newtype f :$ a = App { unApp :: f a }

-- | Compatibility synonym for (:$).
type App = (:$)

-- How about?
-- data f :$ a = App { unApp :: f a }

-- | @newtype@ bijection
biApp :: f a :<->: App f a
biApp = Bi App unApp

-- Apply unary function inside of an 'App representation.
inApp :: (f a -> f' a') -> (App f a -> App f' a')
inApp = unApp ~> App

-- Apply binary function inside of a 'App' representation.
inApp2 :: (f a -> f' a' -> f'' a'') -> (App f a -> App f' a' -> App f'' a'')
inApp2 h (App fa) = inApp (h fa)

-- Example: App IO ()
instance (Applicative f, Monoid m) => Monoid (App f m) where
  mempty  =   App  (pure   mempty )
  mappend = inApp2 (liftA2 mappend)

--  App a `mappend` App b = App (liftA2 mappend a b)


{----------------------------------------------------------
    Identity
----------------------------------------------------------}

-- | Identity type constructor.  Until there's a better place to find it.
-- I'd use "Control.Monad.Identity", but I don't want to introduce a
-- dependency on mtl just for Id.
newtype Id a = Id a deriving Show

-- Could define record field:
-- 
--   newtype Id a = Id { unId :: a } deriving Show
-- 
-- but then Show is uglier.

-- Extract value from an 'Id'
unId :: Id a -> a
unId (Id a) = a

inId :: (a -> b) -> (Id a -> Id b)
inId = unId ~> Id

inId2 :: (a -> b -> c) -> (Id a -> Id b -> Id c)
inId2 f (Id a) = inId (f a)

-- | @newtype@ bijection
biId :: a :<->: Id a
biId = Bi Id unId

instance Functor Id where
  fmap f = inId f

instance Applicative Id where
  pure  = Id
  (<*>) = inId2 ($)

instance Monad Id where
  return = pure
  Id x >>= f = f x

instance Foldable Id where
  foldMap f (Id a) = f a
  -- foldMap f = f . unId
  -- foldMap = (. unId)

instance Traversable Id where
  sequenceA (Id fa) = fmap Id fa

-- Id fa :: Id (f a)
-- fa :: f a
-- fmap Id fa = f (Id a)

{----------------------------------------------------------
    Unary constructor pairing
----------------------------------------------------------}

-- | Pairing of unary type constructors
newtype (f :*: g) a = Prod { unProd :: (f a, g a) }
  -- deriving (Show, Eq, Ord)

-- | Handy infix & curried 'Prod'
(*:*) :: f a -> g a -> (f :*: g) a
(*:*) = curry Prod

-- | @newtype@ bijection
biProd :: (f a, g a) :<->: (f :*: g) a
biProd = Bi Prod unProd

-- | Compose a bijection
convProd :: (b :<->: f a) -> (c :<->: g a) -> (b,c) :<->: (f :*: g) a
convProd biF biG = biF *** biG >>> Bi Prod unProd

-- In GHC 6.7, deriving no longer works on types like :*:.  Take out the
-- following three instances when deriving works again, in GHC 6.8.

instance (Show (f a, g a)) => Show ((f :*: g) a) where
  show (Prod p) = "Prod " ++ show p

instance (Eq (f a, g a)) => Eq ((f :*: g) a) where
  Prod p == Prod q = p == q

instance (Ord (f a, g a)) => Ord ((f :*: g) a) where
  Prod p <= Prod q = p <= q
  Prod p `compare` Prod q = p `compare` q

-- | Apply unary function inside of @f :*: g@ representation.
inProd :: ((f a, g a) -> (f' a', g' a'))
       -> ((f :*: g) a -> (f' :*: g') a')
inProd = unProd ~> Prod

-- | Apply binary function inside of @f :*: g@ representation.
inProd2 :: ((f a, g a) -> (f' a', g' a') -> (f'' a'', g'' a''))
        -> ((f :*: g) a -> (f' :*: g') a' -> (f'' :*: g'') a'')
inProd2 h (Prod p) = inProd (h p)

-- | Apply ternary function inside of @f :*: g@ representation.
inProd3 :: ((f a, g a) -> (f' a', g' a') -> (f'' a'', g'' a'')
                       -> (f''' a''', g''' a'''))
        -> ((f :*: g) a -> (f' :*: g') a' -> (f'' :*: g'') a''
                        -> (f''' :*: g''') a''')
inProd3 h (Prod p) = inProd2 (h p)

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

instance (Applicative f, Applicative g) => Applicative (f :*: g) where
  pure a = Prod (pure a, pure a)
  (<*>) = inProd2 (\ (f,g) (a,b) -> (f <*> a, g <*> b))

{----------------------------------------------------------
    Binary constructor pairing
----------------------------------------------------------}

-- | Pairing of binary type constructors
newtype (f ::*:: g) a b = Prodd { unProdd :: (f a b, g a b) }
  deriving (Show, Eq, Ord)

-- | Handy infix & curried 'Prodd'
(*::*) :: f a b -> g a b -> (f ::*:: g) a b
(*::*) = curry Prodd

-- -- Remove the next three when GHC can derive them (6.8).

-- instance (Show (f a b, g a b)) => Show ((f ::*:: g) a b) where
--   show (Prodd p) = "Prod " ++ show p

-- instance (Eq (f a b, g a b)) => Eq ((f ::*:: g) a b) where
--   Prodd p == Prodd q = p == q

-- instance (Ord (f a b, g a b)) => Ord ((f ::*:: g) a b) where
--   Prodd p < Prodd q = p < q

-- | Apply binary function inside of @f :*: g@ representation.
inProdd :: ((f a b, g a b) -> (f' a' b', g' a' b'))
        -> ((f ::*:: g) a b -> (f' ::*:: g') a' b')
inProdd = unProdd ~> Prodd

-- | Apply binary function inside of @f :*: g@ representation.
inProdd2 :: ((f a b, g a b) -> (f' a' b', g' a' b') -> (f'' a'' b'', g'' a'' b''))
         -> ((f ::*:: g) a b -> (f' ::*:: g') a' b' -> (f'' ::*:: g'') a'' b'')
inProdd2 h (Prodd p) = inProdd (h p)


#if __GLASGOW_HASKELL__ >= 609
instance (Category f, Category f') => Category (f ::*:: f') where
  id  = Prodd (id,id)
  (.) = inProdd2 ((.) ***# (.))
#endif


instance (Arrow f, Arrow f') => Arrow (f ::*:: f') where
  arr    = Prodd . (arr &&&  arr)
#if __GLASGOW_HASKELL__ < 609
  (>>>)  = inProdd2 ((>>>) ***# (>>>))
#endif
  first  = inProdd  (first  ***  first )
  second = inProdd  (second ***  second)
  (***)  = inProdd2 ((***)  ***# (***) )
  (&&&)  = inProdd2 ((&&&)  ***# (&&&) )


{----------------------------------------------------------
    Arrow between /two/ constructor applications
----------------------------------------------------------}

-- | Arrow-like type between type constructors (doesn't enforce @Arrow
-- (~>)@ here).
newtype Arrw j f g a = Arrw { unArrw :: f a `j` g a } -- deriving Monoid

-- For ghc-6.6, use the "deriving" above, but for 6.8 use the "deriving" below.

deriving instance Monoid (f a `j` g a) => Monoid (Arrw j f g a)

-- Replace with generalized bijection?

-- toArrw :: Arrow j => (f a ~> b) -> (c ~> g a) -> ((b ~> c) -> Arrw j f g a)
-- toArrw fromF toG h = Arrw (fromF >>> h >>> toG)

-- fromArrw :: Arrow j => (b ~> f a) -> (g a ~> c) -> (Arrw j f g a -> (b ~> c))
-- fromArrw toF fromG (Arrw h') = toF >>> h' >>> fromG

-- | Apply unary function inside of @Arrw@ representation.
inArrw :: ((f a `j` g a) -> (f' a' `j` g' a'))
       -> ((Arrw j f g) a -> (Arrw j f' g') a')
inArrw = unArrw ~> Arrw

-- | Apply binary function inside of @Arrw j f g@ representation.
inArrw2 :: ((f a `j` g a) -> (f' a' `j` g' a') -> (f'' a'' `j` g'' a''))
        -> (Arrw j f g a -> Arrw j f' g' a' -> Arrw j f'' g'' a'')
inArrw2 h (Arrw p) = inArrw (h p)

-- | Apply ternary function inside of @Arrw j f g@ representation.
inArrw3 ::
  ((f a `j` g a) -> (f' a' `j` g' a') ->
   (f'' a'' `j` g'' a'') -> (f''' a''' `j` g''' a'''))
  -> ((Arrw j f g) a -> (Arrw j f' g') a' -> (Arrw j f'' g'') a'' -> (Arrw j f''' g''') a''')
inArrw3 h (Arrw p) = inArrw2 (h p)

-- Functor & ContraFunctor instances.  Beware use of 'arr', which is not
-- available for some of my favorite arrows.

instance (Arrow j, ContraFunctor f, Functor g) => Functor (Arrw j f g) where
  fmap h = inArrw $ \ fga -> arr (contraFmap h) >>> fga >>> arr (fmap h)

instance (Arrow j, Functor f, ContraFunctor g) => ContraFunctor (Arrw j f g) where
  contraFmap h = inArrw $ \ fga -> arr (fmap h) >>> fga >>> arr (contraFmap h)

-- Restated,
-- 
--   contraFmap h = inArrw $ (arr (fmap h) >>>) . (>>> arr (contraFmap h))

-- 'Arrw' specialized to functions.  
type (:->:) = Arrw (->)

-- | @newtype@ bijection
biFun :: (f a -> g a) :<->: (f :->: g) a
biFun = Bi Arrw unArrw

-- | Compose a bijection
convFun :: (b :<->: f a) -> (c :<->: g a) -> ((b -> c) :<->: (f :->: g) a)
convFun bfa cga = (bfa ---> cga) >>> biFun

-- biA :: ((f a -> g a) :<->: (f :->: g) a)
-- biA = Bi Arrw unArrw


{----------------------------------------------------------
    Augment other modules
----------------------------------------------------------}

---- For Control.Applicative Const

-- newtype Const a b = Const { getConst :: a }

-- | @newtype@ bijection
biConst :: a :<->: Const a b
biConst = Bi Const getConst

inConst :: (a -> b) -> (Const a u -> Const b v)
inConst = getConst ~> Const

inConst2 :: (a -> b -> c) -> Const a u -> Const b v -> Const c w
inConst2 f (Const a) = inConst (f a)

inConst3 :: (a -> b -> c -> d)
         -> Const a u -> Const b v -> Const c w -> Const  d x
inConst3 f (Const a) = inConst2 (f a)


---- For Control.Applicative.Endo

-- deriving instance Monoid o => Monoid (Const o a)
instance Monoid o => Monoid (Const o a) where
  mempty  = Const mempty
  mappend = inConst2 mappend

-- newtype Endo a = Endo { appEndo :: a -> a }

-- | @newtype@ bijection
biEndo :: (a -> a) :<->: Endo a
biEndo = Bi Endo appEndo

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

