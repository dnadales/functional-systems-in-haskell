{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module GHCLanguageExtensions where

import           Control.Arrow           (second)
import           Control.Concurrent.MVar
import           Control.Monad           (ap, liftM)
import           Control.Monad.Fix
import           Control.Monad.Trans
import           Data.IORef
import           System.IO

instance (Monad m) => Monad (MStateT s m) where
  return x = MStateT $ \s -> pure (s, x)
  msa >>= fmsb = MStateT $ \s -> do
    (s0, a) <- runMStateT msa s
    runMStateT (fmsb a) s0

-- Are all monads applicatives?

-- Let:
--
-- > return :: a -> m a
-- > (>>=) :: m a -> (a -> m b) -> m b
--
-- We need to define:
--
-- > (<*>) :: m (a -> b) -> m a -> m b
-- > mf <*> ma = do
-- >   f <- mf
-- >   a <- ma
-- >   return (f a)

newtype MStateT s m a = MStateT { runMStateT :: s -> m (s, a)}

instance (Functor m) => Functor (MStateT s m) where
  fmap f ms = MStateT $ fmap (second f) . runMStateT ms

-- Can't we just use the `DeriveApplicative` and `DeriveFunctor` extensions?
-- Apparently not. See
-- https://stackoverflow.com/questions/34641279/getting-a-no-instance-of-applicative-error-when-declaring-a-monad
-- and the monad instance for `MIdentity` below.
instance (Monad m) => Applicative (MStateT s m) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)

instance MonadTrans (MStateT s) where
  lift m = MStateT $ \s -> (\x -> (s, x)) <$> m

-- * Let's implement `get` and `put`:
put :: Monad m => s -> MStateT s m ()
put s0 = MStateT $ \_ -> return (s0, ())

get :: Monad m => MStateT s m s
get = MStateT $ \s -> return (s, s)


xpp :: (Num s, Monad m) => MStateT s m s
xpp = do
  n <- get
  put (n + 1)
  return n

tryXpp :: IO ()
tryXpp = runMStateT go 0 >>= print
  where
    go = do xpp >>= lift . print
            xpp >>= lift . print

-- * Recursive binding
oneTwo :: (Int, Int)
oneTwo = (fst x, snd y)
  where
    x = (1, snd y)
    y = (fst x, 2)

nthFib :: Int -> Integer
nthFib = (fibs !!)
  where
    fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- ** Reimplementation of `oneTwo` and `nthFib` using a fix-point function.

-- The `fix` combinator is defined in `Control.Monad.Fix`:
--
-- > fix :: (a -> a) -> a
-- > fix f = let x = f x in x
--

oneTwoFix :: (Int, Int)
-- The tilde delays the evaluation of the argument passed to the function.
-- Removing it causes this function to run-forever (diverge), but why?
--
-- Apparently by saying
--
-- > x = f x
--
-- If `f` is eager, `x` has to be evaluated before it can be applied it to `f`
-- (since we have to pattern match). If `f` is not eager on its argument, then
-- we can have a fixed point (result), since the argument is not used at all.
oneTwoFix = fix $ \ ~(_, _) ->
  let x1 = (1, snd y1)
      y1 = (fst x1, 2)
  in  (fst x1, snd y1)

nthFibFix :: Int -> Integer
nthFibFix = (fibs !!)
  where
    fibs = fix $ \fs -> 1 : 1: zipWith (+) fs (tail fs)
    -- The strange thing about this is that the function that we pass to fix
    -- above does not have a fix point apparently, however we don't run into an
    -- infinite loop.

-- And you can implement (all) other recursive functions as well
mFact :: Int -> Int
mFact 0 = 1
mFact n = n * mFact (n - 1)

mFactFix :: Int -> Int
mFactFix = fix $ \f n -> if n == 0 then 1 else n * f (n - 1)

-- * Introducing `MonadFix`

mNthFib :: MonadFix m => Int -> m Integer
mNthFib n = (!! n) <$> mfix (\fs -> return (1 : 1: zipWith (+) fs (tail fs)))
  -- This is just:
  --
  -- > do
  -- >  fibs <- mfix $ \fs -> return (1 : 1: zipWith (+) fs (tail fs))
  -- >  return (fibs !! n)

data Link a = Link { value:: !a, rest :: !(MVar (Link a))}

mkCycle :: IO (MVar (Link Int))
mkCycle = do
  (l1, _) <- mfix $ \p ->
    case p of
      ~(l1', l2') -> do
        l1'' <- newMVar $ Link 1 l2'
        l2'' <- newMVar $ Link 2 l1'
        return (l1'', l2'')
  return l1

-- ** Implementing `mfix`

-- *** Warm up: Identity
newtype MIdentity a = MIdentity { runMIdentity :: a }

instance Functor MIdentity where
    fmap = liftM

instance Applicative MIdentity where
    pure = return
    (<*>) = ap

instance Monad MIdentity where
  return = MIdentity
  m >>= k = k (runMIdentity m)

instance MonadFix MIdentity where
  mfix fm = MIdentity $ fix (runMIdentity . fm)

-- ** Using fixIO
nthFibFixIO :: Int -> IO Integer
nthFibFixIO n  = (!! n) <$> fibs
    where
      fibs = fixIO $ \fs -> do
          putStrLn "Computing your fib" -- funny, I thought we would see this
                                        -- message multiple times when
                                        -- computing @nthFibFixIO 10@.
          return $ 1: 1: zipWith (+) fs (tail fs)

-- * Type-level booleans

data HFalse = HFalse deriving Show
data HTrue = HTrue deriving Show

-- HNot needs the MultiParamTypeClasses extension.
class HNot a b | a -> b where
    hNot :: a -> b

instance HNot HFalse HTrue where
    hNot _ = HTrue

instance HNot HTrue HFalse where
    hNot _ = HFalse

-- Without the functional dependency, if you try to write
--
-- > hNot HTrue
--
-- You'll get:
--
-- >    • Ambiguous type variable ‘a0’ arising from a use of ‘print’
-- >       prevents the constraint ‘(Show a0)’ from being solved.
-- >       Probable fix: use a type annotation to specify what ‘a0’ should be.
-- >       These potential instances exist:
-- >         instance (Show b, Show a) => Show (Either a b)
-- >           -- Defined in ‘Data.Either’
-- >         instance Show Ordering -- Defined in ‘GHC.Show’
-- >         instance Show Integer -- Defined in ‘GHC.Show’
-- >         ...plus 33 others
-- >         ...plus 237 instances involving out-of-scope types
-- >         (use -fprint-potential-instances to see them all)
-- >
--
-- the compiler does not which type to choose for `a0` since:
--
-- > nNot HTrue :: forall b. HNot HTrue b => b
--
-- By using functional dependencies, we have that the type `HFalse` gets
-- uniquely determined by this functional dependency.
--
-- > hNot HTrue :: HFalse
--
-- The use of functional dependencies also prevent us from writing:
--
-- > data HMuah = HMuah deriving Show
--
-- > instance HNot HFalse HMuah where
-- >    hNot _ = HTrue
--
-- Since this will give rise to the following error:
--
-- >     Functional dependencies conflict between instance declarations:
-- >       instance HNot HFalse HTrue
-- >         -- Defined at /home/damian/Documents/github/capitanbatata/functional-systems-in-haskell/fsh-exercises/src/GHCLanguageExtensions.hs:185:10
-- >       instance HNot HFalse HMuah
-- >         -- Defined at /home/damian/Documents/github/capitanbatata/functional-systems-in-haskell/fsh-exercises/src/GHCLanguageExtensions.hs:222:10
--

-- * Computing over types

class TypeEq a b c | a b -> c where
    typeEq :: a -> b -> c

instance TypeEq a a HTrue where
    typeEq _ _ = HTrue

-- We cannot just write:
--
-- > instance {-# OVERLAPS #-} TypeEq a b HFalse where
-- >     typeEq _ _ = HFalse
--
-- Because we'll have conflicting instances:
--
-- >     Functional dependencies conflict between instance declarations:
-- >       instance TypeEq a a HTrue
-- >         -- Defined at /home/damian/Documents/github/capitanbatata/functional-systems-in-haskell/fsh-exercises/src/GHCLanguageExtensions.hs:242:10
-- >       instance [overlap ok] TypeEq a b HFalse
-- >         -- Defined at /home/damian/Documents/github/capitanbatata/functional-systems-in-haskell/fsh-exercises/src/GHCLanguageExtensions.hs:245:27
--
-- Problem: `TypeEq a a HTrue` not more specific than `TypeEq a b HFalse`

class TypeCast a b | a -> b where typeCast :: a -> b
instance TypeCast a a where typeCast = id

-- This requires the 'UndecidableInstances' extension, and the
-- 'OverlappingInstances' extension, but since it is deprecated we use the
-- 'OVERLAPS' pragma.

instance {-# OVERLAPS #-} (TypeCast HFalse c) => TypeEq a b c where
    typeEq _ _ = typeCast HFalse

-- Then you can do things like:
--
-- > typeEq HTrue 'a'
-- > typeEq (10 :: Int) 'a'
--

-- * Heterogeneous lists
type a :+: b = Either a b

data HNil = HNil deriving Show
data h :*: t = h :*: !t deriving Show
infixr 9 :*: -- 9 has the highest precedence

foo :: (Int, String) :*: (Char, Integer) :*: (String, Double) :*: HNil
foo = (9, "Hello") :*: ('b', 7) :*: ("see", 3.0) :*: HNil

-- Notice that you need to append 'HNil' (Or anything else for that matter) to have:
--
-- > foo .! "fetch me a string"
--
-- returning "3.0", since otherwise GHC won't be able to find the correct instance.

-- How would you define a lookup operator on an heterogeneous list 'h':
--
-- > (.!) :: h -> k -> v

-- class Select k h v where -- What happens if we omit the functional dependency?
--
-- > :t foo .! (983 :: Int)
-- > foo .! (983 :: Int)
-- >  :: Select
-- >       Int ((Int, String) :*: ((Char, Integer) :*: (String, Double))) v => V
--
-- > foo .! (983 :: Int) :: String
-- > "Hello"
-- >  foo .! (983 :: Int)
-- > <interactive>:30:2: error:
-- >     • Ambiguous type variable ‘a0’ arising from a use of ‘print’
-- > ...
--
-- So the functional dependency seems to force the choice of 'String' as type
-- 'v'.
class Select k h v | k h -> v where -- What happens if we omit the functional dependency?
    (.!) :: h -> k -> v

instance Select k ((k, v) :*: t) v where
   ((_, val) :*: _) .! _ = val

instance {-# OVERLAPS #-} Select k t w => Select k (kv' :*: t) w where
    (_ :*: t) .! k = t .! k

-- > foo .! (1 :: Int)
-- > "Hello"
-- > foo .! (983 :: Int)
-- > "Hello"

-- Something more on the monomorphic restriction:
--
-- By default the monomorphic restriction seems to be turned off in the ghci, so if you enter:
--
-- > >>> let foo = (9, "Hello") :*: ('b', 7) :*: ("see", 3.0) :*: HNil
-- > >>> :t foo
-- > foo
-- >  :: (Num t2, Num t1, Fractional t) =>
-- >     (t2, [Char]) :*: ((Char, t1) :*: (([Char], t) :*: HNil))
--
-- If you set this on you'll get:
--
-- > >>> :set -XMonomorphismRestriction
-- > >>> let foo = (9, "Hello") :*: ('b', 7) :*: ("see", 3.0) :*: HNil
-- > :t foo
-- > foo
-- >  :: (Integer, [Char])
-- >     :*: ((Char, Integer) :*: (([Char], Double) :*: HNil))
--

-- * Object-oriented programming
returnIO :: a -> IO a
returnIO = return

data GetVal = GetVal deriving Show
data SetVal = SetVal deriving Show
data ClearVal = ClearVal deriving Show

-- mkVal :: Int -> IO ()
-- Select SetVal h (Int -> a) => Int -> h -> IO ((GetVal, IO Int) :*: (SetVal, Int -> IO ()) :*: (ClearVal, a) :*: HNil)
mkVal n self = do
    val <- newIORef (n :: Int)
    returnIO $ (GetVal, readIORef val)
           :*: (SetVal, writeIORef val)
           :*: (ClearVal, self .! SetVal $ 0)
           :*: HNil

testOO :: IO ()
testOO = do
    -- remember mfix :: (a -> m a) -> m a
    -- mkVal 7 ??? (What would we put here?), mfix seems to help here!
    x <- mfix $ mkVal 7 -- Here: :t mfix $ mkVal 7
                        --  :: IO (   (GetVal, IO Int)
                        --        :*: ((SetVal, Int -> IO ())
                        --        :*: ((ClearVal, IO ()) :*: HNil))
                        --        )
    x .! GetVal >>= print
    x .! ClearVal
    x .! GetVal >>= print
    x .! SetVal $ 10
    x .! GetVal >>= print

mkConstVal n self = do
    super <- mkVal n self
    returnIO $ (SetVal, const $ return ()) -- It ignores the 'SetVal' messages.
           :*: super

testInheritance = do
    x <- mfix $ mkConstVal 7
    x .! GetVal >>= print
    x .! ClearVal
    x .! GetVal >>= print
    x .! SetVal $ 10
    x .! GetVal >>= print
