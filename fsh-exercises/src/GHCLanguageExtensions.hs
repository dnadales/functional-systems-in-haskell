module GHCLanguageExtensions where

import           Control.Arrow           (second)
import           Control.Concurrent.MVar
import           Control.Monad           (ap, liftM)
import           Control.Monad.Fix
import           Control.Monad.Trans
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



