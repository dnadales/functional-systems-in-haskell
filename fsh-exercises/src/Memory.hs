-- | Excercises taken from:
--
-- http://www.scs.stanford.edu/16wi-cs240h/slides/memory-slides.html

module Memory where

import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Concurrent

-- | Does the invariant has to hold after a transaction was executed?
-- Apparently yes!
invariantMustHoldOutsideTheTransaction :: IO ()
invariantMustHoldOutsideTheTransaction = do
  t <- newTVarIO (1)
  atomically $ alwaysSucceeds $ do
    v <- readTVar t
    when (v < 0) (fail "Negative value!")
  atomically $ modifyTVar t (subtract 99)
  return ()

-- | Even in another thread!
invariantMustHoldOutsideTheTransaction2 :: IO ()
invariantMustHoldOutsideTheTransaction2 = do
  t <- newTVarIO (1)
  atomically $ alwaysSucceeds $ do
    v <- readTVar t
    when (v < 0) (fail "Negative value!")
  forkIO $ do
    threadDelay 1000000
    atomically $ modifyTVar t (subtract 99)
  return ()

-- | Let's write a father and son program.
son :: TVar Int -> IO ()
son account = do
  forever $ do
    threadDelay 500000
    v <- atomically $ readTVar account
    putStrLn $  "        $$$ <--- I'll spend some money, I have " ++ (show v)
    atomically $ modifyTVar account (subtract 10)

father :: TVar Int -> IO ()  
father account = forever $ do
  threadDelay 1000000
  putStrLn "I'll give some money $$$ ---> "
  atomically $ modifyTVar account (+5)

fatherAndSon :: IO ()
fatherAndSon = do
  account <- newTVarIO 100
  atomically $ alwaysSucceeds $ do
    v <- readTVar account
    when (v < 0) (fail "Negative balance!") -- Note that on a negative balance
                                            -- only the son will abort.
  forkIO (father account)
  forkIO (son account)
  return ()

-- * Newtype semantics
newtype NTInt0 = NTInt0 Int deriving (Show)

-- Note that a newtype constructor cannot have stricteness annotations.
data NTInt1 = NTInt1 !Int deriving (Show)

uNTInt0 = NTInt0 undefined
uNTInt1 = NTInt1 undefined

-- Can we write code that behaves differently for the two definitions of NTInt
-- above?
f0 :: NTInt0 -> Int
f0 (NTInt0 x) = 0

f1 :: NTInt1 -> Int
f1 (NTInt1 x) = 1

-- then we have
ok = f0 uNTInt0

boom = f1 uNTInt1

-- Why does this happens? Well, forcing a value (by matching a constructor)
-- forces strict fields!

-- With irrefutable (lazy) pattern we can avoid this.
f1' :: NTInt1 -> Int
f1' ~(NTInt1 x) = 1

noBoom = f1' uNTInt1
