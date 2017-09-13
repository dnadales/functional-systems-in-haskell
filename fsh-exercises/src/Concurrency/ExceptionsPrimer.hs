{-# LANGUAGE DeriveDataTypeable #-}
-- | Exercises extracted from:
--
--    http://www.scs.stanford.edu/16wi-cs240h/slides/concurrency-slides.html#(1)

module Concurrency.ExceptionsPrimer where

import           Control.Exception
import           Data.Typeable

-- * Declaring your own exceptions:
-- you need the @DeriveDataTypeable@ extension.

data MyError = MyError String deriving (Show, Typeable)
instance Exception MyError

-- * Catching exceptions:
--
-- > catch :: Exception e => IO a -> (e -> IO a) -> IO a
--
-- The following function will only catch exceptions of type @MyError@:
--
-- > λ> catcher $ readFile "/nonono"
-- > *** Exception: /nonono: openFile: does not exist (No such file or directory)
-- > λ> catcher $ readFile "/dev/null"
-- > Just ""
--
-- Throwing exceptions:
--
-- > throw :: Exception e => e -> a
-- > throwIO :: Exception e => e -> IO a
--
-- > λ> catcher $ throwIO $ MyError "boom!"
-- > boom!
-- > Nothing
--
catcher :: IO a -> IO (Maybe a)
catcher action = (Just <$> action) `catch` handler
  where handler (MyError msg) = do putStrLn msg; return Nothing


-- | A pure catcher:
pureCatcher :: a -> IO (Maybe a)
pureCatcher a = (a `seq` return (Just a)) `catch` handler
  where handler (SomeException _) = return Nothing

-- * Some examples
--
-- > λ> pureCatcher $ 1 + 1
-- > Just 2
-- > λ> pureCatcher $ 1 `div` n0
-- > *** Exception: divide by zero
-- > pureCatcher (undefined :: String)
-- > Nothing
--
-- But look at what happens here:
--
-- > pureCatcher (undefined:[] :: String)
-- > Just "*** Exception: Prelude.undefined
-- > CallStack (from HasCallStack):
--   error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
--   undefined, called at <interactive>:107:14 in interactive:Ghci4
--
-- Evaluating a list does not evaluate head or tails!
--
-- See
-- http://stackoverflow.com/questions/11046590/the-seq-function-and-strictness/11048004#11048004:
-- Apparently evaluating a list just checks wether the list is empty or not!
--
-- " However both seq and ($!) evaluate their argument only enough to check that
--  it is not bottom. If the argument is a list, this means it only has to
--  check the first cons cell." (http://users.aber.ac.uk/afc/stricthaskell.html#seq)
--
-- "When evaluating an expression, seq stops as soon as it reaches a
-- constructor."
-- (http://book.realworldhaskell.org/read/functional-programming.html)
--
-- "When evaluating an expression, seq stops as soon as it reaches a
-- constructor. For simple types like numbers, this means that it will evaluate
-- them completely. Algebraic data types are a different story. Consider the
-- value (1+2):(3+4):[]. If we apply seq to this, it will evaluate the (1+2)
-- thunk. Since it will stop when it reaches the first (:) constructor, it will
-- have no effect on the second thunk. The same is true for tuples: seq
-- ((1+2),(3+4)) True will do nothing to the thunks inside the pair, since it
-- immediately hits the pair's constructor"
-- (http://book.realworldhaskell.org/read/functional-programming.html)
--
-- Now what would happen if we define
data StrictList a = Nil | StrictCons !a !(StrictList a) deriving Show

r = pureCatcher ((StrictCons undefined Nil) :: StrictList Char)
