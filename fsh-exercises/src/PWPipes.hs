{-# LANGUAGE OverloadedStrings #-}
-- | Playing with pipes.

module PWPipes where

import           Control.Exception (throwIO, try)
import           Control.Monad     (unless)
import qualified GHC.IO.Exception  as G
import           Pipes
import           System.IO         (isEOF)

-- * Producers

-- | Producer that yields the strings read from the standard input.
stdinLn :: Producer String IO ()
stdinLn = do
  eof <- lift isEOF
  unless eof $ do
    str <- lift getLine
    yield str
    stdinLn

-- To run the producer we need an effect!
--
-- This won't compile:
-- > runEffect stdinLn
--
-- > src/PWPipes.hs:28:22-28: error: …
-- >    • Couldn't match type ‘[Char]’ with ‘X’
-- >      Expected type: Effect IO ()
-- >        Actual type: Producer String IO ()

-- The @for@ function allows use to get an effect.
stdinReader :: Effect IO ()
stdinReader = for stdinLn (lift . putStrLn)

-- Let's run it:
runStdIn :: IO ()
runStdIn = runEffect stdinReader

-- Now let's write a producer that triples its input:
tripleP :: a -> Producer a IO ()
tripleP x = yield x >> yield x >> yield x

-- Can we use @tripleP@ to write an effects that triples the lines entered in
-- the standard input?
--
-- Not with producers only. We need ways to pass the value yielded by @stdinLn@
-- function to @tripleP@, and we have no operator to do this. It seems
-- @tripleP@ should use @await@ for this, and thus it becomes a @Consumer@.
--
-- > stdinReader3 :: Effect IO ()
-- > stdinReader3 = for (stdinLn ??? tripleP) (lift . putStrLn)
--
-- The only alternative is to use nested @for@'s:
stdinReader3 :: Effect IO ()
stdinReader3 = for stdinLn $ \line ->
                 for (tripleP line) (lift . putStrLn)

-- But look how you can define the function above in a more concise way:
stdinReader3' :: Effect IO ()
stdinReader3' = for stdinLn (tripleP ~> lift . putStrLn)

-- So we can now do more cool things like reversing the input three times!
reverseP :: [a] -> Producer [a] IO ()
reverseP  = yield . reverse

stdinRedaer3 :: Effect IO ()
stdinRedaer3 = for stdinLn (tripleP ~> reverseP ~> lift . putStrLn)

-- And in this case, we can first reverse and then triple the output:
stdinRedaer3' :: Effect IO ()
stdinRedaer3' = for stdinLn (reverseP ~> tripleP ~> lift . putStrLn)

-- * Consumers

-- | A consumer that awaits for strings.
stdoutLn :: Consumer String IO ()
stdoutLn = do
  str <- await -- await a String
  x <- lift $ try $ putStrLn str
  case x of
    Left e@(G.IOError { G.ioe_type = t}) ->
      lift $ unless (t == G.ResourceVanished) $ throwIO e
    Right _ -> stdoutLn

-- How do we feed this consumer? We use @>~@ (feed).
stdoutWriter :: IO ()
stdoutWriter = runEffect $ lift readLn >~ stdoutLn
-- Careful when testing this. You need to input strings, which are delimited by
-- @"@s. At least from the ghci REPL. If you forget to quote them you'd get a
--
-- > *** Exception: user error (Prelude.readIO: no parse)
--
-- error.

-- Writing a consumer that consumes two times
doubleC :: Monad m => Consumer a m (a, a)
doubleC = (,) <$> await <*> await

-- And a consumer that consumes a pair of lists and concatenates them.
concatPairC :: Monad m => Consumer ([a], [a]) m [a]
concatPairC = (uncurry (++)) <$> await
  -- do
  -- (xs, ys) <- await
  -- return (xs ++ ys)

-- And lets put everything together:
stdoutDoubleWriter :: IO ()
stdoutDoubleWriter = runEffect $ lift readLn >~ doubleC >~ concatPairC >~ stdoutLn

-- Once @doubleC@ outputs something, this is passed to the @await@s of other consumers.
--
-- Note the difference between the way consumers and producers are composed.
-- The output of the consumers are passed to the other consumers using @>~@,
-- but in case of the producers, they don't consume any input, but producers
-- have to be created by a function @a -> Producer b m c@. Compare the types of
-- both functions:
--
-- > (~>) :: Monad m => (a -> Producer b m ()) -> (b -> Producer c m ()) -> (a -> Producer c m ())
-- > (>~) :: Monad m => Consumer a m b         -> Consumer b m c         -> Consumer a m c
--

-- * Pipes
