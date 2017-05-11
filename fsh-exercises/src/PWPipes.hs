{-# LANGUAGE OverloadedStrings #-}
-- | Playing with pipes.

module PWPipes where

import           Control.Monad (unless)
import           Pipes
import           System.IO     (isEOF)

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

-- Can we use @tripleP@ to write an effects that triples the lines enteres in
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
