{-# LANGUAGE OverloadedStrings #-}
-- | Playing with pipes.

module PWPipes where

import           Control.Exception      (throwIO, try)
import           Control.Monad          (forever, replicateM_, unless)
import qualified Data.ByteString        as S
import qualified GHC.IO.Exception       as G
import           Pipes
import qualified Pipes.Prelude          as Pipes
import qualified Pipes.Safe             as PS
import           Prelude                hiding (take)
import           System.FilePath
import           System.FilePath.Posix  ((</>))
import           System.IO              (isEOF)
import           System.IO
import           System.Posix
import           System.Posix.Directory

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
-- > for :: Monad m => Producer a m r -> (a -> Effect m ()) -> Effect m r
--
-- but also
--
-- > for :: Monad m => Producer a m r -> (a -> Producer b m ()) -> Producer b m r
--
-- since:
--
-- > data X  -- The uninhabited type
-- > type Effect m r = Producer X m r
--

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

-- Let's connect @stdinLn@ and @stdoutLn@ using @>->@:
--
-- > stdinLn  :: Producer String IO ()
-- > stdoutLn :: Consumer String IO ()
-- > (>->)    :: Monad m => Producer a m r -> Consumer a m r -> Effect m r
stdInOut :: IO ()
stdInOut = runEffect $ stdinLn >-> stdoutLn

-- This won't work.
--stdInOut' = runEffect $ stdinLn >-> stdoutLn >-> stdoutLn

take :: Int -> Pipe a a IO ()
take n = do
  replicateM_ n $ do
    x <- await
    yield x
  lift $ putStrLn "You shall not pass!"


maxInput :: Int -> Producer String IO ()
-- stdinLn :: MonadIO m => Producer' String m ()
--
-- One possible way to instantiate the types of @>->@ is:
--
-- > (>->) :: Monad m => Producer a m r -> Pipe   a b m r -> Producer b m r
--
maxInput n = Pipes.stdinLn >-> take n
-- How to run @maxInput@? We need a consumer so that we can produce an effect!
-- Remember:
--
-- > (>->)    :: Monad m => Producer a m r -> Consumer a m r -> Effect m r
--
-- The type instantiation above seems to be the only one that produces an
-- effect...

maxInputStdInOut :: IO ()
maxInputStdInOut = runEffect $ maxInput 4 >-> Pipes.stdoutLn

-- Next, let's build a pipe that says yes three times...
yes :: Monad m => Producer String m r
yes = forever (yield "yes")

yesx3 :: IO ()
yesx3 = runEffect $ yes >-> Pipes.take 3 >-> Pipes.stdoutLn

-- * ListT

-- lists with side-effects in Haskell, hmmm...
pairs :: ListT IO (Int, Char)
pairs = do
  -- > Select :: Producer a m () -> ListT m a
  -- > each :: (Monad m, Foldable f) => f a -> Producer a m ()
  x <- Select $ each [0, 1, 2]
  lift $ putStrLn $ "x = " ++ show x
  y <- Select $ each ['a', 'b']
  lift $ putStrLn $ "y = " ++ show y
  return (x, y)

-- | let's print these pairs
printPairs :: IO ()
-- > for :: Monad m => Producer a m r -> (a -> Effect m ()) -> Effect m r
-- > every :: (Monad m, Enumerable t) => t m a -> Producer' a m ()
printPairs = runEffect $ for (every pairs) (lift . print)

-- | Alternative definition using pipes:
printPairs' :: IO ()
printPairs' = runEffect $ (every pairs) >-> Pipes.print

-- ** Crazy code with pipes...
input :: Producer String IO ()
input = Pipes.stdinLn >-> Pipes.takeWhile (/= "quit")

name :: ListT IO String
name = do
  firstName <- Select input
  lastName <- Select input
  return ("Name: " ++ firstName ++ " " ++ lastName)

outputNames :: IO ()
outputNames = runEffect $ (every name) >-> Pipes.print
-- When you run this, the first name you input will get bound and only the
-- second will change.
--
-- Entering quit one time will allow to re-bind the first name.
--
-- Entering quit twice will quit the whole program.
--
-- What is happening?
--
-- Maybe this can be explained by the way @printPairs@ works:
--
-- > >>> printPairs
-- > x = 0
-- > y = 'a'
-- > (0,'a')
-- > y = 'b'
-- > (0,'b')
-- > x = 1
-- > y = 'a'
-- > (1,'a')
-- > y = 'b'
-- > (1,'b')
-- > x = 2
-- > y = 'a'
-- > (2,'a')
-- > y = 'b'
-- > (2,'b')
--
-- Here you can see that to produce a new element, the inner index (@y@) varies
-- faster, which seems to indicate that the first element is "reused". Only
-- after the elements of @Select $ each ['a', 'b']@ are exhausted, the next
-- element in @Select $ each [0, 1, 2]@ is bound

-- > loop :: Monad m => (a -> ListT m b) -> Pipe a b m r

-- ** Combining computations on @ListT@

-- ** Mix ListT with Pipes

-- * Tricks

mapPipe :: Monad m => (a -> b) -> Pipe a b m r
mapPipe f = for cat $ \x -> yield (f x)
-- We have
-- > for :: Monad m => Pipe   x b m r -> (b -> Pipe     x c m ()) -> Pipe     x c m r
-- > cat :: Monad m => Pipe a a m r
-- > yield :: Monad m => a -> Pipe x a m ()

yes' :: Monad m => Producer String m r
-- This is strange, I would expect that the "y" value is fed only once. However
-- it will continue drawing "y"'s from the producer (@return "y"@).
yes' = return "y" >~ cat

yesx3' :: IO ()
yesx3' = runEffect $ yes' >-> Pipes.take 3 >-> Pipes.stdoutLn

-- TODO: continue with "Tricks"...

-- Composing pipes inside another pipe:
--
-- Remember:
-- > each :: (Monad m, Foldable f) => f a -> Producer a m ()
customerService :: Producer String IO ()
customerService = do
    each [ "Hello, how can I help you?"        -- Begin with a script
         , "Hold for one second."
         ]
    Pipes.stdinLn >-> Pipes.takeWhile (/= "Goodbye!")  -- Now continue with a human

runCustomerService :: IO ()
runCustomerService = runEffect (customerService >-> Pipes.stdoutLn)

-- How to use @each@ and @~>@ to traverse nested structures.
-- > each :: (Monad m, Foldable f) => f a -> Producer a m ()
-- > (~>) :: Monad m               => (a -> Producer a m ()) -> (b -> Producer c m ()) -> (a -> Producer c m ())
-- > each ~> each ~> each ~> lift . print
-- >   :: (Foldable f2, Foldable f1, Foldable f, Show a) =>
-- >     f (f1 (f2 a)) -> Proxy x' x c' c IO ()
extractVals =
  runEffect $ (((each ~> each) ~> each) ~> lift . print) xs
  where xs = [[Just 1, Nothing], [Just 2, Just 3]]
-- Remember that each seems to extract the values of a foldable and puts them
-- into a producer.

extractVals' =
  runEffect $ ((each ~> each) ~> lift . print) xs
  where xs = [[Just 1, Nothing], [Just 2, Just 3]]

-- * Examples

-- ** Listing all the files in a directory
enumFiles :: FilePath -> Producer FilePath (PS.SafeT IO) ()
enumFiles path =
  PS.bracket (openDirStream path) (closeDirStream) loop
  where
    loop :: DirStream -> Producer FilePath (PS.SafeT IO) ()
    loop ds = PS.liftBase (readDirStream ds) >>= checkName
      where
        checkName :: FilePath -> Producer FilePath (PS.SafeT IO) ()
        checkName ""   = return ()
        checkName "."  = loop ds
        checkName ".." = loop ds
        checkName name = PS.liftBase (getSymbolicLinkStatus newPath)
                         >>= checkStat newPath
          where newPath = path </> name

        checkStat path stat
          | isRegularFile stat = yield path >> loop ds
          | isDirectory stat = enumFiles path >> loop ds
          | otherwise = loop ds

-- | Implementation of
--
-- > find . -type f
--
listFiles path = for (enumFiles path) $ PS.liftBase . putStrLn
-- Alternatively, using pipes.
listFiles' path = enumFiles "." >-> Pipes.stdoutLn

runListFiles = PS.runSafeT $ runEffect $  listFiles "."
runListFiles' = PS.runSafeT $ runEffect $ listFiles' "."

-- To run:
--
-- > PS.runSafeT $ runEffect $  listFiles "."

-- Now write a function that counts the number of files.
--
-- It seems we need to deal with the concept of termination...

-- TODO: count the files in a directory.
--
-- See:
--   - http://www.yesodweb.com/blog/2013/10/core-flaw-pipes-conduit
--   - https://stackoverflow.com/questions/35139548/how-to-write-a-haskell-pipes-sum-function
--
-- You might need to use the @next@ function:
--
--   -  next :: Monad m => Producer a m r -> m (Either r (a, Producer a m r))

one :: Monad m => Pipe a Int m ()
one = await >> yield 1 >> one

-- > enumFiles "" >-> one :: Proxy X () () Int (Pipes.Safe.SafeT IO) ()
--
-- or equivalently:
--
-- > Producer Int (PS.SafeT IO) ()
--
-- > sum' (enumFiles "" >-> one) :: Pipes.Safe.SafeT IO Int

countLines :: FilePath -> PS.SafeT IO Int
countLines path = sum' (enumFiles path >-> one)
-- This implements: find . -type f | wc -l

showLines :: FilePath -> IO ()
showLines path = do
  n <- PS.runSafeT $ countLines path
  print $ "Got " ++ (show n) ++ " files"

-- Re-implementation of the sum function.
--
-- Note that this could be generalized to a fold...
sum' :: (Monad m, Num a) => Producer a m () -> m a
sum' = go 0
  where
    go n p = next p >>= \x -> case x of
      Left _        -> return n
      Right (m, p') -> go (n + m) p'


-- TODO: define a function that returns the lines of a file.
enumFile :: FilePath -> Producer S.ByteString (PS.SafeT IO) ()
enumFile path = PS.bracket (openFile path ReadMode) (hClose) loop
  where
    loop :: Handle -> Producer S.ByteString (PS.SafeT IO) ()
    loop h = do
          input <- PS.liftBase (S.hGetSome h 32752)
          if S.null input
            then return ()
            else (yield input) >> loop h

catFile :: FilePath -> IO ()
catFile path = PS.runSafeT $ runEffect $ enumFile path >-> Pipes.print


-- TODO: Give an alternative version of @enumFiles@ as described in the answer here:
--
--  - https://stackoverflow.com/questions/44267928/listing-all-the-files-under-a-directory-recursively-using-pipes`

-- TODO: list the directories in a breadth first order (check whether the depth
-- first version breaks).
