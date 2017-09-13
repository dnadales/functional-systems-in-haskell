{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes   #-}
-- | Exercises taken from:
--
-- http://www.scs.stanford.edu/16wi-cs240h/slides/iteratee-slides.html#(1)

module Iteratee where

import           Control.Exception          (SomeException, bracket)
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString            as S
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Foldable              (fold)
import           Data.Int                   (Int64)
import           System.Directory           (doesDirectoryExist, doesFileExist,
                                             listDirectory)
import           System.FilePath
import           System.FilePath.Posix      ((</>))
import           System.IO                  (IOMode (ReadMode), hClose,
                                             openFile)
import           System.IO.Unsafe           (unsafeInterleaveIO)
import           System.Posix

listDirectoryWithPrefix :: FilePath -> IO [FilePath]
listDirectoryWithPrefix path = map (path </>) <$> (listDirectory path)

directoriesIO :: FilePath -> IO [FilePath]
directoriesIO path =
  listDirectoryWithPrefix path >>= filterM (doesDirectoryExist)

filesIO :: FilePath -> IO [FilePath]
filesIO path = listDirectoryWithPrefix path >>= filterM (doesFileExist)

-- | Return all the files in a directory.
recDir :: FilePath -> IO [FilePath]
recDir path = do
  files <- filesIO path
  recFiles <- recFilesIO
  return (files ++ recFiles)
  where recFilesIO :: IO [FilePath]
        recFilesIO =
          concat <$> (traverse recDir =<< directoriesIO path)

-- | Read the contents of a list of files into a lazy string.
-- readFiles :: [FilePath] -> IO L.ByteString
-- readFiles fs = fold <$> (traverse L.readFile =<< filterM doesFileExist fs)

readFiles' :: [FilePath] -> IO L.ByteString
readFiles' [] = return L.empty
readFiles' (f:fs) = liftM2 L.append (L.readFile f)
                   (unsafeInterleaveIO $ readFiles' fs)

-- | function that counts the lines in a file. The result should be equivalent
-- to:
--
-- > find /usr/include -type f -print | xargs cat | wc -l
countLines :: FilePath -> IO Int64
countLines dir = L8.count '\n' <$> (recDir dir >>= readFiles')

-- | A lazy version of @recDir@:
recDirLazy :: FilePath -> IO [FilePath]
recDirLazy dir = do
  ds <- openDirStream dir

  let nextName = unsafeInterleaveIO $ readDirStream ds >>= checkName

      checkName "" = closeDirStream ds >> return []
      checkName "." = nextName
      checkName ".." = nextName
      checkName name = getSymbolicLinkStatus path >>= checkStat path
          where path = dir </> name

      checkStat path stat
          | isRegularFile stat = liftM (path :) nextName
          | isDirectory stat   = liftM2 (++) (recDir path) nextName
          | otherwise          = nextName

  nextName

-- * The Iteratee

-- | Coding an Iteratee.
data Chunk = Chunk { chunkData  :: !L.ByteString
                   , chunkAtEOF :: !Bool
                   } deriving (Show)

-- | The iteratee is the data sink.
newtype Iter a = Iter { runIter :: Chunk -> Result a }

data Result a = Done { rResult :: a, rResidual :: Chunk }
              | NeedInput !(Iter a)
              | NeedIO !(IO (Result a))
              | Failed !SomeException

-- How would you define readLine?
readLine :: Iter (Maybe L.ByteString)
readLine = Iter (go L.empty)
  where go acc (Chunk input eof)
          | not (L.null b) = Done (Just acca) (Chunk btail eof)
          | not eof = NeedInput (Iter (go acca)) -- @b@ is null but we need
                                                 -- more input.
          --  L.null b && eof = Done Nothing (Chunk acca eof)
          | otherwise = Done Nothing (Chunk acca eof)
          where (a, b) = L8.break (== '\n') input
                acca = L.append acc a
                btail = L.tail b

-- | An enumerator feeds data into the Iteratee:
type Enumerator a = Iter a -> IO (Result a)

-- | Getting the contents of a file:
enumerateFile :: FilePath -> Enumerator a -- Note the use of a polymorphic type
                                          -- @a@.
enumerateFile path iter0 =
  bracket (openFile path ReadMode) hClose $ \h ->
  let go :: Iter a -> IO (Result a)
      go iter = do
        input <- S.hGetSome h 32752
        if S.null input
          then return (NeedInput iter)
          else check $ runIter iter $
               Chunk (L.fromChunks [input]) False
      check :: Result a -> IO (Result a)
      check (NeedInput iter) = go iter
      check (NeedIO iter)    = iter >>= check --  Here we need to check again
                                              -- the result in case we need to
                                              -- do more IO or provide more
                                              -- input.
      check result           = return result
  in go iter0

chunkEOF :: Chunk
chunkEOF = Chunk L.empty True

-- | Exercise 0: extract the result of an iteratee.
getResult0 :: Result a -> IO a
getResult0 (Done res _)     = return res
getResult0 (NeedInput iter) = getResult0 (runIter iter $ chunkEOF)
getResult0 (NeedIO io)      = io >>= getResult0
getResult0 (Failed e)       = throwIO e

-- | Exercise 1: get the first line of a file.
getFirstLine :: FilePath -> IO (Maybe L8.ByteString)
getFirstLine fp = enumerateFile fp readLine >>= getResult0

-- | Exercise 2: what about reading the second line?
getSecondLine :: FilePath -> IO (Maybe L8.ByteString)
getSecondLine fp = enumerateFile fp read2Line >>= getResult0

read2Line :: Iter (Maybe L.ByteString)
read2Line = Iter go
 where go ch =
         case runIter readLine $  ch of
           Done _ ch1  -> runIter readLine $  ch1
           NeedInput _ -> NeedInput read2Line
           NeedIO act  -> NeedIO act
           Failed e    -> Failed e

-- | Exercise 3: counting the lines in a file
countLinesIter :: Iter Int
countLinesIter = Iter $ go 0 readLine
  where go :: Int -> Iter a -> Chunk -> Result Int
        go n iter ch = check $ runIter iter ch
          where check :: Result a -> Result Int
                check (Done _ ch1@(Chunk _ True)) = Done n ch1
                check (Done _ ch1) = go (n + 1) iter ch1
                check (NeedInput iter1) = NeedInput (Iter (go n iter1))
                check (NeedIO io) = NeedIO (check <$> io)
                check (Failed e) = Failed e


countLinesI :: FilePath -> IO Int
countLinesI fp = enumerateFile fp countLinesIter >>= getResult0

-- | Exercise 4: define an iter instane for monad.
instance Functor Result where
  fmap f (Done a rest)    = Done (f a) rest
  fmap f (NeedInput iter) = NeedInput (fmap f iter)
  fmap f (NeedIO ior)     = NeedIO (liftM (fmap f) ior)
  fmap _ (Failed e)       = Failed e

instance Applicative Result where
  pure x = Done x chunkEOF
  (Done f _) <*> res1 = fmap f res1
  (NeedInput iter0) <*> res1 =
    NeedInput $ Iter $ \chunk ->
    runIter iter0 chunk <*> res1
  (NeedIO ior) <*> res1 = NeedIO (liftM (<*> res1) ior)
  (Failed e) <*> _ = Failed e

instance Functor Iter where
  fmap f0 (Iter iter0) = Iter $ \chunk -> fmap f0 (iter0 chunk)

instance Applicative Iter where
  pure = return
  fiter <*> itera = do
    a <- itera
    f <- fiter
    return (f a)

instance Monad Iter where
  return = Iter . Done
  (>>=) :: forall a b . Iter a -> (a -> Iter b) -> Iter b
  (Iter iter0) >>= fiter = Iter $ \chunk -> continue (iter0 chunk)
    where -- continue :: Result a -> Result b
          continue (Done x rest)     = runIter (fiter x) rest
          continue (NeedInput iter1) = NeedInput (iter1 >>= fiter)
          continue (NeedIO ior)      = NeedIO (liftM continue ior)
          continue (Failed e)        = Failed e

  fail msg = iterThrow (ErrorCall msg)

iterThrow :: (Exception e) => e -> Iter a
iterThrow e = Iter $ \_ -> Failed (toException e)

-- Exercise 5: use this monad instance to retrite @countLinesIter@
countLinesIterM :: Iter Int
countLinesIterM = gCountLinesIterM 0
  where gCountLinesIterM n = do
          res <- readLine
          case res of
            Nothing -> return n
            Just _  -> gCountLinesIterM $! n + 1


countLinesIM :: FilePath -> IO Int
countLinesIM fp = enumerateFile fp countLinesIterM >>= getResult0

-- Exercise 6: implement a function to concatenate enumerators.
cat0 :: Enumerator a -> Enumerator a -> Enumerator a
cat0 a b iter = a iter >>= check
  where check (NeedInput iter') = b iter'
        check (NeedIO io)       = io >>= check
        check r                 = return r

enumerateNull :: Enumerator a
enumerateNull = return . NeedInput

countLines0 :: FilePath -> IO Int
countLines0 dir = do
  files <- recDirLazy dir
  let enumerator = foldr cat0 enumerateNull $ map enumerateFile files
  enumerator countLinesIterM >>= getResult0

-- Compare @countLines@ and @countLines@
--
-- > ghci > countLines "/usr/include"
-- > 645463
-- > ghci > countLines0 "/usr/include"
-- > 598333
--
--
-- For some reason the two result above differ. And none of them seem to right...
--
-- > bash > find /usr/include -type f -print | xargs cat | wc -l
-- >  597074
--
-- > ghci > countLines "/Users/damian.nadales/Documents/"
-- > 11513208
-- > ghci >countLines0 "/Users/damian.nadales/Documents/"
-- > 11513208
-- >
instance MonadIO Iter where
  liftIO io = Iter $ \c -> NeedIO $ try io >>= mkResult c
    where mkResult _ (Left e)  = return (Failed e)
          mkResult c (Right a) = return (Done a c)

-- Exercise 6: write a function that uses @enumerateFile@ to dump the contents
-- of a file to memory.
dumpFile :: FilePath -> IO ()
dumpFile path = enumerateFile path iterStdout >>= getResult0


-- Return chunk that is non-empty of has EOF set
iterChunk :: Iter Chunk
iterChunk =
  Iter $ \c@(Chunk buf eof) ->
           if L.null buf && not eof
           then NeedInput iterChunk
           else Done c (Chunk L.empty eof)

-- Dump input to standard output
iterStdout :: Iter ()
iterStdout = do
  (Chunk buf eof) <- iterChunk
  liftIO $ L.putStr buf
  unless eof iterStdout

-- * Inner pipeline stages

-- An enumerator could be an iteratee as well. So let's define it:
type Inum a = Iter a -> Iter (Result a)

inumFile0 :: FilePath -> Inum a
inumFile0 path iter = liftIO $ enumerateFile path iter

-- | Exercise 7: fix @cat0@ to work with @Inums@.
cat :: Inum a -> Inum a -> Inum a
cat inum0 inum1 iter = inum0 iter >>= check
  where check (NeedInput iter') = inum1 iter'
        check (NeedIO io)       = liftIO io >>= check
        check r                 = return r


-- | Exercise 8: write an @Inum@ that acts as @xargs cat@.
xargsCat :: Inum a
xargsCat iter = do
  mpath <- readLine
  case mpath of
    Nothing   -> return (NeedInput iter)
    Just path -> inumFile0 (L8.unpack path) `cat` xargsCat $ iter

-- Fix getResult0 to work both with @IO@ and @Iter@ monads.
getResult :: MonadIO m => Result a -> m a
getResult (Done res _)     = return res
getResult (NeedInput iter) = getResult (runIter iter $ chunkEOF)
getResult (NeedIO io)      = liftIO io >>= getResult
getResult (Failed e)       = liftIO $ throwIO e

-- Define a pipe operator that hooks pipeline statges together.
(.|) :: Inum a -> Iter a -> Iter a
inum .| iter = inum iter >>= getResult
infixr 4 .|

-- Define a function to run an Iter in any @MonadIO@
run :: MonadIO m => Iter a -> m a
run iter = getResult (NeedInput iter)

-- And let's use these operators...
countLines2 :: FilePath -> IO Int
countLines2 path = run $ inumFile0 path .| countLinesIterM

-- * Define exception handling for Iter's.
iterCatch :: Iter a -> (SomeException -> Iter a) -> Iter a
iterCatch (Iter f0) handler = Iter (check . f0)
  where check (NeedInput iter0) = NeedInput (iterCatch iter0 handler)
        check (NeedIO io)       = NeedIO (liftM check io)
        check (Failed e)        = NeedInput (handler e)
        check done              = done

onFailed :: Iter a -> Iter b -> Iter a
onFailed iter cleanup = iterCatch iter (\e -> cleanup >> iterThrow e)

iterBracket :: Iter a -> (a -> Iter b) -> (a -> Iter c) -> Iter c
iterBracket before after action = do
  a <- before
  res <- action a `onFailed` after a
  _ <- after a
  return res

inumBracket :: Iter a -> (a -> Iter b) -> (a -> Inum c) -> Inum c
inumBracket before fafter finum iter = do
  iterBracket before fafter (flip finum iter)

-- * Simplyfying Inum construction
type Codec a = Iter (L.ByteString, Maybe (Inum a))

inumPure :: L.ByteString -> Inum a
inumPure buf (Iter f) = return (f (Chunk buf False))

runCodec :: Codec a -> Inum a
runCodec codec iter = do
  (input, mNext) <- codec
  maybe (inumPure input) (inumPure input `cat`) mNext $ iter

inumFile :: FilePath -> Inum a
inumFile path =
  inumBracket
  (liftIO $ openFile path ReadMode)
  (liftIO . hClose) $
  \h ->
    let inum = runCodec $ do
          input <- liftIO $ S.hGetSome h 32752
          let next = if S.null input then Nothing else Just inum
          return (L.fromChunks [input], next)
    in inum

-- Exercise 9: write @enumDir@ using @Inum@s.
enumDir :: FilePath -> Inum a
enumDir path =
  inumBracket
  (liftIO $ openDirStream path)
  (liftIO . closeDirStream) $
  \ds ->
    let inum = runCodec nextName
        nextName = do
          nextDir <- liftIO $ readDirStream ds
          checkName nextDir

        checkName ""   = return (L.empty, Nothing)
        checkName "."  = nextName
        checkName ".." = nextName
        checkName name = liftIO (getSymbolicLinkStatus newPath)
                         >>= checkStat newPath
          where newPath = path </> name

        checkStat path stat
          | isRegularFile stat =
            return (L8.pack $ path ++ "\n", Just inum)
          | isDirectory stat =
            return (L.empty, Just $ enumDir path `cat` inum)
          | otherwise = nextName

    in inum
