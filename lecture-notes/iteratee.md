
# Simple programming task: count lines

* Here's a Unix command to count lines in include files

    ~~~~
    find /usr/include -type f -print | xargs cat | wc -l
    ~~~~

* Let's implement the same thing in Haskell
    * Examples will require the following imports

        ~~~~ {.haskell}
        import Control.Exception
        import Control.Monad
        import qualified Data.ByteString.Strict as S
        import qualified Data.ByteString.Lazy as L
        import qualified Data.ByteString.Lazy.Char8 as L8
        import System.FilePath
        import System.Posix
        import System.IO.Unsafe    -- for understanding, not recommended
        ~~~~

    * Note in particular `Control.Monad` generalizes `liftM` to more args

        ~~~~ {.haskell}
        liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
        liftM f m1 = do { x1 <- m1; return (f x1) }

        liftM2  :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
        liftM2 f m1 m2 = do { x1 <- m1; x2 <- m2; return (f x1 x2) }

        -- also liftM3, liftM4, liftM5
        ~~~~

# Solution overview

* We need a function to lists all files under a directory recursively

    ~~~~ {.haskell}
    recDir :: FilePath -> IO [FilePath]
    ~~~~

    * We'll consider how to implement this function shortly

* We need a function to read the contents of a list of files

    ~~~~ {.haskell}
    readFiles :: [FilePath] -> IO L.ByteString
    readFiles [] = return L.empty
    readFiles (f:fs) = liftM2 L.append (L.readFile f)
                       (readFiles fs)
    ~~~~

* Can count newlines with `Data.ByteString.Lazy.count`
    * Actually use `.Char8` version to truncate `'\n'` to a `Word8`

    ~~~~ {.haskell}
    countLines :: FilePath -> IO ()
    countLines dir =
        recDir dir >>= readFiles >>= print . L8.count '\n'
    ~~~~

# Let's try this:

~~~~
*Main> countLines "/etc/rc.d"
4979
*Main> countLines "/usr/include"
*** Exception: /usr/include/dovecot/master-service-settings.h: 
openBinaryFile: resource exhausted (Too many open files)
~~~~

* Oops, what happened?  Let's investigate with using
  [`lsof`](http://people.freebsd.org/~abe/) utility

    ~~~~
    *Main> x <- readFiles ["/etc/motd", "/etc/resolv.conf"]
    *Main> :!lsof -c ghc
    ...
    ghc   4008   dm   7r   REG  8,3     0 2752575 /etc/motd
    ghc   4008   dm   8r   REG  8,3   152 2752562 /etc/resolv.conf
    *Main> L.length x
    152
    *Main> :!lsof -c ghc
    [gone]
    ~~~~

    * Lazy I/O in `L.readFile` causes files to be opened but not read
    * `L.length`, a supposedly pure function, causes files to be read
      and closed!
    * If we call `L.readFile` a lot without forcing I/O, run out of
      file descriptors

# Lazy I/O introduction

* Magic (evil) function `unsafeInterleaveIO` from
  [`System.IO.Unsafe`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/System-IO-Unsafe.html)

    ~~~~ {.haskell}
    unsafeInterleaveIO :: IO a -> IO a
    ~~~~

    * Returns a thunk that only performs the I/O when forced
    * I *don't* recommend using the function, but do recommend
      understanding it

* Let's look at the lazy `ByteString`
  [implementation](http://hackage.haskell.org/package/bytestring-0.10.6.0/docs/src/Data.ByteString.Lazy.Internal.html#ByteString)

    ~~~~ {.haskell}
    data ByteString = Empty
                    | Chunk {-# UNPACK #-} !S.ByteString ByteString

    readFile f = openBinaryFile f ReadMode >>= hGetContents

    hGetContents = hGetContentsN defaultChunkSize

    hGetContentsN k h = lazyRead
      where lazyRead = unsafeInterleaveIO loop
            loop = do c <- S.hGetSome h k
                      if S.null c
                        then hClose h >> return Empty
                        else do cs <- lazyRead
                                return (Chunk c cs)
    ~~~~

# Fixing `readFiles`

* `L.readFile` opens files immediately, closes when thunk evaluated
    * Why?  Because most errors happen on file open
    * Would be unintuitive call `L.length`, get "no such file or
      directory" exception

* One fix: delay file opens with `unsafeInterleaveIO`

    ~~~~ {.haskell}
    readFiles :: [FilePath] -> IO L.ByteString
    readFiles [] = return L.empty
    readFiles (f:fs) = liftM2 L.append (L.readFile f)
                       (unsafeInterleaveIO $ readFiles fs)
    ~~~~

    * Now doesn't open next file until previous one closed

        ~~~~
        *Main> x <- recDir "/etc/rc.d" >>= readFiles
        *Main> :!lsof -c ghc
        ... 
        ghc  10180   dm   8r   REG  8,3   894 2754867 /etc/rc.d/healthd
        *Main> L.index x 10000
        62
        *Main> :!lsof -c ghc
        ...
        ghc  10180   dm   8r   REG  8,3   779 2753245 /etc/rc.d/sshd
        ~~~~

# How to implement `recDir`?

* Need to list names in a directory
* Need to determine directories, and recurse into them
    * Other than directories, we'll ignore non-regular files
      (symlinks, pipes, etc.)

* Some useful functions from `System.Posix`:

    ~~~~ {.haskell}
    openDirStream :: FilePath -> IO DirStream
    readDirStream :: DirStream -> IO FilePath   -- "" at end of dir
    closeDirStream :: DirStream -> IO ()

    getSymbolicLinkStatus :: FilePath -> IO FileStatus     -- lstat
    isRegularFile :: FileStatus -> Bool
    isDirectory :: FileStatus -> Bool
    ~~~~

* A useful combinator from `System.Filepath`

    ~~~~ {.haskell}
    (</>) :: FilePath -> FilePath -> FilePath  -- concatenate paths
    ~~~~

* Let's try to do this lazily
    * Recurse over huge directory tree without running out of memory

# lazy `recDir` -- first attempt

~~~~ {.haskell}
recDir :: FilePath -> IO [FilePath]
recDir dir = do
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
~~~~

# testing `recDir`

~~~~
*Main> countLines "/usr/include"
3774172
*Main> x <- recDir "/usr/include"
*Main> :!lsof -c ghc
...
ghc   9412   dm   7r  DIR  254,0 45056    15 /usr/include
*Main> length x
19568
*Main> :!lsof -c ghc
[gone]
~~~~

so far so good, but...

~~~~
*Main> x <- recDir "/etc"
*Main> length x
*** Exception: /etc/sudoers.d: openDirStream:
permission denied (Permission denied)
*Main> :!lsof -c ghc
...
ghc   9817   dm   7r  DIR  254,0 12288 146200 /etc
*Main> 
~~~~

Oops... length threw an exception and now we've leaked a file
descriptor!



# lazy `recDir` -- second attempt

~~~~ {.haskell}
recDir2 :: FilePath -> IO [FilePath]
recDir2 dir = do
  ds <- openDirStream dir
  let protect m = m `onException` closeDirStream ds

      nextName = unsafeInterleaveIO $
                 protect (readDirStream ds) >>= checkName

      checkName "" = closeDirStream ds >> return []
      checkName "." = nextName
      checkName ".." = nextName
      checkName name = getSymbolicLinkStatus path >>= checkStat path
          where path = dir </> name

      checkStat path stat
          | isRegularFile stat = liftM (path :) nextName
          | isDirectory stat =
              liftM2 (++) (protect $ recDir2 path) nextName
          | otherwise = nextName

  nextName
~~~~

* Add `protect` function to catch exceptions

# Testing `recDir2`

~~~~
*Main> x <- recDir2 "/etc"
*Main> length x
*** Exception: /etc/sudoers.d: openDirStream:
permission denied (Permission denied)
*Main> :!lsof -c ghc
[no leaked fd]
~~~~

We've fixed one file descriptor leak, but exceptions at other times
can still leak descriptors...

~~~~
*Main> :!mkdir -p /tmp/perm/perm/perm; chmod 0 /tmp/perm/perm/perm
*Main> recDir2 "/tmp/perm"
*Main> :!lsof -c ghc
...
ghc  7337  dm    8r   DIR   0,17    60 82955 /tmp/perm
~~~~

~~~~
*Main> countLines2 "/etc"
*** Exception: /etc/avenger/dh1024.pem: openBinaryFile:
permission denied (Permission denied)
*Main> :!lsof -c ghc
...
ghc  8102  dm    7r   DIR  253,5 12288 393217 /etc
ghc  8102  dm    8r   DIR  253,5  4096 393227 /etc/avenger
~~~~

# Pitfalls of lazy I/O

* Lazy I/O is nice for prototyping or quick scripts
    * Maybe just want quick line count, don't care about leaked fds
* But Haskell is supposed to be a pure functional language...
    * Evaluating *functions* shouldn't cause file I/O to happen
* It also interacts very poorly with error handling
    * I/O simply has more failure modes than computing over memory
    * Pretending I/O isn't happening makes it hard to deal with errors
      sensibly
* Lazy I/O makes it really easy to consume huge amounts of memory

    * E.g., this works fine:

        ~~~~
        *Main> recDir2 "/usr/include" >>= readFiles >>= print . L.length
        154732979
        ~~~~

    * While this makes GHC consume an extra 150 MB of memory... oops!

        ~~~~
        *Main> x <- recDir2 "/usr/include" >>= readFiles
        *Main> L.length x
        154732979
        ~~~~

# Why does Haskell even have lazy I/O?

* The illusion of a huge string is easy to compute over
    * Don't have to worry about buffer boundaries
    * Can provide a set of general-purpose functions (e.g.,
      `L.length`) that would otherwise have to know about
      application-specific buffering

* Remember how `netcat` used lazy I/O in
   [concurrency lecture](http://www.scs.stanford.edu/16wi-cs240h/slides/concurrency-slides.html#(34))?

    ~~~~ {.haskell}
      -- Copy data back and forth
      done <- newEmptyMVar
      forkIO $ (hGetContents h >>= putStr) `finally` putMVar done ()
      getContents >>= hPutStr h
      takeMVar done
    ~~~~

* `getContents >>= hPutStr h` is concise and pleasing
    * It looks a lot like a Unix pipeline, but Unix pipelines don't
    provide the illusion of huge buffers... just need flow control
* Can we build Haskell equivalent of `cat file | wc -l`?

# The iteratee abstraction [[Kiselyov]](http://okmij.org/ftp/Streams.html#iteratee)

* Let's introduce some terminology
    * We call a data source such as `cat` an **enumerator**
    * A data sink such as `wc` is an **iteratee**
    * Idea: enumerator *iterates* over data by folding data through
      the iteratee
* Iteratee concept introduced by
  [[Kiselyov]](http://okmij.org/ftp/Streams.html#iteratee)
* Currently three implementations of the ideas on hackage
    * [pipes](http://hackage.haskell.org/package/pipes) - Gabriel
      Gonzalez's package (probably best choice)
    * [iterIO](http://hackage.haskell.org/package/iterIO) - my
      implementation, best documented for learning, not actively
      maintained
    * [enumerator](http://hackage.haskell.org/package/enumerator) -
      second implementation, widely used
    * [iteratee](http://hackage.haskell.org/package/iteratee) -
      Kiselyov's implementation, fastest, hardest to understand
* Today's lecture patterned after
  [iterIO](http://hackage.haskell.org/package/iterIO)
    * However, we'll build things up from scratch
    * Code here: <http://cs240h.scs.stanford.edu/notes/miniIter.hs>


# Representing iteratees

* Let's think about pipeline stage `wc` in command `cat file | wc -l`?
* It consumes input, takes actions that are a function of the input
    * If input is not EOF, goes back and consumes more input
    * On EOF, causes I/O side-effects (writes line to stdout)
    * Finally returns an exit value
    * Could also conceivably fail
* Coding Haskell equivalent:

    ~~~~ {.haskell}
    data Chunk = Chunk { chunkData :: !L.ByteString
                       , chunkAtEOF :: !Bool } deriving (Show)

    newtype Iter a = Iter { runIter :: Chunk -> Result a }

    data Result a = Done { rResult :: a, rResidual :: Chunk }
                  | NeedInput !(Iter a)
                  | NeedIO !(IO (Result a))
                  | Failed !SomeException
    ~~~~

# Example: Reading a line of input

~~~~ {.haskell}
readLine :: Iter (Maybe L.ByteString)
readLine = Iter (go L.empty)
    where go acc (Chunk input eof)
              | not (L.null b) = Done (Just acca) (Chunk btail eof)
              | not eof        = NeedInput (Iter (go acca))
              | otherwise      = Done Nothing (Chunk acca eof)
              where (a, b) = L8.break (== '\n') input
                    acca = L.append acc a
                    btail = L.tail b
~~~~

* `readLine` returns `Just` next input line, or `Nothing` if no more
  `'\n'`
    * Processes input one `Chunk` at a time
    * `L8.break (== '\n')` splits input at first newline (if any)
    * `acc :: L.ByteString` keeps accumulating input while no `'\n'`
      found

# Enumerators

* An enumerator feeds data to an iteratee to get a result

    ~~~~ {.haskell}
    type Enumerator a = Iter a -> IO (Result a)
    ~~~~

    * Or with Rank2Types might use `forall a. Iter a -> IO (Result a)`

* For example, could feed the contents of a file like this:

    ~~~~ {.haskell}
    enumerateFile :: FilePath -> Enumerator a
    enumerateFile path iter0 =
        bracket (openFile path ReadMode) hClose $ \h ->
        let go iter = do
              input <- S.hGetSome h 32752
              if S.null input
                then return (NeedInput iter)
                else check $ runIter iter $
                     Chunk (L.fromChunks [input]) False
            check (NeedInput iter) = go iter
            check (NeedIO iter)    = iter >>= check
            check result           = return result
        in go iter0
    ~~~~

    * Leave `chunkAtEOF` `False` to keep possibility of concatenating
      files

# Running iteratees


* Simple function to extract result of an iteratee:

    ~~~~ {.haskell}
    chunkEOF :: Chunk
    chunkEOF = Chunk L.empty True

    getResult0 :: Result a -> IO a
    getResult0 (Done a _)           = return a
    getResult0 (NeedInput (Iter f)) = getResult0 (f chunkEOF)
    getResult0 (NeedIO io)          = io >>= getResult0
    getResult0 (Failed e)           = throwIO e
    ~~~~

* For example, a complicated way to get first line of a file...

    ~~~~ {.haskell}
    *Main> enumerateFile "/etc/resolv.conf" readLine >>= getResult0
    Just (Chunk "search scs.stanford.edu" Empty)
    ~~~~

# Calling iteratees from other iteratees

* Our actual goal is to count lines, so want an `Iter Int`
* Let's leverage `readLine` to build this

    ~~~~ {.haskell}
    nlines0 :: Iter Int
    nlines0 = Iter (go 0)
        where go n c0 = check (runIter readLine c0)
                  where
                    check (NeedInput (Iter f)) =
                        NeedInput (Iter (check . f))
                    check (Done (Just _) c) = go (n + 1) c
                    check (Done Nothing c)  = Done n c
                    check (NeedIO r)        = NeedIO (liftM check r)
                    check (Failed e)        = Failed e
    ~~~~

* This works!

    ~~~~
    *Main> enumerateFile "/etc/resolv.conf" nlines0 >>= getResult0
    4
    ~~~~

    * But seriously?  What a yucky way to count lines!
    * What if there were an easy way to implement one `Iter` in terms
      of another?


# Make `Iter` into a `Monad`!

~~~~ {.haskell}
instance Monad Iter where
    return a = Iter $ Done a
    m >>= k = Iter $ \c -> check (runIter m c)
        where check (Done a c)     = runIter (k a) c
              check (NeedInput m') = NeedInput (m' >>= k)
              check (NeedIO io)    = NeedIO (liftM check io)
              check (Failed e)     = Failed e
    fail msg = iterThrow (ErrorCall msg)

iterThrow :: (Exception e) => e -> Iter a
iterThrow e = Iter $ \_ -> Failed (toException e)
~~~~

* Each `Iter` action consumes some input and returns a result

* Monads let us completely hide the details of residual input!

    ~~~~ {.haskell}
    nlines1 :: Iter Int
    nlines1 = go 0
        where go n = readLine >>= check n
              check n (Just _) = go $! n + 1
              check n Nothing  = return n
    ~~~~


# Counting lines revisited

* Let's implement a function to concatenate enumerators

    ~~~~ {.haskell}
    cat0 :: Enumerator a -> Enumerator a -> Enumerator a
    cat0 a b iter = a iter >>= check
        where check (NeedInput iter') = b iter'
              check (NeedIO io)       = io >>= check
              check r                 = return r
    ~~~~

* This gives us what we need to count lines again:

    ~~~~ {.haskell}
    enumerateNull :: Enumerator a
    enumerateNull = return . NeedInput

    countLines0 :: FilePath -> IO Int
    countLines0 dir = do
      files <- recDir dir
      let enumerator = foldr cat0 enumerateNull $
                       map enumerateFile files
      enumerator nlines1 >>= getResult0
    ~~~~

    * This is obviously a bit less efficient, since it creates a bunch
      of garbage for each line, but we are guaranteed no leaked file
      descriptors or memory

# The `MonadIO` class

* Module
  [`Control.Monad.Trans`](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-Trans.html)
  defines an important class `MonadIO`

    ~~~~ {.haskell}
    class (Monad m) => MonadIO m where
        liftIO :: IO a -> m a
    ~~~~

    * Defines a class of monads in which you can execute IO actions
    * Use `liftIO` to write code that works in multiple Monads

* Trivial example

    ~~~~ {.haskell}
    instance MonadIO IO where liftIO = id
    ~~~~

* Let's make `Iter` an instance of `MonadIO`


    ~~~~ {.haskell}
    instance MonadIO Iter where
        liftIO io = Iter $ \c -> NeedIO $ try io >>= mkResult c
            where mkResult _ (Left e)  = return (Failed e)
                  mkResult c (Right a) = return (Done a c)
    ~~~~

# More simple iteratees

~~~~ {.haskell}
-- Return chunk that is non-empty of has EOF set
iterChunk :: Iter Chunk
iterChunk = Iter $ \c@(Chunk buf eof) ->
            if L.null buf && not eof
            then NeedInput iterChunk
            else Done c (Chunk L.empty eof)

-- Dump input to standard output
iterStdout :: Iter ()
iterStdout = do
  (Chunk buf eof) <- iterChunk
  liftIO $ L.putStr buf
  unless eof iterStdout
~~~~

* Very useful for debugging enumerators

~~~~
*Main> enumerateFile "/etc/issue" iterStdout >>= getResult0
Arch Linux \r  (\n) (\l)

*Main>
~~~~


# Inner pipeline stages

* Unix pipelines can consist of more than two stages

    ~~~~
    find /usr/include -type f -print | xargs cat | wc -l
    ~~~~

    * `xargs cat` takes filenames as input and produces contents as
      output

    * So it's both an iteratee and an enumerator.  Call it an `Inum`:

    ~~~~ {.haskell}
    type Inum a = Iter a -> Iter (Result a)
    ~~~~

* Let's get rid of `Enumerator` as `Inum` is more general

    * For example, an `Inum` that enumerates a file is just an `Iter`
      that happens to consume no input:

    ~~~~ {.haskell}
    inumFile0 :: FilePath -> Inum a
    inumFile0 path iter = liftIO $ enumerateFile path iter
    ~~~~

# `Inum` examples

* Let's fix `cat0` to work with `Inum`s

    ~~~~ {.haskell}
    cat :: Inum a -> Inum a -> Inum a
    cat a b iter = a iter >>= check
        where check (NeedInput iter') = b iter'
              check (NeedIO io)       = liftIO io >>= check
              check r                 = return r
    ~~~~

    * (Actually works for `Enumerator`s too if we get rid of type
      signature)


* Example:  an `Inum` that acts like `xargs cat` command

    ~~~~ {.haskell}
    xargsCat :: Inum a
    xargsCat iter = do
      mpath <- readLine
      case mpath of
        Nothing   -> return (NeedInput iter)
        Just path -> inumFile (L8.unpack path) `cat` xargsCat $ iter
    ~~~~

    * Because `nextFile` is an `Iter`, it can consume input
    * But it also generates output that it feeds to an `Iter`

# Building pipelines

* Let's fix `getResult0` to work in both the `IO` and `Iter` monads:

    ~~~~ {.haskell}
    getResult :: (MonadIO m) => Result a -> m a
    getResult (Done a _)           = return a
    getResult (NeedInput (Iter f)) = getResult (f chunkEOF)
    getResult (NeedIO io)          = liftIO io >>= getResult
    getResult (Failed e)           = liftIO $ throwIO e
    ~~~~

* Now let's define a pipe operator to hook pipeline stages together

    ~~~~ {.haskell}
    (.|) :: Inum a -> Iter a -> Iter a
    (.|) inum iter = inum iter >>= getResult
    infixr 4 .|
    ~~~~

* And a function to let us run an `Iter` in any `MonadIO` monad

    ~~~~ {.haskell}
    run :: (MonadIO m) => Iter a -> m a
    run = getResult . NeedInput
    ~~~~

* Wow this is starting to look more like command pipelines!

    ~~~~
    *Main> run $ inumFile "/etc/mtab" .| countLines1
    12
    ~~~~

# Exception handling

* Let's write exception functions analogous to standard `IO` ones

~~~~ {.haskell}
iterCatch :: Iter a -> (SomeException -> Iter a) -> Iter a
iterCatch (Iter f0) handler = Iter (check . f0)
    where check (NeedInput (Iter f)) = NeedInput (Iter (check . f))
          check (NeedIO io)          = NeedIO (liftM check io)
          check (Failed e)           = NeedInput (handler e)
          check done                 = done

onFailed :: Iter a -> Iter b -> Iter a
onFailed iter cleanup = iter `iterCatch` \e -> cleanup >> iterThrow e

iterBracket :: Iter a -> (a -> Iter b) -> (a -> Iter c) -> Iter c
iterBracket before after action = do
  a <- before
  b <- action a `onFailed` after a
  after a
  return b

inumBracket :: Iter a -> (a -> Iter b) -> (a -> Inum c) -> Inum c
inumBracket before after inum iter =
    iterBracket before after (flip inum iter)
~~~~

# Simplifying `Inum` construction

* `Inum`s still hard to write... why not build them from `Iter`s?
    * Introduce a `Codec` which returns data and an optional next `Inum`

    ~~~~ {.haskell}
    type Codec a = Iter (L.ByteString, Maybe (Inum a))

    inumPure :: L.ByteString -> Inum a
    inumPure buf (Iter f) = return (f (Chunk buf False))

    runCodec :: Codec a -> Inum a
    runCodec codec iter = do
      (input, mNext) <- codec
      maybe (inumPure input) (inumPure input `cat`) mNext $ iter
    ~~~~

* Example:

    ~~~~ {.haskell}
    inumFile  :: FilePath -> Inum a
    inumFile path = inumBracket (liftIO $ openFile path ReadMode)
                    (liftIO . hClose) $ \h ->
        let inum = runCodec $ do
              input <- liftIO $ S.hGetSome h 32752
              let next = if S.null input then Nothing else Just inum
              return (L.fromChunks [input], next)
        in inum
    ~~~~

# Example: `enumDir`

~~~~ {.haskell}
enumDir :: FilePath -> Inum a
enumDir dir = inumBracket (liftIO $ openDirStream dir)
              (liftIO . closeDirStream) $ \ds ->
  let inum = runCodec nextName
      nextName = liftIO (readDirStream ds) >>= checkName

      checkName "" = return (L.empty, Nothing)
      checkName "." = nextName
      checkName ".." = nextName
      checkName name = liftIO (getSymbolicLinkStatus path)
                       >>= checkStat path
          where path = dir </> name

      checkStat path stat
          | isRegularFile stat =
              return (L8.pack $ path ++ "\n", Just inum)
          | isDirectory stat =
              return (L.empty, Just $ enumDir path `cat` inum)
          | otherwise = nextName
  in inum
~~~~

~~~~
*Main> run $ enumDir "/etc/rc.d" .| xargsCat .| nlines1
4588
~~~~

# The `MonadPlus` class

* `Control.Monad` defines another important class,
  [`MonadPlus`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Monad.html#t:MonadPlus):

    ~~~~ {.haskell}
    class Monad m => MonadPlus m where
       mzero :: m a 
       mplus :: m a -> m a -> m a
    ~~~~

    * For
      [`Alternative`](http://hackage.haskell.org/package/base/docs/Control-Applicative.html#t:Alternative)s
      that are also `Monads`:

        ~~~~ {.haskell}
        instance MonadPlus T where
          mzero = empty
          mplus = <|>
        ~~~~

    * Represents monads that let you try an alternative after a
      failure

* Example:  `Maybe`

    ~~~~ {.haskell}
    instance MonadPlus Maybe where
       mzero = Nothing
       Nothing `mplus` ys = ys
       xs      `mplus` _  = xs
    ~~~~

    * ``ma `mplus` mb `mplus` mc`` is handy way to get first
      non-`Nothing` value

# Making `Iter` a `MonadPlus`

~~~~ {.haskell}
instance MonadPlus Iter where
    mzero = fail "mzero"
    mplus itera0 iterb = go mempty itera0
        where go acc itera = Iter $ \c ->
                  let acc' = mappend acc c
                      check (NeedInput i) = NeedInput (go acc' i)
                      check (NeedIO io) = NeedIO (liftM check io)
                      check (Failed _) = runIter iterb acc'
                      check r = r
                  in check $ runIter itera c
~~~~

* Re-run second iter on same input
    * Allows you to handle parse alternatives
    * iterIO contains parsing combinator library based on this idea
    * Allows protocol implementations to look like grammar
* See
  [`Data.IterIO.Parse`](http://hackage.haskell.org/packages/archive/iterIO/0.2/doc/html/Data-IterIO-Parse.html)
  for examples of parsing combinators

