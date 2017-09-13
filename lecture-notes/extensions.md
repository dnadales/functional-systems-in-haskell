
# GHC Language extensions

* GHC implements many extensions to Haskell, enabled by
    * Placing `{-# LANGUAGE` *ExtensionName* `#-}` at top of file
      (recommended)
    * Compiling with `-X`*ExtensionName* (less recommended, except for
      `-XSafe`)
    * Typing `:set -X`*ExtensionName* at `ghci` prompt (or running
      `ghci` with `-X`...)
* Complete list at [Language options][LanguageOptions] section of
      GHC's option summary
* Some extensions are very safe to use
    * E.g., core libraries depend on extension in a deep way
    * Extension very superficial, easily de-sugars into Haskell2010
<!--
    * Extension restricts rather than expands set of permissible programs
-->
* Other extensions less widely accepted
    * E.g., makes type inference/checking undecidable or non-deterministic
    * Undermines type safety
    * A work in progress that could never be incorporated into standard 
* Many extensions in a middle/gray area

# Background: Monad transformers

* Type constructors building monads parameterized by other monads
    * Method
     [`lift`](http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/Control-Monad-Trans-Class.html#t:MonadTrans)
     executes actions from underlying transformed monad:

    ~~~~ {.haskell}
    class MonadTrans t where
        lift :: Monad m => m a -> t m a
    ~~~~

    * Note monads have kind &#x2217; &#x2192; &#x2217;, so
      transformers have kind (&#x2217; &#x2192; &#x2217;) &#x2192;
      &#x2217; &#x2192; &#x2217;

* Example:  State transformer monad,
  [`StateT`](http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/Control-Monad-Trans-State-Lazy.html#v:StateT)
<!--
    * Transformer version of
      [`State` from old lecture](http://cs240h.scs.stanford.edu/notes/monads-slides.html#(32))--can
      combine, e.g., state and `IO`
-->

    ~~~~ {.haskell}
    newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

    instance (Monad m) => Monad (StateT s m) where
        return a = StateT $ \s -> return (a, s)
        m >>= k  = StateT $ \s0 -> do          -- in monad m
                     ~(a, s1) <- runStateT m s0
                     runStateT (k a) s1

    instance MonadTrans (StateT s) where
        lift ma = StateT $ \s -> do            -- in monad m
                    a <- ma
                    return (a, s)
    ~~~~

# Using `StateT`

* `get` and `put` allow you to modify state

    ~~~~ {.haskell}
    get :: (Monad m) => StateT s m s
    put :: (Monad m) => s -> StateT s m ()
    ~~~~

* Example: Haskell equivalent of `x++` in C

    ~~~~ {.haskell}
    import Control.Monad.Trans
    import Control.Monad.Trans.State

    main :: IO ()
    main = runStateT go 0 >>= print
      where go = do xplusplus >>= lift . print
                    xplusplus >>= lift . print
            xplusplus = do n <- get; put (n + 1); return n
    ~~~~

    ~~~~
    *Main> main
    0
    1
    ((),2)
    ~~~~

<!--
# More complex `StateT` example

* Example: count lines of standard input

    ~~~~ {.haskell}
    import Control.Exception
    import Control.Monad
    import Control.Monad.Trans
    import Control.Monad.Trans.State

    countLines :: IO Int
    countLines = liftM fst $ runStateT go (0::Int)
      where go = lift (try getLine) >>= doline
            doline (Left (SomeException _)) = get
            doline (Right _) = do n <- get; put (n + 1); go
    ~~~~

    * Note that `try getLine` is an `IO` action, executed with `lift`
    * Mixed with `IO` are `get`, `set` actions from `StateT Int IO` monad
-->

# Exercise: Implement `get` and `put`

* Recall `StateT` implementation

~~~~ {.haskell}
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s)
    m >>= k  = StateT $ \s0 -> do          -- in monad m
                 ~(a, s1) <- runStateT m s0
                 runStateT (k a) s1
~~~~

* How to implement the following?

~~~~ {.haskell}
get :: (Monad m) => StateT s m s


put :: (Monad m) => s -> StateT s m ()

~~~~

# Exercise: Implement `get` and `put`

* Recall `StateT` implementation

~~~~ {.haskell}
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s)
    m >>= k  = StateT $ \s0 -> do          -- in monad m
                 ~(a, s1) <- runStateT m s0
                 runStateT (k a) s1
~~~~

* How to implement the following?

~~~~ {.haskell}
get :: (Monad m) => StateT s m s
get = StateT $ \s -> return (s, s)

put :: (Monad m) => s -> StateT s m ()
put s = StateT $ \_ -> return ((), s)
~~~~



# The `MonadIO` class

* Remember `liftIO`?  Lets you execute IO regardless of current monad

~~~~ {.haskell}
class (Monad m) => MonadIO m where
    liftIO :: IO a -> m a

instance MonadIO IO where
    liftIO = id
~~~~

* Let's make `liftIO` work for `StateT`

    ~~~~ {.haskell}
    instance (MonadIO m) => MonadIO (StateT s m) where
        liftIO = lift . liftIO
    ~~~~

* Now can write functions that use IO and work in many monads:

    ~~~~ {.haskell}
    myprint :: (Show a, MonadIO m) => a -> m ()
    myprint a = liftIO $ print $ show a
    ~~~~


* All standard Monad transformers implement class `MonadIO`
    * `ContT`, `ErrorT`, `ListT`, `RWST`, `ReaderT`, `StateT`, `WriterT`, ...

# Background: recursive bindings

* Top-level, `let`, and `where` bindings are all recursive in Haskell,
  e.g.:

    ~~~~ {.haskell}
    oneTwo :: (Int, Int)
    oneTwo = (fst y, snd x)
        where x = (1, snd y)    -- mutual recursion
              y = (fst x, 2)

    nthFib :: Int -> Integer
    nthFib n = fibList !! n
        where fibList = 1 : 1 : zipWith (+) fibList (tail fibList)
    ~~~~

* Recursion can be implemented using a fixed-point combinator
    * Function
      [`fix`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Function.html#v:fix)
      calls a function with its own result, use to re-implement above:

        ~~~~ {.haskell}
        fix :: (a -> a) -> a
        fix f = let x = f x in x
        ~~~~

        ~~~~ {.haskell}
        oneTwo' :: (Int, Int)
        oneTwo' = (fst y, snd x)
            where (x, y) = fix $ \ ~(x0, y0) -> let x1 = (1, snd y0)
                                                    y1 = (fst x0, 2)
                                                in (x1, y1)
        nthFib' n = fibList !! n
            where fibList = fix $ \l -> 1 : 1 : zipWith (+) l (tail l)
        ~~~~

# Recursion and monadic bindings

* By contrast, monadic bindings are *not* recursive

    ~~~~ {.haskell}
    do fibList <- return $ 1 : 1 : zipWith (+) fibList (tail fibList)
       ...     -- error, fibList not in scope  ^^^^^^^       ^^^^^^^
    ~~~~

* But monads in the [`MonadFix`][] class have a fixed-point combinator

    ~~~~ {.haskell}
    class Monad m => MonadFix m where
        mfix :: (a -> m a) -> m a
    ~~~~

    * `mfix` can be used to implement recursive monadic bindings
      [[Erk&#xf6;k00]][erkok], e.g.:

    ~~~~ {.haskell}
    mfib :: (MonadFix m) => Int -> m Integer
    mfib n = do
      fibList <- mfix $ \l -> return $ 1 : 1 : zipWith (+) l (tail l)
      return $ fibList !! n -- ^^^^^
    ~~~~

* Why?  E.g., might want to simulate circuits with monads
    * Need recursion if there is a loop in your circuit
    * Might want recursion anyway to avoid worrying about order of statements

# The [`RecursiveDo`][] extension

* New `rec` keyword introduces recursive bindings in a `do` block
  [[Erk&#xf6;k02]][mdo]
    * Monad must be an instance of `MonadFix` (`rec` desugars to `mfix` calls)

    ~~~~ {.haskell}
    oneTwo'' :: (MonadFix m) => m (Int, Int)
    oneTwo'' = do
      rec x <- return (1, snd y)
          y <- return (fst x, 2)
      return (fst y, snd x)
    ~~~~

    * Desugars to:

    ~~~~ {.haskell}
    oneTwo''' :: (MonadFix m) => m (Int, Int)
    oneTwo''' = do
      (x, y) <- mfix $ \ ~(x0, y0) -> do x1 <- return (1, snd y0)
                                         y1 <- return (fst x0, 2)
                                         return (x1, y1)
      return (fst y, snd x)
    ~~~~

* In practice `RecursiveDo` helps structure thinking
    * Then can manually desugar rather than require a language extension
    * But `mfix` on its own is quite useful

# Example uses of `mfix` and `rec`

* Create recursive data structures in one shot

    ~~~~ {.haskell}
    data Link a = Link !a !(MVar (Link a)) -- note ! is okay

    mkCycle :: IO (MVar (Link Int))
    mkCycle = do
      rec l1 <- newMVar $ Link 1 l2        -- but $! would diverge
          l2 <- newMVar $ Link 2 l1
      return l1
    ~~~~

* Call non-strict methods of classes (easy access to return-type dictionary)

    ~~~~ {.haskell}
    class MyClass t where
        myTypeName :: t -> String        -- non-strict in argument
        myDefaultValue :: t
    instance MyClass Int where
        myTypeName _ = "Int"
        myDefaultValue = 0

    getVal :: (MyClass t) => IO t
    getVal = mfix $ \t -> do      -- doesn't use mfix's full power
      putStrLn $ "Caller wants type " ++ myTypeName t
      return myDefaultValue
    ~~~~

# Implementing `mfix`

* Warm-up: The [`Identity`](http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/Data-Functor-Identity.html#v:Identity) monad

    ~~~~ {.haskell}
    newtype Identity a = Identity { runIdentity :: a }
    instance Monad Identity where
        return = Identity
        m >>= k = k (runIdentity m)
    ~~~~

    * `newtype` compiles to nothing, so basically same as `fix`:

    ~~~~ {.haskell}
    instance MonadFix Identity where
        mfix f = let x = f (runIdentity x) in x
    ~~~~

# `fixIO` -- `IO` Monad fixed point

* Remember magic [`unsafeInterleaveIO`] used for lazy IO?

    ~~~~ {.haskell}
    unsafeInterleaveIO :: IO a -> IO a
    ~~~~

    * Looks like an `IO` identify function, but defers IO until the thunk forced
    * Danger---don't try this at home!  No longer a functional language

        ~~~~ {.haskell}
        weird :: IO String
        weird = do
          xxx <- unsafeInterleaveIO $ do putStrLn "Gotcha!"; return []
          return $ 'a':'b':'c':xxx
        ~~~~

* For `IO`, `mfix = fixIO`:

    ~~~~ {.haskell}
    fixIO :: (a -> IO a) -> IO a
    fixIO k = do
        ref <- newIORef (throw NonTermination)
        ans <- unsafeInterleaveIO (readIORef ref)
        result <- k ans
        writeIORef ref result
        return result
    ~~~~

    * This is quite similar to what the compiler does for pure `fix`

# A generic `mfix` is not possible

* What if we tried to define an `mfix`-like function for all monads?

    ~~~~ {.haskell}
    mbroken :: (Monad m) => (a -> m a) -> m a -- equivalent to mfix?
    mbroken f = fix (>>= f)
    ~~~~

    * This is equivalent to

    ~~~~ {.haskell}
    mbroken f = mbroken f >>= f
    ~~~~

    * But `>>=` is strict in its first argument for many monads, so

    ~~~~
    *Main> mfix $ const (return 0)
    0
    *Main> mbroken $ const (return 0)
    *** Exception: stack overflow
    ~~~~

* So `mfix` needs to take fixed point over value, not over monadic action
    * How to do this is monad-specific
    * Doesn't work for all monads (`ContT`, `ListT`)

# `MonadFix` instance for `StateT`

* What about the [`StateT`](http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/Control-Monad-Trans-State-Lazy.html#t:StateT) monad?

    ~~~~ {.haskell}
    newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

    instance (Monad m) => Monad (StateT s m) where
        return a = StateT $ \s -> return (a, s)
        m >>= k  = StateT $ \s0 -> do          -- in monad m
                     ~(a, s1) <- runStateT m s0
                     runStateT (k a) s1

    ~~~~

    * Possibly easiest to see using `rec` notation

    ~~~~ {.haskell}
    instance MonadFix m => MonadFix (StateT s m) where
        mfix f = StateT $ \s0 -> do            -- in monad m
                   rec ~(a, s1) <- runStateT (f a) s0
                   return (a, s1)
    ~~~~

    * But easily implemented with no language extensions

    ~~~~ {.haskell}
    instance MonadFix m => MonadFix (StateT s m) where
        mfix f = StateT $ \s -> mfix $ \ ~(a, _) -> runStateT (f a) s
    ~~~~


# Review: Type classes

* A [Haskell 2010 type class declaration][typeclasses] can take the form:

    ~~~~ {.haskell}
    class ClassName var where
        methodName :: Type {- where type references var -}
    ~~~~

    ~~~~ {.haskell}
    class (SuperClass var) => ClassName var where ...
    class (Super1 var, Super2 var) => ClassName var where ...
    ...
    ~~~~

    * Note that `var` need not have kind &#x2217;
    * However, the type of each method must mention `var` and an
      implicit `(Classname var)` is added to the context of each
      method, e.g.:

        ~~~~ {.haskell}
        Prelude> :t return
        return :: Monad m => a -> m a
        ~~~~

* A [Haskell 2010 instance declaration][instances] has the form:

    ~~~~ {.haskell}
    instance [context =>] ClassName (TypeCon v1 ... vk) where ...
    ~~~~

    * Note `v1` ... `vk` are all variables and all distinct, ruling
      out, e.g., `instance C (a,a)` or `instance C (Int a)` or
      `instance [[a]]`

# [`MultiParamTypeClasses`][] extension

* Enables type classes with multiple parameters, E.g.:

    ~~~~ {.haskell}
    {-# LANGUAGE MultiParamTypeClasses #-}
    class Convert a b where convert :: a -> b
    instance Convert Int Bool where convert = (/= 0)
    instance Convert Int Integer where convert = toInteger
    instance (Convert a b) => Convert [a] [b] where
        convert = map convert
    ~~~~

* Extension itself is relatively safe, but encourages other extensions

    * E.g., each method's type must use every type parameter

        ~~~~ {.haskell}
        class MyClass a b where
            aDefault :: a  -- can never use (without more extensions...)
        ~~~~

    * All types (argument and return) must be fully determined

        ~~~~ {.haskell}
               convert 0 :: Bool   -- error, 0 has type (Num a) => a
        ~~~~

    * And the usual instance restrictions still apply

        ~~~~ {.haskell}
        instance Convert Int [Char] where convert = show  -- error bad param
        ~~~~

        * `[Char]`--i.e., `([] Char)`--is not a valid instance
          parameter, would have to be `([] a)`

# [`FlexibleInstances`] extension

* Allows more specific type paremeters (relatively safe extension)
    * E.g., now we can say:

    ~~~~ {.haskell}
    {-# LANGUAGE FlexibleInstances #-}

    instance Convert Int [Char] where
        convert = show
    ~~~~

    * And we can make all types convert to themselves:

    ~~~~ {.haskell}
    instance Convert a a where convert a = a
    ~~~~

    ~~~~
    *Main> convert () :: ()
    ()
    *Main> convert ([1,2,3]::[Int]) :: [Integer]
    [1,2,3]
    *Main> convert ([1,2,3]::[Int]) :: [Int]
    <interactive>:1:1:
        Overlapping instances for Convert [Int] [Int]
          instance Convert a a
          instance Convert a b => Convert [a] [b]
    ~~~~

    * Oops, two instances apply; GHC doesn't know which to choose

# [`OverlappingInstances`][] extension

* This extension is used, but also widely frowned upon
    * Only need this extension if overlapping instances actually used
    * Enable extension where instances defined, not where used
    * Compiler picks the most specific matching instance.  $I_1$ is
      more specific than $I_2$ when $I_1$ can be created by
      substituting for the variables of $I_2$ and not vice versa
    * <span style="color:red">Contexts (part before `=>`) not
        considered when selecting instances</span>
* Example:  Do something like `Show` for `String` vs. `[a]`

    ~~~~ {.haskell}
    class MyShow a where myShow :: a -> String
    instance MyShow Char where myShow = show
    instance MyShow Int where myShow = show
    instance MyShow [Char] where myShow = id
    instance (MyShow a) => MyShow [a] where
        myShow []     = "[]"
        myShow (x:xs) = "[" ++ myShow x ++ go xs
            where go (y:ys) = "," ++ myShow y ++ go ys
                  go []     = "]"
    ~~~~

* So does enabling `OverlappingInstances` fix `Convert`?

# Most specific instances

* What is the most specific instance?

    ~~~~ {.haskell}
    {-# LANGUAGE MultiParamTypeClasses #-}
    {-# LANGUAGE FlexibleInstances #-}
    {-# LANGUAGE OverlappingInstances #-}
    instance Convert a a where ...
    instance (Convert a b) => Convert [a] [b] where ...
    ~~~~

    ~~~~
    *Main> convert ([1,2,3]::[Int]) :: [Int]
    <interactive>:1:1:
        Overlapping instances for Convert [Int] [Int]
          instance [overlap ok] Convert a a
          instance [overlap ok] Convert a b => Convert [a] [b]
    ~~~~

    * Neither instance is most specific!
    * We have to add a *third* instance to break the tie--one that can
      be created by substituting for variables in either of the other
      two overlapping instances

    ~~~~ {.haskell}
    instance Convert [a] [a] where convert = id
    ~~~~

    ~~~
    *Main> convert ([1,2,3]::[Int]) :: [Int]
    [1,2,3]
    ~~~ 

# A case against `OverlappingInstances`

~~~~ {.haskell}
module Help where
    class MyShow a where
      myshow :: a -> String
    instance MyShow a => MyShow [a] where
      myshow xs = concatMap myshow xs

    showHelp :: MyShow a => [a] -> String
    showHelp xs = myshow xs     -- doesn't see overlapping instance

module Main where
    import Help

    data T = MkT
    instance MyShow T where
      myshow x = "Used generic instance"
    instance MyShow [T] where
      myshow xs = "Used more specific instance"

    main = do { print (myshow [MkT]); print (showHelp [MkT]) }
~~~~

~~~~
*Main> main
"Used more specific instance"
"Used generic instance"
~~~~

# Aside: How `Show` actually works

* Add an extra helper method, `showList`, with a default definition:

~~~~ {.haskell}
class Show a where
  show :: a -> String
  showList :: [a] -> ShowS
  showList as = '[' : intercalate ", " (map show as) ++ "]"
  -- Note actual implementation more efficient but equivalent

instance (Show a) => Show [a] where
  show as = showList as
~~~~

* `Show` instance for `Char` overrides default `showList`

* But had to plan all this out from the start
    * Want an easy way to special-case trees or other data structures
      besides lists?
    * Then you are stuck using overlapping instances

# [`FlexibleContexts`][] extension

* `MultiParamTypeClasses` leads to inexpressible types

    ~~~~ {.haskell}
    toInt val = convert val :: Int
    ~~~~

    * What is the type of function `toInt`?  Would like to write:

    ~~~~ {.haskell}
    toInt :: (Convert a Int) => a -> Int
    ~~~~

    * But `(Convert a Int) =>` is an illegal context, as `Int` not a
      type variable

* `FlexibleContexts` extension makes the above type legal to write
    * Is a relatively safe extension to use
* Still a couple of restrictions
    * Each type variable in context must be "reachable" from a type
      variable in type<br> (Reachable = explicitly used, or in another
      constraint with a reachable variable.)

        ~~~~ {.haskell}
        sym :: forall a. Eq a => Int   -- illegal
        ~~~~

    * Every constraint must have a type variable

        ~~~~ {.haskell}
        sym :: Eq Int => Bool          -- illegal
        ~~~~

# Monad classes

* It's neat that `liftIO` works from so many monads
    * Why not do something similar for `StateT`?  Make `get`/`set` methods

    ~~~~ {.haskell}
    {-# LANGUAGE MultiParamTypeClasses #-}
    {-# LANGUAGE FlexibleInstances #-}

    class (Monad m) => MonadState s m where
        get :: m s
        put :: s -> m ()
    instance (Monad m) => MonadState s (StateT s m) where
        get = StateT $ \s -> return (s, s)
        put s = StateT $ \_ -> return ((), s)
    ~~~~

* Now for each other `MonadTrans`, pass requests down

    * This is just like `liftIO`.  E.g., for `ReaderT`:

    ~~~~ {.haskell}
    instance (MonadIO m) => MonadIO (ReaderT r m) where
        liftIO = lift . liftIO

    instance (MonadState s m) => MonadState s (ReaderT r m) where
        get = lift get
        put = lift . put
    ~~~~

# Problem: we've defeated type inference

* Remember `xplusplus`?

    ~~~~ {.haskell}
            xplusplus = do n <- get; put (n + 1); return n
    ~~~~

    * The compiler knows we are in `StateT Int IO` monad
    * So can infer that the type of `get` is `Num s => StateT Int IO s`
    * But need to know `s` in order to select an instance of `MonadState`!
    * For all compiler knows, might be other matching instances, e.g.,

        ~~~~ {.haskell}
        instance MonadState Double (StateT Int IO) where
            -- would be legal, but exists only in compiler's imagination
        ~~~~

* Since compiler can't infer return type of `get`, must type it manually:

    ~~~~ {.haskell}
        xplusplus = do n <- get :: StateT Int IO Int
                       put (n + 1)
                       return n
    ~~~~

    * Yuck!  Lack of type inference gets old fast!

# [`FunctionalDependencies`][] extension

* Widely used & frowned upon (not as bad as `OverlappingInstances`)
    * Also referred to as "fundeps"
* Lets a class declare some parameters to be functions of others

    ~~~~ {.haskell}
    class (Monad m) => MonadState s m | m -> s where
        get :: m s
        put :: s -> m ()
    ~~~~

    * The best way to think of this is in terms of *instance selection*
    * "`| m -> s`" says can select an instance based on `m` without
      considering `s`, because **`s` is a function of `m`**
    * Once you've selected the instance, you can use `s` for type inference

* Disallows conflicting instances (even w. `OverlappingInstances`)
* Also allows arbitrary computation at the type level
  [[Hallgren]][funWithFundeps]
    * But language committee wants compilation to be decidable and
      deterministic
    * So need to add some restrictions

# [Sufficient conditions of decidable instances][instanceRules]

* Anatomy of an instance:  `instance` [*context* `=>`] *head* [`where` *body*]
    * *context* consists of zero or more comma-separated *assertions*

1.  The Paterson Conditions: for each assertion in the context

    a.  No type variable has more occurrences in the assertion than in
        the head

        ~~~~ {.haskell}
        class Class a b
        instance (Class a a) => Class [a] Bool  -- bad: 2 * a > 1 * a
        instance (Class a b) => Class [a] Bool  -- bad: 1 * b > 0 * b
        ~~~~

    b.  The assertion has fewer constructors and variables than the head

        ~~~~ {.haskell}
        instance (Class a Int) => Class a Integer   -- bad: 2 >= 2
        ~~~~

2.  The Coverage Condition:  For each fundep *left* `->` *right*, the
    types in *right* cannot have type variables not mentioned in
    *left*
 
    ~~~~ {.haskell}
    class Class a b | a -> b
    instance Class a (Maybe a)       -- ok: a "covered" by left
    instance Class Int (Maybe b)     -- bad: b not covered
    instance Class a (Either a b)    -- bad: b not covered
    ~~~~

# Undecidable vs. exponential -- who cares?

* Editorial: maybe decidability of language is overrated
    * Computers aren't Turing machines with infinite tapes, after all
* This legal, decidable program will crash your Haskell compiler

    ~~~~ {.haskell}
    crash = f5 ()
        where f0 x = (x, x)      -- type size 2^{2^0}
              f1 x = f0 (f0 x)   -- type size 2^{2^1}
              f2 x = f1 (f1 x)   -- type size 2^{2^2}
              f3 x = f2 (f2 x)   -- type size 2^{2^3}
              f4 x = f3 (f3 x)   -- type size 2^{2^4}
              f5 x = f4 (f4 x)   -- type size 2^{2^5}
    ~~~~

* While plenty of not _a priori_ provably decidable programs happily
  compile
    * The conditions of the last slide are *sufficient*, not *necessary*
    * Might have other ways of knowing your program can compile
    * Or maybe figure it out from trial and error?

# [`UndecidableInstances`][] extension

* Lifts the Paterson and Coverage conditions
    * Also enables `FlexibleContexts` when enabled
* Instead, imposes a maximum recursion depth
    * Default maximum depth is 20
    * Can increase with `-fcontext-stack=`*n* option, e.g.:

        ~~~~ {.haskell}
        {-# OPTIONS_GHC -fcontext-stack=1024 #-}
        {-# LANGUAGE UndecidableInstances #-}
        ~~~~

* A bit reminiscent of C++ templates
    * gcc has a `-ftemplate-depth=` option
    * Note C++11 raises minimum depth from 17 to 1024
    * Similarly, people have talked of increasing GHC's default
      context-stack

# `MonadIO` revisited

* Recall definition of `MonadIO`

    ~~~~ {.haskell}
    class (Monad m) => MonadIO m where
        liftIO :: IO a -> m a
    instance MonadIO IO where
        liftIO = id
    ~~~~

* Currently must define an instance for every transformer

    ~~~~ {.haskell}
    instance MonadIO m => MonadIO (StateT s m) where liftIO = lift . liftIO
    instance MonadIO m => MonadIO (ReaderT t m) where liftIO = lift . liftIO
    instance MonadIO m => MonadIO (WriterT w m) where liftIO = lift . liftIO
    ...
    ~~~~

* With `UndecidableInstances`, one instance can cover all transformers!

    ~~~~ {.haskell}
    {-# LANGUAGE FlexibleInstances #-}
    {-# LANGUAGE UndecidableInstances #-}

    -- undecidable: assertion Monad (t m) no smaller than head
    instance (MonadTrans t, MonadIO m, Monad (t m)) =>
        MonadIO (t m) where liftIO = lift . liftIO
    ~~~~

# Summary of extensions

* We've seen 6 typeclass-related extensions

    ~~~~ {.haskell}
    {-# LANGUAGE MultiParamTypeClasses #-}  -- very conservative
    {-# LANGUAGE FlexibleInstances #-}      -- conservative
    {-# LANGUAGE FlexibleContexts #-}       -- conservative
    {-# LANGUAGE FunctionalDependencies #-} -- frowned upon
    {-# LANGUAGE UndecidableInstances #-}   -- very frowned upon
    {-# LANGUAGE OverlappingInstances #-}   -- the most controversial
    ~~~~

    * Not all of these are looked upon kindly by the community
    * But if you enable all six, can be very powerful

* Remainder of lecture looks at what you can do with all 6 enabled
    * Much inspired by [[Hlist]][] and [[OOHaskell]][]

<!--
# Type-level natural numbers


~~~~ {.haskell}
data Zero = Zero      -- Type-level 0
data Succ n = Succ n  -- Type-level successor (n + 1)

class NatPlus a b c | a b -> c, a c -> b where
    natPlus :: a -> b -> c
    natMinus :: c -> a -> b

instance NatPlus Zero a a where
    natPlus _ a = a
    natMinus a _ = a

-- Note failure of coverage condition below
instance (NatPlus a b c) => NatPlus (Succ a) b (Succ c) where 
    natPlus (Succ a) b = (Succ (natPlus a b))
    natMinus (Succ c) (Succ a) = natMinus c a
~~~~

* Fundeps + Context let us compute recursively on types!
    * If context has assertion `NatPlus a b c`, then from types
    `Succ a` and `b` we can compute `Succ c` (computation at type level)


# Type-level booleans

~~~~ {.haskell}
data HFalse = HFalse deriving Show
data HTrue = HTrue deriving Show

class HNot a b | a -> b where hnot :: a -> b
instance HNot HFalse HTrue where hnot _ = HTrue
instance HNot HTrue HFalse where hnot _ = HFalse

class HEven a b | a -> b where hEven :: a -> b
instance HEven Zero HTrue where hEven _ = HTrue
instance (HEven n b, HNot b nb) => HEven (Succ n) nb where
    hEven (Succ n) = hnot (hEven n)
~~~~

~~~~
*Main> hEven Zero
HTrue
*Main> hEven (Succ Zero)
HFalse
*Main> hEven (Succ (Succ Zero))
HTrue
*Main> hEven (Succ (Succ (Succ Zero)))
HFalse
~~~~

* Note how we use assertion `HNot b nb` to compute negation of `b`

-->

# Warm-up: Type-level booleans

~~~~ {.haskell}
data HFalse = HFalse deriving Show
data HTrue = HTrue deriving Show

class HNot a b | a -> b where hNot :: a -> b
instance HNot HFalse HTrue where hNot _ = HTrue
instance HNot HTrue HFalse where hNot _ = HFalse
~~~~

~~~~
*Main> hNot HTrue
HFalse
*Main> hNot HFalse
HTrue
~~~~

* Note how fundep in `HNot b nb` computes negation of `b` **at the
  type level**
* Haven't used `OverlappingInstances` yet, let's start...

# Computing over types

* Can we compute whether two types are equal?  First attempt:

    ~~~~ {.haskell}
    class TypeEq a b c | a b -> c where typeEq :: a -> b -> c
    instance TypeEq a a HTrue where typeEq _ _ = HTrue
    instance TypeEq a b HFalse where typeEq _ _ = HFalse
    ~~~~

    * Problem: `TypeEq a a HTrue` not more specific than
      `TypeEq a b HFalse`
    * ... but `TypeEq a a HTrue` *is* more specific than `TypeEq a b c`

* Recall that context is never consulted for instance selection
    * Only afterwards to reject failed assertions or infer types from fundeps
    * Solution:  compute `c` after instance selection using another fundep

    ~~~~ {.haskell}
    class TypeCast a b | a -> b where typeCast :: a -> b
    instance TypeCast a a where typeCast = id

    instance TypeEq a a HTrue where typeEq _ _ = HTrue -- as before
    instance (TypeCast HFalse c) => TypeEq a b c where
        typeEq _ _ = typeCast HFalse
    ~~~~

# The utility of `TypeEq`

* Editorial: `TypeEq` is kind of the holy grail of fundeps
    * If you can implement `TypeEq`, you can program recursively at
      type level by distinguishing base and recursive cases!
    * But relies deeply on `OverlappingInstances`...
* Example: Let's do for `MonadState` what we did for `MonadIO`

    ~~~~ {.haskell}
    -- If t is StateT, then do one thing for (t s m) (base case):
    instance (Monad m) => MonadState s (StateT s m) where
        get = StateT $ \s -> return (s, s)
        put = StateT $ \_ -> return ((), s)
    -- If t is not StateT, do something else (recursive case):
    instance (MonadTrans t, MonadState s m, Monad (t m)) =>
        MonadState s (t m) where
            get = lift get
            put = lift . put
    ~~~~

    * `MonadIO` was easier because type `IO` can't match parameter `(t m)`
    * Unfortunately, `StateT s m` matches *both* of above instance heads
    * So need `OverlappingInstances` to select first instance for
      `StateT s m`

# Heterogeneous lists

* Last extension: [`TypeOperators`][] allows infix types starting with
  "`:`"

    ~~~~ {.haskell}
    data a :*: b = Foo a b
    type a :+: b = Either a b
    ~~~~

* Let's use an infix constructor to define a heterogeneous list

    ~~~~ {.haskell}
    data HNil = HNil deriving Show
    data (:*:) h t = h :*: !t deriving Show
    infixr 9 :*:

    -- Example:
    data A = A deriving Show
    data B = B deriving Show
    data C = C deriving Show

    foo = (A, "Hello") :*: (B, 7) :*: (C, 3.0) :*: HNil
    ~~~~

    ~~~~
    *Main> foo
    (A,"Hello") :*: ((B,7) :*: ((C,3.0) :*: HNil))
    *Main> :t foo
    foo :: (A, [Char]) :*: ((B, Integer) :*: ((C, Double) :*: HNil))
    ~~~~

# Operations on heterogeneous lists

* Notice our list consisted of pairs

    ~~~~ {.haskell}
    foo :: (A, [Char]) :*: (B, Integer) :*: (C, Double) :*: HNil
    foo = (A, "Hello") :*: (B, 7) :*: (C, 3.0) :*: HNil
    ~~~~

    * View first element as a key or tag, second as a value---How to
      look up value?

    ~~~~ {.haskell}
    class Select k h v | k h -> v where
        (.!) :: h -> k -> v
    instance Select k ((k, v) :*: t) v where
        (.!) ((_, v) :*: _) _ = v
    instance (Select k h v) => Select k (kv' :*: h) v where
        (.!) (kv' :*: h) k = h .! k
    ~~~~

    ~~~~
    *Main> foo .! A
    "Hello"
    ~~~~

* Once again, note the importance of `OverlappingInstances`
    * Needed to break recursion when type of lookup tag matches head of list
* Can use to implement all sorts of other features (concatenation, etc.)

# Object-oriented programming

* Heterogeneous lists can implement object-oriented programming!

    ~~~~ {.haskell}
    returnIO :: a -> IO a
    returnIO = return

    data GetVal = GetVal deriving Show
    data SetVal = SetVal deriving Show
    data ClearVal = ClearVal deriving Show

    mkVal n self = do
      val <- newIORef (n :: Int)
      returnIO $ (GetVal, readIORef val)
               :*: (SetVal, writeIORef val)
               :*: (ClearVal, self .! SetVal $ 0)
               :*: HNil

    test = do               -- prints 7, then 0
      x <- mfix $ mkVal 7
      x .! GetVal >>= print
      x .! ClearVal
      x .! GetVal >>= print
    ~~~~

* But why `mfix`?

# "Tying the recursive knot"

* `mfix` allows you to override methods with inheritance
    * Example, create a "const val" that ignores `SetVal` messages

    ~~~~ {.haskell}
    mkConstVal n self = do
      super <- mkVal n self
      returnIO $ (SetVal, const $ return ())
               :*: super

    test2 = do
      x <- mfix $ mkConstVal 7
      x .! GetVal >>= print
      x .! ClearVal
      x .! GetVal >>= print
    ~~~~

    ~~~~
    *Main> test
    7
    0
    *Main> test2
    7
    7   
    ~~~~

* `mkVal`'s call to `SetVal` was properly overridden by `mkConstVal`

[LanguageOptions]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flag-reference.html#idp46686514436848
[instanceRules]: http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#instance-rules
[erkok]: http://citeseer.ist.psu.edu/viewdoc/download;jsessionid=13851C3A2D4F33918B9D662C20F30762?doi=10.1.1.43.5313&rep=rep1&type=pdf
[mdo]: https://sites.google.com/site/leventerkok/recdo.pdf?attredirects=0
[typeclasses]: http://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-760004.3.1
[instances]: http://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-770004.3.2
[funWithFundeps]: http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=D19C7E3BD1B5C1FC24035542B1494ED9?doi=10.1.1.22.7806&rep=rep1&type=pdf
[`MonadFix`]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Monad-Fix.html#t:MonadFix
[`ExistentialQuantification`]: http://www.haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html#existential-quantification
[`MultiParamTypeClasses`]: http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#id559142
[`Rank2Types`]: http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#universal-quantification
[`RecursiveDo`]: http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#recursive-do-notation
[`FlexibleInstances`]: http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#instance-decls
[`OverlappingInstances`]: http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#instance-overlap
[`FunctionalDependencies`]: http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#functional-dependencies
[`FlexibleContexts`]: http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#flexible-contexts
[`UndecidableInstances`]: http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#undecidable-instances
[`TypeOperators`]: http://www.haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html#infix-tycons
[[HList]]: http://okmij.org/ftp/Haskell/HList-ext.pdf
[[OOHaskell]]: http://arxiv.org/abs/cs/0509027
[`unsafeInterleaveIO`]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/System-IO-Unsafe.html#v:unsafeInterleaveIO
