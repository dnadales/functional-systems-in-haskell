
# MVars revisited

* Exercise: Write transfer function to move money between accounts

    * `wget`
      [`cs240h.stanford.edu/transfer.hs`](http://cs240h.scs.stanford.edu/transfer.hs)
     

    ~~~~ {.haskell}
    import Control.Concurrent
    import Control.Monad

    type Account = MVar Double

    transfer :: Double -> Account -> Account -> IO ()
    transfer amount from to = ???
    ~~~~

    * Should work atomically with multiple threads
    * E.g., other threads should never see money in neither account or
      both accounts
    * Don't transfer money if insufficient funds in account

* Example:

    ~~~~
    *Main> :load "transfer.hs"
    Ok, modules loaded: Main.
    *Main> main
    9.0
    1.0
    ~~~~

# First attempt at solution

~~~~ {.haskell}
type Account = MVar Double

transfer :: Double -> Account -> Account -> IO ()
transfer amount from to =
  modifyMVar_ from $ \bf -> do
    when (bf < amount) $ fail "not enough money"
    modifyMVar_ to $ \bt -> return $! bt + amount
    return $! bf - amount
~~~~

* What's wrong with the above code?

# First attempt at solution

~~~~ {.haskell}
type Account = MVar Double

transfer :: Double -> Account -> Account -> IO ()
transfer amount from to =
  modifyMVar_ from $ \bf -> do
    when (bf < amount) $ fail "not enough money"
    modifyMVar_ to $ \bt -> return $! bt + amount
    return $! bf - amount
~~~~

* What's wrong with the above code?
    1. Can deadlock when simultaneously transferring money in both
       directions

        ~~~~ {.haskell}
        forkIO $ transfer 1 ac1 ac2
        forkIO $ transfer 1 ac2 ac1
        ~~~~

    2. Throwing an exception when not enough money is ugly... what if
       we just waited for enough money to show up before completing
       the transfer?

* How would you fix #1?

# Second attempt at solution

* Strategy:  Use non-blocking
  [`tryTakeMVar`](http://www.haskell.org/ghc/docs/latest/html/libraries/base-4.7.0.0/Control-Concurrent-MVar.html#v:tryTakeMVar)
  for second `MVar`
    * If it fails, release both and try again in different order

~~~~ {.haskell}
safetransfer :: Double -> Account -> Account -> IO ()
safetransfer amount from to = do
  let tryTransfer = modifyMVar from $ \ bf -> do
        when (bf < amount) $ fail "not enough money"
        mbt <- tryTakeMVar to
        case mbt of
          Just bt -> do putMVar to $! bt + amount
                        return (bf - amount, True)
          Nothing -> return (bf, False)
  ok <- tryTransfer
  unless ok $ safetransfer (- amount) to from
~~~~

* Is this gross enough for you yet?
    * If not, make the code sleep when not enough funds are present in
      `from`
    * ... or fix it to handle asynchronous exceptions properly


# [Software transactional memory][STM]

* What if instead we used database-like transactions?
    * Read and write a bunch of variables
    * Writes initially go to log, then get committed atomically at end
    * Did you get an inconsistent view or clash with another update?
        No problem, just abort and retry the whole transaction

* Would be hard to do in C or Java
    * What if you wrote to the network or file system during transaction?
    * "Externalized" actions can't easily be rolled back 

* But in Haskell, the `IO` type (or lack thereof) can control side
  effects

* Slides inspired by good write-up in [[Peyton Jones]][beautiful-pdf]

# [STM] basics

* New variable type `TVar a`
    * Similar interface to an [`IORef a`][IORef] (like an MVar w/o lock)
    * Module [`Control.Concurrent.TVar`][TVar] gives you

    ~~~~ {.haskell}
    newTVarIO   :: a -> IO (TVar a)
    readTVarIO  :: TVar a -> IO a

    readTVar    :: TVar a -> STM a
    writeTVar   :: TVar a -> a -> STM ()
    modifyTVar  :: TVar a -> (a -> a) -> STM ()  -- lazy
    modifyTVar' :: TVar a -> (a -> a) -> STM ()  -- strict
    ~~~~

* New [`STM` monad][STM-monad] allows `TVar` access but no
  irreversible side effects

    ~~~~ {.haskell}
    atomically :: STM a -> IO a
    ~~~~

    * `atomically` lets you run `STM` computations from `IO`
    * You get: semantics of one global lock + parallelism of
      fine-grained locks!
    * In exchange, you give up the ability to perform externalized
      `IO` actions

# STM Example

~~~~ {.haskell}
type Account = TVar Double

transfer :: Double -> Account -> Account -> STM ()
transfer amount from to = do
  modifyTVar' from (subtract amount)
  modifyTVar' to (+ amount)

main :: IO ()
main = do
  ac1 <- newTVarIO 10
  ac2 <- newTVarIO 0
  atomically $ transfer 1 ac1 ac2
~~~~

* Note: `subtract a b = b - a`
    * Language wart:  Unlike all other binary operators, can't make
      section with `(- a)` because that's unary negation (i.e.,
      `0-a`)

* What if you want to wait when not enough money in account?

# Aborting

~~~~ {.haskell}
retry :: STM a
orElse :: STM a -> STM a -> STM a
~~~~

* `retry` aborts the transaction
    * But `STM` knows what `TVar`s code read to detect conflicts...
    * Can sleep until some `TVar` code read changes w/o explicit
      condition variables

    ~~~~ {.haskell}
    transfer :: Double -> Account -> Account -> STM ()
    transfer amount from to = do
      bf <- readTVar from
      when (amount > bf) retry
      modifyTVar' from (subtract amount)
      modifyTVar' to (+ amount)
    ~~~~

* `orElse` tries second action if first one aborts (sleeps if both
    abort)

    ~~~~ {.haskell}
    transfer2 :: Double -> Account -> Account -> Account -> STM ()
    transfer2 amount from1 from2 to =
      atomically $ transferSTM amount from1 to
                   `orElse` transferSTM amount from2 to
    ~~~~

    * Effectively provides nested transactions

# Enforcing invariants

~~~~ {.haskell}
alwaysSucceeds :: STM a -> STM ()
~~~~

* `alwaysSucceeds` registers invariant to check after every transaction<br>
  (Either the invariant throws an exception or its return value ignored)

* Example: say you are paranoid about negative account balances

~~~~ {.haskell}
newAccount :: Double -> STM Account
newAccount balance = do
  tv <- newTVar balance
  alwaysSucceeds $ do balance <- readTVar tv
                      when (balance < 0) $ fail "negative balance"
  return tv

bogus :: IO ()
bogus = do
  ac <- atomically $ newAccount 10
  atomically $ modifyTVar ac (subtract 15)
~~~~

* Will catch errors immediately at end of & roll back faulty
  transactions

~~~~
*Main> bogus
*** Exception: negative balance
~~~~

# Switching gears...

* Let's get back to pure functional code

    * ...but think about how to implement it

* How does the compiler represent data in memory?

    * For more details after lecture, see [GHC commentary][heap-layout]

# Na&#xef;ve Haskell data representation

* A value requires a constructor, plus arguments

    * At runtime, need to determine a value's constructor, but not
      it's type<br/> (Compiler already type-checked program, so no
      runtime type checks)

    ~~~~ {.c}
    struct Val {
      unsigned long constrno; /* constructor # */
      struct Val *args[];     /* flexible array */
    };
    ~~~~

    * For a type like `[Int]`, `constrno` might be 0 for `[]` and 1
      for `(:)`, where `[]` has 0-sized `args` and `(:)` has 2-element
      `args`
    * For a type like `Int`, `constrno` can be the actual integer,
      with no `args`
    * For a single-constructor type (e.g., `Point`) `constrno` not
      used

* Problems with our approach so far
    * No way to represent exceptions or thunks
    * Garbage collector needs to know how many elements are in `args`
    * Small values such as `Int`s are `Val *`s, requiring chasing a pointer

# Add level of indirection to describe values

~~~~ {.c}
typedef struct Val {
  const struct ValInfo *info;
  struct Val *args[];
} Val;

/* Statically allocated at compile time.  Only one per
 * constructor (or closure-creating expression, etc.) */
struct ValInfo {
  struct GCInfo gcInfo;  /* for garbage collector */
  enum { CONSTRNO, FUNC, THUNK, IND } tag;
  union {
    unsigned int constrno;
    Val *(*func) (const Val *closure, const Val *arg);
    Exception *(*thunk) (Val *closure);
  };
};
~~~~

* `gcInfo` says how many `Val *`s are in `args` and where they are
* `tag == CONSTRNO` means `constrno` valid, used as on last slide
* `tag == IND` means `args[0]` is an indirect *forwarding pointer*
  to another `Val` and union is unused; useful if size of `args`
  grows

# Function values

* A `Val` whose `ValInfo` has `tag == FUNC` uses the `func` field

    ~~~~ {.c}
        Val *(*func) (const Val *closure, const Val *arg);
    ~~~~

* To apply function `f` to argument `a` (where both are type `Val *`):

    ~~~~ {.c}
            f->info->func (f, a);
    ~~~~

* Note that `func`'s first argument (`closure`) is the function `Val` itself
    * Provides a _closure_ environment so `ValInfo`/`func` can be re-used
* `func`'s second argument (`arg`) is the argument `a` on which the
  function is being evaluated
* Assume all functions take one argument
    * Logically this is fine since we have currying
    * For performance, real compilers must optimize multi-argument case

# Closures

* Top-level bindings don't need the `closure` argument to `func`

    ~~~~ {.haskell}
    addOne :: Int -> Int
    addOne x = x + 1
    ~~~~

    * The `Val` for function `addOne` can have zero-length `args`

* Local bindings may need environment values in `closure`

    ~~~~ {.haskell}
    add :: Int -> (Int -> Int)
    add n = \m -> addn m
        where addn m = n + m
    ~~~~

    * Compiler will only emit code for local function `addn` once
    * But logically, there is a separate `addn` function (with a
      different `n`) for each invocation of `add`
    * So each `addn` instance is a different `Val`, but all share the
      same `ValInfo`
    * Use `args[0]` in each `Val` to specify the value of `n`

# Thunk values

* A `Val` with `tag == THUNK` uses the `thunk` field in `ValInfo`

    ~~~~ {.c}
        Exception *(*thunk) (Val *closure);
    ~~~~

    * *Updates* `v` (turns it into non-thunk) or returns a non-`NULL`
      `Exception *`

* To evaluate a thunk:

    ~~~~ {.c}
            v->info->thunk (v);
    ~~~~

* Two big differences between thunks and functions
    * A function takes an argument, while a thunk does not
    * A function value is immutable, while a thunk updates itself

* Note also that a thunk may throw an exception
    * Functions can, too, but for simplicity let's implement it by
      having the function return a thunk that throws an exception

# Forcing

* Turning a thunk into a non-thunk is known as *forcing* it
* What if a thunk's return value doesn't fit in thunk's `args`?
    * This is why we have the `IND` `ValInfo` tag---Allocate new
      `Val`, place indirect forwarding pointer in old `Val`
* A possible implementation of forcing that walks `IND` pointers:

    ~~~~ {.c}
    Exception *force (Val **vp)
    {
      for (;;) {
        if ((*vp)->info->tag == IND)
          *vp = (*vp)->arg[0];
        else if ((*vp)->info->tag == THUNK) {
          Exception *e = (*vp)->info->thunk (*vp);
          if (e)
            return e;
        }
        else
          return NULL;
      }
    }
    ~~~~


# Currying

* Let's use simple implementation of currying (GHC very complex)
* Set `closure->args` to head of list of previously curried args

    ~~~~ {.haskell}
    const3 :: a -> b -> c -> a
    const3 a b c = a
    ~~~~

    * Compiler emits 3 `ValInfo`s and 3 functions for `const3`
    * Top-level binding's `ValInfo` has `func = const3_1`
    * `const3_1` creates `Val v1` where `arg[0]` is first argument
      (`a`) and
      <span style="white-space: nowrap;">`info->func = const3_2`</span>
    * `const3_2` creates a `Val v2` where `arg[0]` is the second
      argument (`b`), `arg[1]` is `v1`, and `info->func` is `const3_3`
    * `const3_3` has access to all arguments and actually implements
      `const3`

* Shared arguments have common arg tails, only evaluated once

    ~~~~ {.haskell}
        let f = const3 (superExpensive 5) -- v1, evaluated once
        in (f 1 2, f 3 4)
    ~~~~

# Code for currying example

~~~~ {.haskell}
const3 :: a -> b -> c -> a
const3 a b c = a
~~~~

~~~~ {.c}
Val *const3_1 (Val *ignored, Val *a)
{
  v = (Val *) gc_malloc (offsetof (Val, args[1]));
  v->info = &const3_2_info;  /* func = const3_2 */
  v->args[0] = a;
  return v;
}

Val *const3_2 (Val *closure, Val *b)
{
  v = (Val *) gc_malloc (offsetof (Val, args[2]));
  v->info = &const3_3_info;  /* func = const3_3 */
  v->args[0] = b;
  v->args[1] = closure;
  return v;
}

Val *const3_3 (Val *v, Val *c)
{
  return v->args[1]->args[0];
}
~~~~

# Unboxed types

* Unfortunately, now `Int` has even more overhead
    * To use, must check `i->info->tag` then access `i->info->constr`
    * Moreover, each number needs a distinct `ValInfo` structure (but
      `ValInfo`s statically allocated---how do you know what numbers
      the program will need)

* Idea: Have special *unboxed* types that don't use `struct Val`

    ~~~~ {.c}
    union Arg {
      struct Val *boxed;     /* most values are boxed */
      unsigned long unboxed; /* "primitive" values */
    };

    typedef struct Val {
      const struct ValInfo *info;
      union Arg args[];      /* args can be boxed or unboxed */
    } Val;
    ~~~~

    * Unboxed types have no constructor and cannot be thunks (no
      `ValInfo`)
    * Can fit in a single register or take the place of a `Val *` arg
    * Must extend `GCInfo` to identify which args are and are not boxed


# Unboxed types in GHC

* GHC exposes unboxed types (even though not part of Haskell)
    * Symbols use `#` character--must enable with
      [`-XMagicHash`][MagicHash] option
    * Have unboxed types (`Int#`) and primitive operations on them
      (`+#`)
    * See [GHC.Prim][GHC.Prim] or type "`:browse GHC.Prim`" in GHCI
    * Also have [unboxed constants][MagicHash]--`2#`, `'a'#`, `2##`
      (unsigned), `2.0##`

* What is `Int` really?
    * Single-constructor data type, with a single, unboxed argument

    ~~~~
    Prelude> :set -XMagicHash
    Prelude> :m +GHC.Types GHC.Prim
    Prelude GHC.Types GHC.Prim> :i Int
    data Int = I# Int#      -- Defined in GHC.Types
    ...
    Prelude GHC.Types GHC.Prim> case 1 of I# u -> I# (u +# 2#)
    3
    ~~~~

    * Lets `Int` contain thunk, but avoids pointer dereference once
      evaluated
    * [GHC.Prim] is a fake module (for documentation only), but
      [GHC.Types] is real!

# Restrictions on unboxed types

* Cannot instantiate type variables with unboxed types
    * E.g., `Val *` always fits in general-purpose register, unboxed
      types might need FP reg

    ~~~~ {.haskell}
    {-# LANGUAGE MagicHash #-}
    import GHC.Prim

    data FastPoint = FastPoint Double# Double#  -- ok
    fp = FastPoint 2.0## 2.0##                  -- ok

    -- Error: can't pass unboxed type to polymorphic function
    fp' = FastPoint 2.0## (id 2.0##)

    -- Error: can't use unboxed type as type parameter
    noInt :: Maybe Int#
    noInt = Nothing
    ~~~~

* Enforced by making unboxed types a different kind of type

    ~~~~
    Prelude GHC.Types GHC.Prim> :kind Int#
    Int# :: #
    ~~~~

    * Recall type variables have kinds with stars (&#x2217;, &#x2217;
      &#x2192; &#x2217;, etc.), never `#`

    * Polymorphism works because all types of kind &#x2217;
      represented by our `Val *`

# `seq` revisited

* Recall `seq :: a -> b -> b`
    * If `seq a b` is forced, then first `a` is forced, then `b` is
      forced and returned
* Consider the following code:

    ~~~~ {.haskell}
    infiniteLoop = infiniteLoop :: Char   -- loops forever

    seqTest1 = infiniteLoop `seq` "Hello" -- loops forever

    seqTest2 = str `seq` length str       -- returns 6
        where str = infiniteLoop:"Hello"
    ~~~~

    * `seqTest1` hangs forever, while `seqTest2` happily returns 6
* `seq` only forces a `Val`, not the `arg` fields of the `Val`
    * `seqTest2`'s `seq` forces `str`'s constructor `(:)`, but not the
      head or tail
    * This is known as putting `str` in *Weak Head Normal Form* (WHNF)
    * Can't fully evaluate an arbitrary data type (but see
      [Control.DeepSeq](http://hackage.haskell.org/packages/archive/deepseq/latest/doc/html/Control-DeepSeq.html))


# Example: hypothetical `seq` implementation

~~~~ {.c}
const struct ValInfo seq_info = {
  some_gcinfo, THUNK, .thunk = &seq_thunk
};

Val *seq_2 (Val *closure, Val *b)
{ /* assume seq_1 put first arg of (seq a b) in closure */
  c = (Val *) gc_malloc (offsetof (Val, args[2]));
  c->info = &seq_info;
  c->args[0].boxed = closure->args[0];
  c->args[1].boxed = b;
  return c;
}

Exception *seq_thunk (Void *c)
{
  Exception *e = force (&c->args[0].boxed);
  if (!e) {
    c->info = &ind_info;     /* ValInfo with tag = IND */
    c->args[0].boxed = c->args[1]; /* forward to b */
  }
  return e;
}
~~~~

# Strictness revisited

* Recall strictness flag on fields in data declarations

    ~~~~ {.haskell}
    data IntWrapper = IntWrapper !Int
    ~~~~

    * `Int` has `!` before it, meaning it must be strict
    * Strict means the `Int`'s `ValInfo` cannot have `tag` `THUNK` or `IND`
* Accessing a strict `Int` touches only one cache line
    * Recall `data Int = I# Int#` has only one constructor
    * Plus strict flag means `tag == CONSTRNO`, so know what's in
      `ValInfo`
    * Plus `Int#` is unboxed
    * Thus, once `IntWrapper` forced, immediately safe to access `Int`
      as

        ~~~~ {.c}
            myIntWrapper.arg[0].boxed->arg[0].unboxed
        ~~~~

# Semantic effects of strictness

* Strictness is primarily used for optimization
    * To avoid building up long chains of thunks
    * To save overhead of checking whether thunk evaluated
* But has semantic effects:  A non-strict `Int` is not just a number
    * Can also throw an exception or loop forever when evaluated
    * Such behavior can be modeled as a special value $\bot$
      ("bottom")
    * So the values of `Int` are $\{0,1\}^{64} \cup \{\bot\}$
    * Types that include value $\bot$ are called *lifted*
* Note 1: an unboxed type is necessarily unlifted
* Note 2: `!Int` not a first-class type, only valid for `data` fields

    ~~~~ {.haskell}
    data SMaybe a = SJust !a | SNothing   -- ok, data field
    strictAdd :: !Int -> !Int -> !Int     -- error
    type StrictMaybeInt = Maybe !Int      -- error
    ~~~~

# `case` statements revisited

* `case` statement pattern matching can force thunks
    * An *irrefutable* pattern is one that always matches
    * A pattern consisting of a single variable or `_` is irrefutable
    * Any non-irrefutable pattern forces evaluation of the argument
    * Matching happens top-to-bottom, and left-to-right within
      alternatives
* Function pattern matching is the same as (desuggared into) `case`
    * Recall `undefined :: a` is `Prelude` symbol with value $\bot$

    ~~~~ {.haskell}
    f ('a':'b':rest) = rest
    f _              = "ok"
    test1 = f (undefined:[])   -- error
    test2 = f ('a':undefined)  -- error
    test3 = f ('x':undefined)  -- "ok" (didn't force tail)
    ~~~~

* Adding `~` before a pattern makes it irrefutable

    ~~~~ {.haskell}
    three = (\ ~(h:t) -> 3) undefined  -- evaluates to 3
    ~~~~

# `newtype` declarations

* We've seen three ways to introduce new types
    * `data` -- creates a new (boxed) type, adding overhead of a `Val`
      wrapper
    * `type` -- creates an alias for an existing type, with no overhead
    * `newtype` -- so we have have been vague about it
* Sometimes you want a new type implemented by an existing type
    * E.g., might want `Meters`, `Seconds`, `Grams`, all implemented
      by `Double`
    * Using `type` would make them all synonymous, facilitating errors
    * Might want different instances of `Show` for each, impossible
      with `type`
    * Could say `data Meters = Meters Double` -- but will add overhead
* The `newtype` keyword introduces new type _with no overhead_
    * Use just like `data`, but limited to one constructor and one
      field
    * This is possible because all type-checking is compile-time

# `newtype` semantics

* What's the semantic difference between these two declarations?

    ~~~~ {.haskell}
    newtype NTInt = NTInt Int deriving (Show) -- def 1
    ~~~~

    ~~~~ {.haskell}
    data NTInt = NTInt !Int deriving (Show)   -- def 2
    ~~~~

* Exercise:  Suppose you have

    ~~~~ {.haskell}
    uNTInt = NTInt undefined
    ~~~~

    Write code that behaves differently for two definitions of `NTInt`

# `newtype` semantics

* What's the semantic difference between these two declarations?

    ~~~~ {.haskell}
    newtype NTInt = NTInt Int deriving (Show) -- def 1
    ~~~~

    ~~~~ {.haskell}
    data NTInt = NTInt !Int deriving (Show)   -- def 2
    ~~~~

* Solution:

    ~~~~ {.haskell}
    uNTInt = NTInt undefined
    testNT = case uNTInt of NTInt _ -> True
    ~~~~

* The `NTInt` constructor is a "fake" compile-time-only construct

    * A case statement deconstructing a `newtype` compiles to nothing
    * `testNT` evaluates to `True` under definition 1 (`newtype`)

* Conversely, forcing a value (by matching constructor) forces strict fields

    * Throws undefined error under definition 2 (`data`)

# The [`UNPACK`][UNPACK] pragma

* `newtype` almost always better than `data` when it applies
* What about a multi-field data type?

    ~~~~ {.haskell}
    data TwoInts = TwoInts !Int !Int
    ~~~~

    * Fields are strict, we know they'll have `CONSTRNO` `ValInfo`
    * Why not stick the `Int#`s directly into the `args` of a
      `TwoInts` `Val`?
    * GHC provides an `UNPACK` pragma to do just this

        ~~~~ {.haskell}
        data TwoInts = TwoInts {-# UNPACK #-} !Int {-# UNPACK #-} !Int
        ~~~~

    * Works for any strict field with a single-constructor datatype
      (so omitted `ValInfo` unambiguous)

* Unlike `newtype`, `UNPACK` is not always a win
    * If you pass field as argument, will need to re-box it
* `-funbox-strict-fields` flag unpacks *all* strict fields



# User-managed memory

* Opaque type [`Ptr a`][Ptr] represents pointers to type `a`
    * Pointers are not typesafe--allow pointer arithmetic and casting

        ~~~~ {.haskell}
        nullPtr :: Ptr a
        plusPtr :: Ptr a -> Int -> Ptr b
        minusPtr :: Ptr a -> Ptr b -> Int
        castPtr :: Ptr a -> Ptr b
        ~~~~

    * Pointer arithmetic is always in units of bytes (unlike in C,
      where unit is size of the pointed-to object)
* Class [`Storable`][Storable] provides raw access to memory using
  `Ptr`s

    ~~~~ {.haskell}
    class Storable a where
        sizeOf :: a -> Int
        alignment :: a -> Int
        peek :: Ptr a -> IO a
        poke :: Ptr a -> a -> IO ()
        ...
    ~~~~

    * Most basic types (`Bool`, `Int`, `Char`, `Ptr a`, etc.) are `Storable`

# `alloca`

* Easiest way to get a valid `Ptr` is [`alloca`][alloca]:

    ~~~~ {.haskell}
    alloca :: Storable a => (Ptr a -> IO b) -> IO b
    ~~~~

    * Allocates enough space for an object of type `a`
    * Calls function with a `Ptr` to the space
    * Reclaims the memory when the function returns (much like C
      `alloca`)
    * Can also ask for a specific number of bytes:

    ~~~~ {.haskell}
    allocaBytes :: Int -> (Ptr a -> IO b) -> IO b
    ~~~~

* `Foreign` module provides handy [`with`][with] utility

    ~~~~ {.haskell}
    with :: Storable a => a -> (Ptr a -> IO b) -> IO b
    with val f  =
      alloca $ \ptr -> do
        poke ptr val
        res <- f ptr
        return res
    ~~~~


# More `Storable` types

* `Foreign.C` contains wrappers for C types
    * `CInt`, `CUInt`, `CChar`, `CDouble`, `CIntPtr` etc.
* `Data.Int` and `Data.Word` have all sizes of machine integer
    * `Int8`, `Int16`, `Int32`, `Int64` -- signed integers
    * `Word8`, `Word16`, `Word32`, `Word64` -- unsigned integers

* Example: extract all the bytes from a `Storable` object

    ~~~~ {.haskell}
    toBytes :: (Storable a) => a -> [Word8]
    toBytes a = unsafePerformIO $
        with a $ \pa -> go (castPtr pa) (pa `plusPtr` sizeOf a)
        where go p e | p < e = do b <- peek p
                                  bs <- go (p `plusPtr` 1) e
                                  return (b:bs)
                     | otherwise = return []
    ~~~~

    * `unsafePerformIO` might be okay here since `toBytes` pure
    * Notice how `plusPtr` lets us change from `Ptr a` to `Ptr Word8`

# `malloc` and `mallocForeignPtr`

* Can also allocate longer-lived memory with `malloc`

    ~~~~ {.haskell}
    malloc :: Storable a => IO (Ptr a)
    mallocBytes :: Int -> IO (Ptr a)
    free :: Ptr a -> IO ()
    realloc :: Storable b => Ptr a -> IO (Ptr b)
    reallocBytes :: Ptr a -> Int -> IO (Ptr a)
    ~~~~

    * Disadvantage:  bad programming can lead to memory
      leaks/corruption

* `ForeignPtr` lets you delegate deallocation to garbage collector

    ~~~~ {.haskell}
    mallocForeignPtr :: Storable a => IO (ForeignPtr a)
    mallocForeignPtrBytes :: Int -> IO (ForeignPtr a)
    ~~~~

# Working with `ForeignPtr`s

* To use `ForeignPtr`, must convert it to `Ptr`
    * Problem: How does GC know `ForeignPtr` in scope when using
      `Ptr`?
    * Solution: use `Ptr` within function that keeps reference to
      `ForeignPtr`

    ~~~~ {.haskell}
    withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
    ~~~~

* Can also convert `Ptr`s to `ForeignPtr`s

    ~~~~ {.haskell}
    type FinalizerPtr a = FunPtr (Ptr a -> IO ())
    newForeignPtr :: FinalizerPtr a -> Ptr a
                  -> IO (ForeignPtr a)
    newForeignPtr_ :: Ptr a -> IO (ForeignPtr a)
    addForeignPtrFinalizer :: FinalizerPtr a -> ForeignPtr a
                           -> IO ()
    ~~~~

    * Can add multiple finalizers, will run in reverse order
* Note use of `FunPtr` -- this is type wrapper for C function pointer
    * Need foreign function interface to create these
    * [`finalizerFree`][finalizerFree] symbol conveniently provides
      function pointer for `free`

# [Foreign function interface][FFI] (FFI)

* Can import foreign functions like this:

    ~~~~ {.haskell}
    foreign import ccall unsafe "stdlib.h malloc"
        c_malloc :: CSize -> IO (Ptr a)
    foreign import ccall unsafe "stdlib.h free"
        c_free :: Ptr a -> IO ()
    ~~~~

    * `ccall` says use C calling convention (also `cplusplus` and few
      others)
    * `unsafe` promises the C function will not call back into
      Haskell
    * `unafe` faster than `safe`, but gives undefined results if call
      triggers GC
* Spec for import string: `"`[`static`] [*c-header*] [`&`][*c-name*]`"`
    * `static` required only if *c-name* is `dynamic` or `wrapper`
    * *c-header* is a single `.h` file with the declaration
       (ignored by GHC)
    * '&' imports pointer rather than function (required for `FunPtr`s)

    ~~~~ {.haskell}
    foreign import ccall unsafe "foo.h foo"
        foo :: Int -- foo must be function: int foo(void);
    foreign import ccall unsafe "foo.h &bar"
        bar :: Ptr Int -- here bar can be int: int bar;
    ~~~~


# FFI types

* FFI function arguments must be *basic foreign types*
    * `Char`, `Int`, `Double`, `Float`, `Bool`, `Int8`, `Int16`,
      `Int32`, `Int64`, `Word8`, `Word16`, `Word32`, `Word64`, `Ptr`
      `a`, `FunPtr a`, and `StablePtr a`
    * Also accepts any `type` or `newtype` wrappers for basic types
      (`CInt`, `CChar`, etc.)
* FFI function results can be
    * Any valid argument type
    * `()` (for functions returning `void`)
    * `IO a` where `a` is either of the above two
* Use `IO` if function has side effects or non-determinism
    * Okay to omit if it is a pure C function:

        ~~~~ {.haskell}
        foreign import ccall unsafe "arpa/inet.h ntohl"
            ntohl :: Word32 -> Word32
        ~~~~

    * Haskell can't check C purity, so omitting `IO` can cause
      problems

# [`hsc2hs`][hsc2hs]

* How to access C data structures?

    ~~~~ {.c}
    struct mystruct {
      char *name;
      int value;
    };
    ~~~~

    * Might model with opaque placeholder type

    ~~~~ {.haskell}
    data MyStruct        -- no constructors, just a placeholder
    getValue :: Ptr MyStruct -> IO CInt
    getValue ptr = peek $ ptr `plusPtr` 8  -- assumes char * 8 bytes
    ~~~~

* [`hsc2hs`][hsc2hs] is pre-processor that lets you compute C values

    ~~~~ {.haskell}
    #include "myheader.h"
    getValue ptr = peek $ ptr `plusPtr`
                   #{offset struct mystruct, value}
    ~~~~

    * Super-simple implementation just uses C macros & `printf`
    * Find the file [`template-hsc.h`][template-hsc.h] on your system
      to see defs of `#` commands
    * Can also define your own macros with `#let` (like `#define` w/o
      parens)


# [`ByteString`s][bytestring]

* Haskell `String`s obviously not very efficient

* In Lab2, you saw faster `ByteStrings`

    ~~~~ {.haskell}
    import qualified Data.ByteString as S
    import qualified Data.ByteString.Char8 as S8
    ~~~~

    * Interface looks like a list:  `S.head`, `S.tail`,
    `S.length`, `S.foldl`, `S.cons` (like `:`), `S.empty` (like `[]`),
    `S.hPut` (like `hPutStr`), `S.readFile`
    * Must import qualified to avoid name clashes
    * `S.pack` and `S.unpack` translate to/from `[Word8]`
    * `S8` has same functions as `S`, but uses `Char` instead of
    `Word8`---means you lose upper bits of `Char`
        * Use
          [`toString`](http://hackage.haskell.org/packages/archive/utf8-string/1.0.1.1/doc/html/Data-ByteString-UTF8.html#v:toString)
          from
          [utf8-string](http://hackage.haskell.org/package/utf8-string)
          to avoid loss, or use Bryan's
          [text](http://hackage.haskell.org/package/text) instead of
          `ByteString` for any serious text needs

* Now let's look at implementation

    ~~~~ {.haskell}
    data ByteString = PS {-# UNPACK #-} !(ForeignPtr Word8)
                         {-# UNPACK #-} !Int  -- offset
                         {-# UNPACK #-} !Int  -- length
    ~~~~

# [Lazy `ByteString`s][ByteString.Lazy]

* Same package implements [*lazy* `ByteString`s][ByteString.Lazy]

    ~~~~ {.haskell}
    import qualified Data.ByteString.Lazy as L
    import qualified Data.ByteString.Lazy.Char8 as L8
    ~~~~

    * Provides mostly the same functions as strict `ByteString`
      modules

* Confusing that both modules use same names for many things
    * Important to look at import qualifications to understand code
    * Worse: documentation does not qualify symbol names<br/>
      Tip: **hover your mouse over symbol and look at URL to figure
      out module**
    * Also, `S.ByteString` and `S8.ByteString` are the same type
      (re-exported), and similarly for `L.ByteString` and
      `L8.ByteString`
    * `S.ByteString` and `L.ByteString` *not* same type, but can
      convert:

    ~~~~ {.haskell}
    fromChunks :: [S.ByteString] -> L.ByteString
    toChunks :: L.ByteString -> [S.ByteString]
    ~~~~

# Lazy `ByteString` implementation

* Lazy `ByteString`s are implemented in terms of strict ones

    ~~~~ {.haskell}
    data ByteString = Empty
                    | Chunk {-# UNPACK #-} !S.ByteString ByteString
    ~~~~

    * Invariant: `Chunk`'s first argument (`S.ByteString`) never `null`
    * Basically a linked list of strict `ByteString`s
    * Head is strict, tail is not, allowing lazy computation or I/O

* When to use strict/lazy `ByteString`s?
    * Obviously use lazy when you need laziness (e.g., lazy I/O,
      infinite or cyclical strings, etc.)
    * Lazy also much faster at concatenation (need to build a new list
      of `S.ByteString`s, but not copy the data they contain)
    * Strict makes it much easier to implement things like string
      search
    * Converting strict to lazy `ByteString`s is cheap, reverse is not
      (so if a library can work efficiently on lazy `ByteString`s,
      good to expose that functionality)


[beautiful]: http://research.microsoft.com/en-us/um/people/simonpj/papers/stm/index.htm#beautiful
[beautiful-pdf]: http://research.microsoft.com/en-us/um/people/simonpj/papers/stm/beautiful.pdf
[STM]: http://hackage.haskell.org/package/stm
[Ptr]: http://hackage.haskell.org/package/base/docs/Foreign-Ptr.html#t:Ptr
[Storable]: http://hackage.haskell.org/package/base/docs/Foreign-Storable.html#t:Storable
[GHC.Prim]: https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-prim-0.4.0.0/GHC-Prim.html
[MagicHash]: http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#magic-hash
[UNPACK]: http://www.haskell.org/ghc/docs/latest/html/users_guide/pragmas.html#unpack-pragma
[alloca]: http://hackage.haskell.org/package/base/docs/Foreign-Marshal-Alloc.html#v:alloca
[with]: http://hackage.haskell.org/package/base/docs/Foreign-Marshal-Utils.html#v:with
[FFI]: http://www.haskell.org/onlinereport/haskell2010/haskellch8.html
[hsc2hs]: http://www.haskell.org/ghc/docs/latest/html/users_guide/hsc2hs.html
[template-hsc.h]: https://raw.githubusercontent.com/ghc/hsc2hs/master/template-hsc.h
[bytestring]: http://hackage.haskell.org/package/bytestring
[ByteString.Lazy]: http://www.haskell.org/ghc/docs/latest/html/libraries/bytestring-0.10.6.0/Data-ByteString-Lazy.html
[finalizerFree]: http://hackage.haskell.org/package/base-4.8.2.0/docs/Foreign-Marshal-Alloc.html#v:finalizerFree
[IORef]: http://hackage.haskell.org/package/base/docs/Data-IORef.html
[TVar]: https://hackage.haskell.org/package/stm-2.4.4/docs/Control-Concurrent-STM-TVar.html
[STM-monad]: http://hackage.haskell.org/package/stm-2.4.4/docs/Control-Monad-STM.html
[heap-layout]: https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects
[GHC.Types]: https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-prim-0.4.0.0/src/GHC-Types.html
[CTYPE]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ffi.html#ffi-capi
