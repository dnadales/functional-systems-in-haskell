% Phantoms

Let's think about a programming pattern we've seen, but not paid
attention to.

# Patterns: I

~~~~ {.haskell}
0
~~~~

~~~~ {.haskell}
0 + n  ==  n
n + 0  ==  n
~~~~

~~~~ {.haskell}
(a + b) + c  ==  a + (b + c)
~~~~


# Patterns: II

~~~~ {.haskell}
1
~~~~

~~~~ {.haskell}
1 * n  ==  n
n * 1  ==  n
~~~~

~~~~ {.haskell}
(a * b) * c  ==  a * (b * c)
~~~~


# Patterns: III

~~~~ {.haskell}
[]
~~~~

~~~~ {.haskell}
[] ++ n  ==  n
n ++ []  ==  n
~~~~

~~~~ {.haskell}
(a ++ b) ++ c  ==  a ++ (b ++ c)
~~~~


# Patterns: IV

~~~~ {.haskell}
True
~~~~

~~~~ {.haskell}
True && n  ==  n
n && True  ==  n
~~~~

~~~~ {.haskell}
(a && b) && c == a && (b && c)
~~~~


# Patterns, abstracted

Typeclass:

~~~~ {.haskell}
class Monoid a where
	-- A "zero element"
    mempty  :: a
	-- An associative operation
    mappend :: a -> a -> a
~~~~

Where can you find this typeclass?

~~~~ {.haskell}
import Data.Monoid
~~~~


# Monoids

Instances of `Monoid` must obey some rules.

Rule 1: identity element

~~~~ {.haskell}
mempty `mappend` n  ==  n
n `mappend` mempty  ==  n
~~~~

Rule 2: our associative operation *must actually associate*.

~~~~ {.haskell}
(a `mappend` b) `mappend` c  ==
a `mappend` (b `mappend` c)
~~~~


# Rules?

Monoids come from abstract algebra.

In abstract algebra, rules that must be true are called *axioms*.

Also called *laws*.

In Haskell, how are these rules/axioms/laws enforced?

* They are not.


# Monoids for lists

Here's the easiest and most familiar-to-Haskellers case:

~~~~ {.haskell}
instance Monoid [a] where
     mempty           = []
     xs `mappend` ys  = xs ++ ys
~~~~

Pop quiz:

* What other definition(s) would follow the `Monoid` laws?

* Do they make any sense?


# Monoids for numbers?

Numbers are an interesting case.

Addition as monoid:

* Identity `0`

* Associative operator `+`

Multiplication as monoid:

* Identity `1`

* Associative operator `*`


# When do we use typeclasses?

Suppose you want to abstract a code pattern into a typeclass.

Under what circumstances is this likely to work best?

* When there is *just one* "canonical" behaviour you expect for a
  given type.

For lists, our `Monoid` instance *is* canonical:

* Any other behaviour that follows the laws is just *weird*.

For numbers, we have two sensible behaviours:

* No one `Monoid` instance can be called canonical!


# Monoids for multiplication

~~~~ {.haskell}
newtype Product a = Product { getProduct :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where
    mempty                        = Product 1

    Product x `mappend` Product y = Product (x * y)
~~~~


# Monoids for addition

~~~~ {.haskell}
newtype Sum a = Sum { getSum :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Sum a) where
    mempty                = Sum 0

    Sum x `mappend` Sum y = Sum (x + y)
~~~~


# The `Either` type

There exists a built-in type named `Either`.

~~~~ {.haskell}
data Either a b = Left a | Right b
~~~~

By convention:

* `Left` means "something went wrong"

* `Right` means "result was a success"

Often used as follows:

~~~~ {.haskell}
type Result a = Either String a
~~~~

(where the `String` carries an error message)


# Coding exercise

Create a `Monoid` instance that will give the *first success* from a
chain of `Either` values.

Desired behaviour:

~~~~ {.haskell}
Left "you goofed" `mappend`
Left "i win!"     `mappend`
Right "rats! you won!"

   ==

Right "rats! you won!"
~~~~

You have five minutes.


# Ambient machinery for the coding exercise

If you import `Data.Monoid` you will have the following definitions
available:

~~~~ {.haskell}
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a

data Either a b = Left a | Right b
~~~~


# Language hitch

Did you try to write code like this?

~~~~ {.haskell}
instance Monoid (Either a b) where
    mempty = Left {- what ??? -}

    Right a `mappend` _ = Right a
    _       `mappend` b = b
~~~~

You surely ran into trouble while trying to define `mempty`.

Why?


# Type quantification

In Haskell, type variables are *quantified*.

They stand in for all types in a given domain.

If there's no typeclass mentioned, a type variable is implicitly
*universally* quantified.

We can write these quantifiers explicitly:

~~~~ {.haskell}
length :: forall a. [a] -> Int
~~~~

"The `length` function must accept any list, no matter what type of
data it contains."


# Universal quantification

Why is universal quantification relevant here?

~~~~ {.haskell}
instance Monoid (Either a b) where
    mempty = Left {- what ??? -}
~~~~


# Universal quantification

Why is universal quantification relevant here?

~~~~ {.haskell}
instance Monoid (Either a b) where
    mempty = Left {- what ??? -}
~~~~

Since `mempty` gives a "zero element", it must somehow produce a zero
element for the type `a`.

But since `a` is universally quantified, it stands in for *every
type*.

Clearly there is no one legal value that is of every type.

It is impossible to write a sensible instance.


# A possible fix

This won't typecheck either:

~~~~ {.haskell}
instance Monoid (Either String a) where
    mempty = Left "fnord"

    Right a `mappend` _ = Right a
    _       `mappend` b = b
~~~~

However, we can make it compile by adding the following to the top of
our source file:

~~~~ {.haskell}
{-# LANGUAGE FlexibleInstances #-}
~~~~


# Pragmas

This is a specially formatted comment:

~~~~ {.haskell}
{- i am a normal comment -}

{-# i am a special comment #-}
~~~~

"Special" comments usually contain directives ("pragmas") that change
the compiler's behaviour.

The `LANGUAGE` pragma enables non-standard language features.

~~~~ {.haskell}
{-# LANGUAGE FlexibleInstances #-}
~~~~

`FlexibleInstances` makes the compiler consider
[more typeclass instances as legal](http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#instance-rules)
than the Haskell 98 standard allows.


# More about pragmas

You'll see a few more pragmas as we progress.

Some are widely used, others are not.

Some are safe, others are not...

* up to and including allowing the typechecker to go into an infinite
  loop! (`UndecidableInstances`)

`FlexibleInstances` is widely used and often safe.


# Back to our fix

This *will* typecheck:

~~~~ {.haskell}
{-# LANGUAGE FlexibleInstances #-}

instance Monoid (Either String a) where
    mempty = Left "fnord"

    Right a `mappend` _ = Right a
    _       `mappend` b = b
~~~~

But is it canonical?


# Canonicality

Why worry about our `Monoid` instance being canonical?

Any time you declare an instance of any typeclass:

* It is automatically made available to every module that imports your
  module.

* You can't say "I don't want to import instance `X`" :-(

If you define a weird instance of a popular typeclass, you'll "infect"
people who import your module.

* Make sure your instances make sense!


# Finally!

Via use of `newtype`, we don't accidentally associate a silly `Monoid`
instance with `Either String a`.

~~~~ {.haskell}
{-# LANGUAGE FlexibleInstances #-}

import Data.Monoid

newtype FirstRight a b = FirstRight {
    getFirstRight :: Either a b
  }

instance Monoid (FirstRight String a) where
  mempty = FirstRight (Left "suxx0rz")

  a@(FirstRight (Right _)) `mappend` _ = a
  _                        `mappend` b = b
~~~~


# HTTP POST

Let's upload some vitally important data to a server.

~~~~
curl --data foo=bar --verbose \
  http://httpbin.org/post
~~~~


# Multipart form upload

When we POST multipart data to a form (e.g. uploading a photo), some
information is mandatory, while other stuff is optional.

~~~~ {.haskell}
data Part = Part {
    -- name of the <input> tag this belongs to
      name        :: String
    -- filename of file we're uploading
    , fileName    :: Maybe FilePath
    -- type of file
    , contentType :: Maybe ContentType
    -- file contents
    , body        :: String
    } deriving (Show)
~~~~


# Uploading data

Suppose we want to build a HTTP client that supports POST.

Web pages tend to expect multipart form data, while REST APIs have
different needs.

Here are some types that let us represent a POST body.

~~~~ {.haskell}
type Param = (String, String)

type ContentType = String

data Payload = NoPayload
             | Raw ContentType String
             | Params [Param]
             | FormData [Part]
               deriving (Show)
~~~~

Can you write a `Monoid` instance for `Payload`?

Decide for yourself, then discuss with a partner for 2 minutes.


# Huh

This part is easy enough:

~~~~ {.haskell}
instance Monoid Payload where
    mempty = NoPayload

    mappend NoPayload b = b
    mappend a NoPayload = a

    mappend (Params a) (Params b) = Params (a++b)

	{- ... -}
~~~~

What about the rest of `mappend`?


# Semantic problems

It is easy to see how we can glom together `Params` or `FormData`.

~~~~ {.haskell}
data Payload = NoPayload
             | Raw ContentType String
             | Params [Param]
             | FormData [Part]
~~~~

However, mixing `Raw` with `Params`, or `Params` with `FormData`, is
nonsensical.

A straightforward `Monoid` instance will have to crash (!!!) if we try
this.


# Handling failure (badly)

What if we use the `Maybe` type to represent a failed attempt to
`mappend`?

~~~~ {.haskell}
{-# LANGUAGE FlexibleInstances #-}

-- I dropped the NoPayload constructor. Why?
data Payload = Raw ContentType String
             | Params [Param]
             | FormData [Part]
               deriving (Show)

instance Monoid (Maybe Payload) where
  mempty = Nothing

  mappend Nothing b = b
  mappend a Nothing = a

  mappend (Just (Params a)) (Just (Params b))
    = Just (Params (a++b))
  mappend (Just (FormData a)) (Just (FormData b))
    = Just (FormData (a++b))
  mappend _ _ = Nothing
~~~~


# Yay?

This compiles, but it has a conceptual problem.

* Every time we use `mappend`, we have to pattern-match the result to
  see if the `mappend` succeeded.

In API design circles, this is called "crappy".

But wait, it gets worse!


# O error message, where art thou?

Let me try this in `ghci`:

~~~~ {.haskell}
Just (Params []) `mappend` Just (Params [])
~~~~


# Overlapping instances

Remember `FlexibleInstances`?

It allowed us to write a `Monoid` instance for the type `Maybe
Payload`.

Trouble is, `Data.Monoid` already defined an instance for `Maybe a`.

`FlexibleInstances` allows these two *definitions* to coexist happily.

But when we want to *use* an instance, GHC doesn't know which one to
use!


# Overlapping instances

Enter the `OverlappingInstances` pragma:

~~~~ {.haskell}
{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
~~~~

This allows multiple instances to coexist *and* be used.

The most specific instance that is visible will be used.

A very handy extension!

* Also a big semantic gun pointing at your foot.


# Problems with overlapping instances

Why worry about `OverlappingInstances`?

* Makes it very easy for incorrect programs to still typecheck.

* Can cause confusing error messages.

* A program that typechecks can have its meaning changed by adding an
  instance declaration in some remote module.

On the plus side, you can
[publish papers about their problems](http://web.cecs.pdx.edu/~jgmorris/pubs/morris-icfp2010-instances.pdf),
so they're not bad for an academic career.


# Checking in

We have a `Monoid` instance that:

* Has a janky API

* Uses a dodgy language extension

Can we do better?


# Phantom types

Let's add a type parameter on the left hand side of our `Payload`
type.

~~~~ {.haskell}
data Payload a = NoPayload
               | Raw ContentType String
               | Params [Param]
               | FormData [Part]
               deriving (Show)
~~~~

The type variable `a` *does not appear in the RHS*.

We call this a *phantom type*.

What's it for?


# A tiny upload API

~~~~ {.haskell}
param :: String -> String -> Payload [Param]
param name value = Params [(name, value)]
~~~~

~~~~ {.haskell}
filePart :: String -> FilePath -> IO (Payload [Part])
filePart name path = do
  body <- readFile name
  return (FormData [Part name (Just path) Nothing body])
~~~~


# Consider the types

~~~~ {.haskell}
param :: String -> String
      -> Payload [Param]

filePart :: String -> FilePath
         -> IO (Payload [Part])
~~~~

Notice:

* The first function returns a `Payload [Param]`

* The second returns a `Payload [Part]`

The phantom parameter makes these *distinct types*.

* The runtime representation is the same in each case.

* The compiler prevents us from mixing the two by accident.


# Code moment

Please write a body for `addParams` below.

~~~~ {.haskell}
instance Monoid (Payload [Param]) where
    mempty = NoPayload
    mappend = addParams
~~~~

Download the code you'll need:

~~~~
curl -O http://www.scs.stanford.edu/14sp-cs240h/PayloadPhantom.hs
~~~~

You have five minutes.


# Making this all work

We have a constrained public API for creating `Payload` values.

~~~~ {.haskell}
param :: String -> String -> Payload [Param]

filePart :: String -> FilePath -> IO (Payload [Part])

fileString :: String -> Maybe FilePath -> String -> (Payload [Part])
~~~~

How do we enforce this?

We export the *name* of the type `Part`, but *not any of its
constructors*.


# Exporting a type

The `(..)` notation below means "export the type `Part` and all of its
constructors".

~~~~ {.haskell}
module PayloadPhantom
    (
      Part(..)
    {- ... trimmed out ... -}
    ) where
~~~~


# Exporting a type

The `(..)` notation below means "export the type `Part` and all of its
constructors".

~~~~ {.haskell}
module PayloadPhantom
    (
      Part(..)
    {- ... trimmed out ... -}
    ) where
~~~~

Notice that we omit the `(..)` below, meaning "export the type
`Payload`, but *not any of its constructors*".

~~~~ {.haskell}
module PayloadPhantom
    (
      Part(..)
    , Payload -- no constructors
    {- ... trimmed out ... -}
    ) where
~~~~


# Exporting a type

The `(..)` notation below means "export the type `Part` and all of its
constructors".

~~~~ {.haskell}
module PayloadPhantom
    (
      Part(..)
    {- ... trimmed out ... -}
    ) where
~~~~

So we export the `Payload` type, and *only* the functions that we
defined and control ("smart constructors") that construct values of
this type.

~~~~ {.haskell}
module PayloadPhantom
    (
      Part(..)
    , Payload -- no constructors
	, param
	, filePart
	, fileString
    {- ... trimmed out ... -}
    ) where
~~~~


# Trying it out

In `ghci`:

~~~~
ghci> param "foo" "bar" <> param "baz" "quux"
Params [("foo","bar"),("baz","quux")]
~~~~

This uses my favourite operator from `Data.Monoid`:

~~~~ {.haskell}
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
~~~~

What do we get if we try this?

~~~~ {.haskell}
param "foo" "bar" <> fileString "baz" Nothing "quux"
~~~~


# Last of the monoids

For which of the following should we write `Monoid` instances?

~~~~ {.haskell}
data Payload a = NoPayload
               | Raw ContentType String
               | Params [Param]
               | FormData [Part]
               deriving (Show)
~~~~


# Why care so much about monoids?

Monoids have many merits:

* Simple

* Easy for clients to use

* Force you to address API design problems early on


# Monoids without an identity

Like the abstract algebraic approach?

A package on Hackage named `semigroups` gives us monoids *without* an
identity operation: semigroups.

Alas:

* The `Monoid` type was developed before the `semigroups` package

* The two should be related, but thanks to this accident of history
  are not


# Principles

Why care about phantom types and monoids?

* We want to build the simplest correct libraries we can

Monoids help us focus on simplicity.

Phantom types make it easier to build APIs where flat-out broken
behaviours can be made impossible by the compiler.


# Mutable variables

We've already seen the very handy `MVar` type, which represents a
"blocking mutable box": we can put a value in or take one out, but
we'll block if we put when full or take when empty.

Even though `MVar`s are the fastest blocking concurrent structure in
the industry (they made the the Kessel Run in less than twelve
parsecs!), we don't always want blocking semantics.

For cases where we want *non-*blocking updates, there's the `IORef`
type, which gives us mutable references.

~~~~ {.haskell}
import Data.IORef

newIORef    :: a -> IO (IORef a)

readIORef   :: IORef a -> IO a
writeIORef  :: IORef a -> a -> IO ()

modifyIORef :: IORef a -> (a -> a) -> IO ()
~~~~


# Managing mutation

Application writers are often faced with a question like this:

* I have a big app, and parts of it need their behaviour tweaked by an
  administrator at runtime.

There are of course many ways to address this sort of problem.

Let's consider one where we use a reference to a piece of config data.

Any code that's executing in the `IO` monad can, if it knows the name of
the config reference, retrieve the current config:

~~~~ {.haskell}
curCfg <- readIORef cfgRef
~~~~

The trouble is, ill-behaved code could clearly also *modify* the
current configuration, and leave us with a debugging nightmare.


# Phantom types to the rescue!

Let's create a new type of mutable reference.

We use a phantom type `t` to statically track whether a piece of code
is allowed to modify the reference or not.

~~~~ {.haskell}
import Data.IORef

newtype Ref t a = Ref (IORef a)
~~~~

Remember, our use of `newtype` here means that the `Ref` type only
exists at compile time: it imposes *no* runtime cost.

Since we are using a phantom type, we don't even need values of our
access control types:

~~~~ {.haskell}
data ReadOnly
data ReadWrite
~~~~

We're already in a good spot!  Not only are we creating
compiler-enforced access control, but it will have *zero* runtime
cost.


# Creating a mutable reference

To create a new reference, we just have to ensure that it has the
right type.

~~~~ {.haskell}
newRef :: a -> IO (Ref ReadWrite a)
newRef a = Ref `fmap` newIORef a
~~~~


# Reading and writing a mutable reference

Since we want to be able to read both read-only and read-write
references, we don't need to mention the access mode when writing a
type signature for `readRef`.

~~~~ {.haskell}
readRef :: Ref t a -> IO a
readRef (Ref ref) = readIORef ref
~~~~

Of course, code can only write to a reference if the compiler can
statically prove (via the type system) that it has write access.

~~~~ {.haskell}
writeRef :: Ref ReadWrite a -> a -> IO ()
writeRef (Ref ref) v = writeIORef ref v
~~~~


# Converting a reference to read-only

This function allows us to convert any kind of reference into a
read-only reference:

~~~~ {.haskell}
readOnly :: Ref t a -> Ref ReadOnly a
readOnly (Ref ref) = Ref ref
~~~~

In order to prevent clients from promoting a reference from read-only
to read-write, we do *not* provide a function that goes in the
opposite direction.

We also use the familiar technique of constructor hiding at the top of
our source file:

~~~~ {.haskell}
module Ref
    (
      Ref, -- export type ctor, but not value ctor
      newRef, readOnly,
      readRef, writeRef
    ) where
~~~~


# Further reading

A *really* good read:

* [Data analysis with monoids](http://twdkz.wordpress.com/2013/05/31/data-analysis-with-monoids/)

Monoids for MapReduce:

* [Google’s MapReduce Programming Model---Revisited](http://userpages.uni-koblenz.de/~laemmel/MapReduce/paper.pdf)
