% Zippers and lenses

Let's talk about well-behaved Haskell programs for a bit.

So well-typed but non-terminating constructs such as the following are
forbidden:

~~~~ {.haskell}
loop :: Bool
loop = loop

wtf :: Bool
wtf = undefined

crash :: Bool
crash = error "fnord"
~~~~


# Back to basics

How many values can we construct from the following type?

~~~~ {.haskell}
data Bool = False | True
~~~~


# Ordering

Another well-known type:

~~~~ {.haskell}
data Ordering = LT | EQ | GT
~~~~

Clearly we can construct three different values of this type.


# A zero-valued type

In Haskell 2010, we can create types from which *no* values can be
constructed:

~~~~ {.haskell}
data Empty
~~~~

This type has no value constructors (and we can't use `deriving`
syntax on it).

"Why?" you may ask. For programming with types while compiling.


# Zero, one, two...

So big deal, we can create types with zero or more constructors:

~~~~ {.haskell}
data Empty
~~~~

~~~~ {.haskell}
data One = One
~~~~

~~~~ {.haskell}
data Bool = False | True
~~~~


# Adding some parameters

Given these:

~~~~ {.haskell}
data Ordering = LT | EQ | GT

data Bool = False | True
~~~~

Here's another type to ponder.

~~~~ {.haskell}
data A = A Bool
       | B Ordering
~~~~

Spend a minute working out how many values this can have.  We'll do a
quick poll.


# Abstracting I

Now how many values can this familiar type have?

~~~~ {.haskell}
(a,b)
~~~~


# Abstracting II

Now how many values can this familiar type have?

~~~~ {.haskell}
data Either a b = Left a | Right b
~~~~


# Algebra I

Why do we refer to these as *product* types?

~~~~ {.haskell}
(a,b,c)

data Product a b c = Product a b c
~~~~

They can hold a number of values equal to:

$a \times b \times c$


# Algebra II

The same holds for the naming of *sum* types:

~~~~ {.haskell}
data Sum a b c = A a
               | B b
			   | C c
~~~~

They can hold a number of values equal to:

$a + b + c$


# Working with nested data

Suppose we're writing a benchmarking tool.  We'll take criterion as an
example.

Measurements produce noisy samples.


# The effect of outliers

We want to understand how outliers in our sample data affect the
sample mean and standard deviation.

~~~~ {.haskell}
data OutlierEffect
    = Unaffected -- ^ Less than 1% effect.
    | Slight     -- ^ Between 1% and 10%.
	| Moderate   -- ^ Between 10% and 50%.
	| Severe     -- ^ Above 50% (i.e. measurements
                 -- are useless).
~~~~

Our `OutlierEffect` type is embedded in another type that carries
extra information.

~~~~ {.haskell}
data OutlierVariance = OutlierVariance {
      ovEffect      :: OutlierEffect
    , ovDescription :: String
    , ovFraction    :: Double
    }
~~~~


# More nesting

And `OutlierVariance` is buried in another type.

~~~~ {.haskell}
data SampleAnalysis = SampleAnalysis {
      anMean       :: [Double]
    , anStdDev     :: [Double]
    , anOutlierVar :: OutlierVariance
    }
~~~~

Which is nested in *yet another* type.

~~~~ {.haskell}
data Payload = Payload {
      sample         :: [Double]
    , sampleAnalysis :: SampleAnalysis
    , outliers       :: Outliers
    }
~~~~


# Accessing data is easy

Even with three levels of nesting, it's easy to access an
`OutlierEffect` given a `Payload`.

~~~~ {.haskell}
effect :: Payload -> OutlierEffect
effect = ovEffect . anOutlierVar . sampleAnalysis
~~~~

These record accessor functions are handy!


# Updates, not so much

OK, so suppose we want to "*modify*" an `OutlierEffect` buried in a
`Payload`.

~~~~ {.haskell}
editEffect :: (OutlierEffect -> OutlierEffect)
           -> Payload -> Payload
editEffect eff payload =
    payload {
      sampleAnalysis = analysis {
        anOutlierVar = variance {
          ovEffect = eff effect
        }
      }
    }
  where analysis = sampleAnalysis payload
        variance = anOutlierVar analysis
        effect   = ovEffect variance
~~~~

This is hideous!  It hardly even looks like Haskell.


# What was this?

We just saw Haskell's record update syntax in action.

~~~~ {.haskell}
setAddrZip :: Zip -> Address -> Address
setAddrZip zip addr = addr { addrZip = zip }
~~~~

This notation means:

* Make a complete copy of the record `addr`.

* When copying, set the `addrZip` field to `zip`.

It's a way of "editing" a value that leaves the original unchanged,
but doesn't require us to specify every field to copy.

It's also a very non-composable hack, as we saw.


# What we actually want

Our demands:

1. Access fields within records.

1. Compose *accesses*, so that we can inspect fields within nested
   records.

1. Update fields within records.

1. Compose *updates*, so that we can modify fields within nested
   records.

With Haskell's record syntax, we get #1 and #2, sort of #3 (if we
squint), and #4 is hideous.


# What to do?

Suppose we have a pair.

~~~~ {.haskell}
(a,b)
~~~~

We'd like to edit its second element.

~~~~ {.haskell}
editSnd :: (b -> c) -> (a,b) -> (a,c)
editSnd f (a,b) = (a, f b)
~~~~

Let's refer to the idea that we're interested in the second element as
*focusing* on it.

It's equally easy to edit the first element.

~~~~ {.haskell}
editFst :: (a -> c) -> (a,b) -> (c,b)
editFst f (a,b) = (f a, b)
~~~~


# Holes

Let's refer to the slot we want to fill when editing a tuple as a
*hole*.

Here, the hole is in the second position.

~~~~ {.haskell}
editSnd :: (b -> c) -> (a,b) -> (a,c)
editSnd f (a,b) = (a, f b)
~~~~

And here, it's in the first.

~~~~ {.haskell}
editFst :: (a -> c) -> (a,b) -> (c,b)
editFst f (a,b) = (f a, b)
~~~~


# Counting holes

If we drop the `b` from `(a,b)`, how many values does the resulting
pseudo-type have?


# Counting holes

If we drop the `b` from `(a,b)`, how many values does the resulting
pseudo-type have?

What if we drop `a` from `(a,b)`?


# Counting holes

If we drop the `b` from `(a,b)`, how many values does the resulting
pseudo-type have?

What if we drop `a` from `(a,b)`?

If we want to drop some arbitrary field from `(a,b,c)`, we can
represent this via a type.

~~~~ {.haskell}
data Hole3 a b c = AHole b c
                 | BHole a c
                 | CHole a b
~~~~



# Counting holes

We can write the number of values of `(x,x,x)` as $x \times x \times
x$, or $x^3$.

If we substitute `x` for `a`, `b`, and `c` below, how many different
values of type `Hole3` can there be?

~~~~ {.haskell}
data Hole3 a b c = AHole b c
                 | BHole a c
                 | CHole a b
~~~~


# Counting holes

We can write the number of values of `(x,x,x)` as $x \times x \times
x$, or $x^3$.

If we substitute `x` for `a`, `b`, and `c` below, how many different
values of type `Hole3` can there be?

~~~~ {.haskell}
data Hole3 x x x = AHole x x
                 | BHole x x
                 | CHole x x
~~~~

Hmm, that's $3x^2$.

Does this remind you of symbolic differentiation?


# Back to pairs

Here's a hole type for pairs.

~~~~ {.haskell}
data PairHole a b = HoleFst b
                  | HoleSnd a
~~~~

If we pull a value out of the hole, we need to store it somewhere so
we can work with it.

~~~~ {.haskell}
data PairZipper a b c = PZ c (PairHole a b)
~~~~

Why do we have an extra type parameter `c`?

* So we can choose what type of value to store in the hole later.


# Quick exercise

Please provide bodies for the two undefined functions below.

You have one minute.

~~~~ {.haskell}
data PairHole a b = HoleFst b
                  | HoleSnd a

data PairZipper a b c = PZ c (PairHole a b)

focusFst :: (a,b) -> PairZipper a b a
focusFst = undefined

focusSnd :: (a,b) -> PairZipper a b b
focusSnd = undefined
~~~~

Skeleton: [http://cs240h.scs.stanford.edu/Hole1.hs](http://cs240h.scs.stanford.edu/Hole1.hs)


# My solution

~~~~ {.haskell}
data PairHole a b = HoleFst b
                  | HoleSnd a

data PairZipper a b c = PZ c (PairHole a b)

focusFst :: (a,b) -> PairZipper a b a
focusFst (a,b) = PZ a (HoleFst b)

focusSnd :: (a,b) -> PairZipper a b b
focusSnd (a,b) = PZ b (HoleSnd a)
~~~~

A nice thing about this?

* The polymorphism forces there to be only one possible
  implementation.


# The inverse conversion

We obviously also need to be able to convert from a zipper back to a
pair.

~~~~ {.haskell}
unfocusFst :: PairZipper a b a -> (a,b)
unfocusFst (PZ a (HoleFst b)) = (a,b)

unfocusSnd :: PairZipper a b b -> (a,b)
unfocusSnd (PZ b (HoleSnd a)) = (a,b)
~~~~


# Accessing the focused value

Now that we have focus functions to get the first or second element of
a pair, we can write a generic accessor function for our zipper type.

~~~~ {.haskell}
view :: PairZipper a b c -> c
view (PZ c _) = c
~~~~

Try in `ghci`:

~~~~ {.haskell}
>>> view (focusFst ("hello",1))
"hello"
>>> view (focusSnd ("hello",1))
1
~~~~


# Editing the focused value

This is the more fun part.

~~~~ {.haskell}
over :: (c -> c)
     -> PairZipper a b c
     -> PairZipper a b c
over f (PZ c l) = PZ (f c) l
~~~~

Once again in `ghci`:

~~~~ {.haskell}
>>> unfocusSnd . over succ . focusSnd $ ("hello",1::Int)
("hello",2)
~~~~


# Editing part deux

What will this print in `ghci`?

~~~~ {.haskell}
>>> unfocusFst . over length . focusFst $ ("hello",1::Int)
~~~~


# Editing part deux

What will this print in `ghci`?

~~~~ {.haskell}
>>> unfocusFst . over length . focusFst $ ("hello",1::Int)
~~~~

It's a type error! `over` is not polymorphic enough.

Bad version:

~~~~ {.haskell}
over :: (c -> c)
     -> PairZipper a b c
     -> PairZipper a b c
over f (PZ c l) = PZ (f c) l
~~~~

The good version allows editing to change the type of the field being
edited:

~~~~ {.haskell}
over :: (c -> d)
     -> PairZipper a b c
     -> PairZipper a b d
over f (PZ c l) = PZ (f c) l
~~~~


# Hmm

This approach has problems.

We have to specify what field we're focusing at both ends of the
"pipeline".

* This is repetitive.

Can we compose these so that we can 'focusFst' then 'focusSnd' to get
another zipper?

* No.


# Gluing things together

Instead of keeping `focusFst` and `unfocusFst` separate and wiring
them together by hand, let's manage them automatically.

~~~~ {.haskell}
data Focused t a b = Focused {
    focused :: a
  , rebuild :: b -> t
  }
~~~~

A `Focused` is a pair consisting of:

* The focused element

* A function that knows how to reconstitute the original value

~~~~ {.haskell}
type Focuser s t a b = s -> Focused t a b
~~~~

A `Focuser` is a function that takes a value and gives us a `Focused`.


# Why so polymorphic?

Recall that our original definition of `over` wasn't polymorphic
enough.

We could not change the type of the first element while editing a
pair.

~~~~ {.haskell}
>>> unfocusFst . over length . focusFst $ ("hello",1::Int)
~~~~

Well, `Focused` and `Focuser` have so many type parameters to give
exactly this generality.


# Another look

~~~~ {.haskell}
data Focused t a b = Focused {
    focused :: a
  , rebuild :: b -> t
  }
~~~~

`Focused` is in effect saying:

* I am focusing on an `a`

* I might change its type to `b`

* When I am eventually done focusing, I will give you back a `t`
  (which is `s` with every `a` replaced with `b`)


# Another look

~~~~ {.haskell}
type Focuser s t a b = s -> Focused t a b
~~~~

The "meaning" of `Focuser` is:

* You give me an `s`

* I will focus on an `a`

* I might change its type to `b`

* When I'm done focusing, I might change the thing I give you back
  from `s` to `t` (once again `s` with every `a` replaced with `b`)


# Some machinery

Functions for working with these types:

~~~~ {.haskell}
unfocus :: Focused s a a -> s
unfocus (Focused focused rebuild) = rebuild focused

view :: Focuser s t a b -> s -> a
view l s = focused (l s)

over :: Focuser s t a b -> (a -> b) -> s -> t
over l f s = let Focused focused rebuild = l s
             in rebuild (f focused)
~~~~

Our friends `focusFst` and `focusSnd` recast in this framework:

~~~~ {.haskell}
_1 :: Focuser (a,b) (c,b) a c
_1 (a,b) = Focused a (\c -> (c,b))

_2 :: Focuser (a,b) (a,c) b c
_2 (a,b) = Focused b (\c -> (a,c))
~~~~


# Your turn

Here's your scaffolding:

~~~~ {.haskell}
data Focused t a b = Focused {
    focused :: a
  , rebuild :: b -> t
  }

type Focuser s t a b = s -> Focused t a b
~~~~

Take two minutes to implement this:

~~~~ {.haskell}
focusHead :: Focuser [a] [a] a a
focusHead = undefined
~~~~

It should focus on the head of a list, such that we can run this in
`ghci`:

~~~~ {.haskell}
>>> over focusHead toUpper "anita"
"Anita"
~~~~


Skeleton: [http://cs240h.scs.stanford.edu/Focus.hs](http://cs240h.scs.stanford.edu/Focus.hs)


# Abstracting again

Our two most interesting functions have a lot in common.

~~~~ {.haskell}
over :: Focuser s t a b -> (a -> b) -> s -> t
view :: Focuser s t a b             -> s -> a
~~~~

How could we unify these types?

* By using abstraction to decide what type to use.

~~~~ {.haskell}
wat :: Focuser s t a b -> (a -> f b) -> s -> f t
~~~~

# Type-level fun

Here, `f` is a type-level function.

~~~~ {.haskell}
wat :: Focuser s t a b -> (a -> f b) -> s -> f t
~~~~

If we supply the type-level identity function, `f` disappears and we
get out the type of `over`:

~~~~ {.haskell}
wat  :: Focuser s t a b -> (a -> f b) -> s -> f t
over :: Focuser s t a b -> (a ->   b) -> s ->   t
~~~~

With the type-level `const a` function, we get the type of `view`:

~~~~ {.haskell}
wat  :: Focuser s t a b -> (a -> f b) -> s -> f t
view :: Focuser s t a b {- ignored -} -> s -> a
~~~~


# Type-level identity

Defined in
[`Data.Functor.Identity`](http://hackage.haskell.org/package/transformers/docs/Data-Functor-Identity.html):

~~~~ {.haskell}
newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)
~~~~


# Type-level const

Defined in [`Control.Applicative`](http://hackage.haskell.org/package/base/docs/Control-Applicative.html#v:Const):

~~~~ {.haskell}
newtype Const a b = Const { getConst :: a }

instance Functor (Const a) where
    fmap _ (Const v) = Const v
~~~~


# Our final type

~~~~ {.haskell}
{-# LANGUAGE RankNTypes #-}

type Lens s t a b = forall f. Functor f =>
                    (a -> f b) -> s -> f t
~~~~

From our perspective as lens library writers:

We use `forall` here to make it clear that *we control* the `Functor`
we use, not our caller.

We choose `Identity` or `Const a` to get the right types for `over`
and `view`.


# Our final type

~~~~ {.haskell}
{-# LANGUAGE RankNTypes #-}

type Lens s t a b = forall f. Functor f =>
                    (a -> f b) -> s -> f t
~~~~

From our perspective as lens library writers:

We have to explain this type to users.

* Give me an `s`, and I will focus on its elements of type `a`

* If you use `over` to edit, you can change those `a` types to `b`

* Once you're done editing, you'll get back a `t`, which (if you
  didn't change `a` to `b`) will be `s`


# New machinery

~~~~ {.haskell}
{-# LANGUAGE RankNTypes #-}

import Control.Applicative
import Data.Functor.Identity

type Lens s t a b = forall f. Functor f =>
                    (a -> f b) -> s -> f t

over :: Lens s t a b -> (a -> b) -> s -> t
over l f s = runIdentity (l (Identity . f) s)

view :: Lens s t a b -> s -> a
view l s = getConst (l Const s)
~~~~


# Tuple sections

If we turn on this:

~~~~ {.haskell}
{-# LANGUAGE TupleSections #-}
~~~~

And write this:

~~~~ {.haskell}
(a,)
~~~~

It's equivalent to this:

~~~~ {.haskell}
\b -> (a,b)
~~~~


# More machinery

~~~~ {.haskell}
{-# LANGUAGE TupleSections #-}

_1 :: Lens (a,b) (c,b) a c
_1 f (a,b) = (,b) <$> f a

_2 :: Lens (a,b) (a,c) b c
_2 f (a,b) = (a,) <$> f b

_head :: Lens [a] [a] a a
_head f (a:as) = (:as) <$> f a
~~~~


# Composing access

In `ghci`:

~~~~ {.haskell}
>>> view (_1 . _head) ("foo",True)
'f'
~~~~

Why is this different from the traditional order of composition?

~~~~ {.haskell}
>>> (head . fst) ("foo",True)
'f'
~~~~


# Composition of lenses

What is a lens even *for*?

* It turns an action on a *part* of a structure into an action on the
  *whole* structure.

Thus:

* `_1` and `_2` are *not* "just getters", they take an *entire pair*
  and focus on its first or second element.

* It's `view` and `over` that then determine getter-or-setter nature.

What does it then mean to compose lenses?

If you write `_1 . _head`, you are:

* Taking the entire pair, and focusing on its first element

* Taking the entire pair, and focusing on the head of the list *inside
  the first element of the pair*


#

![](a88.jpg)


# Composing modifications

Let's work out how we would use the lens machinery to give us a pair
with an uppercased first name.

~~~~ {.haskell}
("anita", True)
~~~~


# 1: Why are lenses composable?

At first glance, it's hard to tell why `_1 . _head` even typechecks:

~~~~ {.haskell}
_1    :: Functor f => (a -> f c) -> (a, b) -> f (c, b)
_head :: Functor f => (a -> f a) -> [a] -> f [a]
~~~~

And especially---why can we compose using `.` for function
composition?


# 2: Why are lenses composable?

The key: remembering that a function of 2 arguments is really a
function of 1 arg that returns a function.

~~~~ {.haskell}
_1 :: Functor f =>
      (a -> f c) ->
      ((a, b) -> f (c, b))

_head :: Functor f =>
         (a -> f a) ->
         ([a] -> f [a])

_1._head :: Functor f =>
            (a -> f a) ->
		    ([a], b) -> f ([a], b)
~~~~


# What next?

The best place to start is with the gateway drugs:

* [lens-family-core](http://hackage.haskell.org/package/lens-family-core)
  and [lens-simple](http://hackage.haskell.org/package/lens-simple)
  are easiest to learn

* Also the easiest source to *read*: highly recommended!

The full monty:

* The [lens package](http://lens.github.io/) is *way* more powerful,
  richer, more abstract, more difficult to learn

* A little controversial due to being **huge**

Somewhere in between:

* [microlens](http://hackage.haskell.org/package/microlens) is based
  on lens, but smaller and with fewer dependencies

Lenses in action:

* My [wreq HTTP library](http://www.serpentine.com/wreq)


# Spotter's guide to lens operators

`^.` is `view` (think "getter")

`%~` is `over` (think "editor")

`.~` is `over` -- but accepts a *value* instead of a *function* (think "setter")

`&` is just `$` with arguments flipped

Used as follows:

~~~~ {.haskell}
foo & someField %~ ('a':)
    & otherField .~ 'b'
~~~~

("Thing being modified, followed by modifiers in a chain.")


# So anyway...

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">OH: Outsiders think the learning curve for Haskell is a cliff; Haskellers know it&#39;s a cliff with <a href="https://twitter.com/kmett">@kmett</a> on top building more cliff. <a href="https://twitter.com/hashtag/lca2016?src=hash">#lca2016</a></p>&mdash; Katie Miller (@codemiller) <a href="https://twitter.com/codemiller/status/695516883483828224">February 5, 2016</a></blockquote>
