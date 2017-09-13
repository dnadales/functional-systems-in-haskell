% Let's talk about testing

Have any profs ever talked to you about testing?


# Testing in industry

There are a few "states of the art" for testing software:

* Excel spreadsheet full of instructions to follow by hand

  * *I am not making this up*

* Unit tests

* Integration tests

* Fuzz tests


# What am I interested in?

For today, I want to talk about unit tests and their more interesting
descendants.

Shamelessly borrowing from Wikipedia:

~~~~ {.java}
public class TestAdder {
    public void testSum() {
        Adder adder = new AdderImpl();
        assert(adder.add(1, 1) == 2);
        assert(adder.add(1, 2) == 3);
        assert(adder.add(2, 2) == 4);
        assert(adder.add(0, 0) == 0);
        assert(adder.add(-1, -2) == -3);
        assert(adder.add(-1, 1) == 0);
        assert(adder.add(1234, 988) == 2222);
    }
}
~~~~


# What's the problem?

Count the number of test cases below.

~~~~ {.java}
public class TestAdder {
    public void testSum() {
        Adder adder = new AdderImpl();
        assert(adder.add(1, 1) == 2);
        assert(adder.add(1, 2) == 3);
        assert(adder.add(2, 2) == 4);
        assert(adder.add(0, 0) == 0);
        assert(adder.add(-1, -2) == -3);
        assert(adder.add(-1, 1) == 0);
        assert(adder.add(1234, 988) == 2222);
    }
}
~~~~

Okay, don't. It's 7.


# The limits of unit tests

Unit tests are only useful up to a point.

![](patience.png)

Your patience and ability to think up nasty corner cases are VERY
finite.

Best to use them wisely.

But how?


# Outsourcing

For patience, we have computers.

For nasty corner cases, we have random number generators.

Let's put them to use.


# A simple example: UTF-16 encoding

UTF-16 is a Unicode encoding that:

* takes a *code point* (a Unicode character)

* turns it into 1 or 2 16-bit *code units*

Variable length encoding:

* code points below 0x10000 are encoded as a single code unit

* at and above 0x10000, two code units


# Encoding a single code point

We know that `Char` represents a Unicode code point.

The `Word16` type represents a 16-bit value.

~~~~ {.haskell}
import Data.Word (Word16)
~~~~

What should the type signature of `encodeChar` be?

~~~~ {.haskell}
encodeChar :: ???
~~~~


# The base case is easy

We can easily turn the single-code-unit case into some Haskell using a
few handy functions.

~~~~ {.haskell}
import Data.Char (ord)

ord :: Char -> Int

fromIntegral :: (Integral a, Num b) => a -> b
~~~~

We use `fromIntegral` to convert from `Int` to `Word16` because
Haskell will not explicitly coerce for us.

~~~~ {.haskell}
encodeChar :: Char -> [Word16]
encodeChar x
  | w < 0x10000 = [fromIntegral w]
  where w = ord x
~~~~


# The two-code-unit case

To encode code points above 0x10000, we need some new bit-banging
functions.

~~~~ {.haskell}
import Data.Bits ((.&.), shiftR)
~~~~

The `.&.` operator gives us bitwise *and*, while `shiftR` is a right
shift.

~~~~ {.haskell}
encodeChar :: Char -> [Word16]
encodeChar x
  | w < 0x10000 = [fromIntegral w]
  | otherwise   = [fromIntegral a, fromIntegral b]
  where w = ord x
        a = ((w - 0x10000) `shiftR` 10) + 0xD800
        b = (w .&. 0x3FF) + 0xDC00
~~~~


# Basic testing

If you want unit tests, `HUnit` is the package you need.

~~~~ {.haskell}
import Test.HUnit (assertEqual)

testASCII =
  assertEqual "ASCII encodes as one code unit"
    1 (length (encodeChar 'a'))
~~~~


# A bad test

Let's intentionally write a bogus test.

~~~~ {.haskell}
-- 𐆘
badTest = do
  assertEqual "sestertius encodes as one code unit"
    1 (length (encodeChar '\x10198'))
~~~~

If we run this in `ghci`:

~~~~
ghci> badTest
*** Exception: HUnitFailure "sestertius encodes as one code unit\nexpected: 1\n but got: 2"
~~~~

Not pretty, but it works.


# But wait: unit tests?

So I just slammed unit tests and now I'm showing you how to write
them?

Well, we can generalize past the limits of unit tests.


# A proxy for a bigger picture

What do we really want with this test?

~~~~ {.haskell}
testASCII = do
  assertEqual "ASCII encodes as one code unit"
    1 (length (encodeChar 'a'))
~~~~

We are really asserting that *every* ASCII code point encodes as a
single code unit.

~~~~ {.haskell}
testOne char = do
  assertEqual "ASCII encodes as one code unit"
    1 (length (encodeChar char))
~~~~


# Hmm: better?

What if we parameterize our test:

~~~~ {.haskell}
testOne char = do
  assertEqual "ASCII encodes as one code unit"
    1 (length (encodeChar char))
~~~~

And drive it from a harness:

~~~~ {.haskell}
testASCII = mapM_ testOne ['\0'..'\127']
~~~~


# Taking stock

This is better, in that our original test is generalized.

It's also worse, because we're exhaustively enumerating every single
test input.

We get away with it here because Unicode is small, and computers are fast.

But it's the *principle* of the thing: automate better!


# Enter QuickCheck

Forget about `HUnit`, here's the package we'll focus on.

~~~~ {.haskell}
import Test.QuickCheck

prop_encodeOne c = length (encodeChar c) == 1
~~~~

In `ghci`:

~~~~
ghci> quickCheck prop_encodeOne
+++ OK, passed 100 tests.
~~~~


# What just happened?

Why did `quickCheck` say this:

~~~~
+++ OK, passed 100 tests.
~~~~

It did the following:

* *generated* 100 random values for us

* fed each one to `prop_encodeOne`

* ensured that each test passed


# Now I have a headache

Let's look back at our "test function":

~~~~ {.haskell}
prop_encodeOne c = length (encodeChar c) == 1
~~~~

This is *very suspicious*.

We know that `encodeChar` sometimes produces lists of length 2.

So why did our 100 tests pass?


# Starting small

For most types, QuickCheck operates from the handy assumption that
"small" test cases are more useful than big ones.

As tests pass for small random inputs, it generates "bigger" ones.

With just 100 tests, we are simply not likely to generate a code point
that encodes as two code units.


# Behind the scenes: generating values

How does QuickCheck do its thing, anyway?

It needs to be able to generate random values.

This it achieves via typeclasses.

~~~~ {.haskell}
-- Generator type.
data Gen a

-- The set of types for which we
-- can produce random values.
class Arbitrary a where
    arbitrary :: Gen a
~~~~


# Behind the scenes: some machinery

~~~~ {.haskell}
-- Generate a random value within a range.
choose :: Random a => (a,a) -> Gen a
instance Arbitrary Bool where
    arbitrary = choose (False,True)

instance Arbitrary Char {- ... -}
~~~~


# Behind the scenes: testable things

~~~~ {.haskell}
-- Simply protection for a Gen.
data Property = MkProperty (Gen a)

-- The set of types that can be tested.
class Testable prop

-- The instance bodies are not interesting.
instance Testable Bool

instance (Arbitrary a, Show a, Testable prop)
    => Testable (a -> prop)
~~~~

The two instances above are crucial.


# How does this work?

Let's write our test function with a type signature.

~~~~ {.haskell}
prop_encodeOne :: Char -> Bool
prop_encodeOne c = length (encodeChar c) == 1
~~~~

And `quickCheck`:

~~~~ {.haskell}
quickCheck :: Testable prop => prop -> IO ()
~~~~


# Look again

If `quickCheck` accepts `prop_encodeOne`, then the latter must be an
instance of `Testable`.

~~~~ {.haskell}
prop_encodeOne :: Char -> Bool

quickCheck :: Testable prop => prop -> IO ()
~~~~

But how? Via these two instances.

~~~~ {.haskell}
-- Satisfied by the result type
instance Testable Bool

-- Satisfied by the argument and result
instance (Arbitrary a, Show a, Testable prop)
    => Testable (a -> prop)
~~~~


# Long story short

If we pass `quickCheck` a function, then:

* provided its arguments are all instances of `Arbitrary` and `Show`

* *and* provided its result is an instance of `Testable`

*then* `quickCheck` can:

* *generate* arbitrary values of *all* necessary types automatically,

* run our test on those values,

* and ensure that our test always passes


# So what?

We still have a broken test!

`quickCheck` tells us that it always passes---when it shouldn't!

Why? We have to read the source.

~~~~ {.haskell}
module Test.QuickCheck.Arbitrary where

instance Arbitrary Char where
  arbitrary = chr `fmap` oneof [choose (0,127),
                                choose (0,255)]
~~~~

Oh great, QuickCheck will only generate 8-bit characters.

Our assumption that it would eventually generate big-enough inputs was
wrong for `Char`.

Therefore our test can never fail.

How...unfortunate!


# Writing a new Arbitrary instance

So now we face a challenge.

We want a type that is almost exactly like `Char`, but that has a
different `Arbitrary` instance.

To create such a type, we use the `newtype` keyword.

~~~~ {.haskell}
newtype BigChar = Big Char
                deriving (Eq, Show)
~~~~

The type is named `BigChar`; its constructor is named `Big`.

We use `deriving` to reuse the `Eq` instance of the underlying `Char`
type, and to generate a new `Show` instance.


# What next?

We want to be able to flesh this out:

~~~~ {.haskell}
instance Arbitrary BigChar where
    arbitrary = {- ... what? ... -}
~~~~

The highest Unicode code point is 0x10FFFF.

We want to generate values in this range.

We saw this earlier:

~~~~ {.haskell}
-- Generate a random value within a range.
choose :: Random a => (a,a) -> Gen a
~~~~


# Random values: the hard way

In order to use `choose`, we must make `BigChar` an instance of
`Random`.

Here's a verbose way to do it:

~~~~ {.haskell}
import Control.Arrow (first)
import System.Random

instance Random BigChar where
  random                = first Big `fmap` random
  randomR (Big a,Big b) = first Big `fmap` randomR (a,b)
~~~~


# Random values: easier

If we want to avoid the boilerplate code from the previous slide, we
can use a trick:

* The `GeneralizedNewtypeDeriving` language extension

* This lets GHC automatically derive some non-standard typeclass
  instances for us, e.g. `Random`

~~~~ {.haskell}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import System.Random

newtype BigChar = Big Char
                deriving (Eq, Show, Random)
~~~~

* All we did was add `Random` to the `deriving` clause above.

* As the name suggests, this only works with the `newtype` keyword.


# Our instance, and a rerun

An instance with a body:

~~~~ {.haskell}
instance Arbitrary BigChar where
    arbitrary = choose (Big '\0',Big '\x10FFFF')
~~~~

A new test that unwraps a `BigChar` value:

~~~~ {.haskell}
prop_encodeOne3 (Big c) = length (encodeChar c) == 1
~~~~

And let's try it:

~~~~
ghci> quickCheck prop_encodeOne3
*** Failed! Falsifiable (after 1 test):
Big '\317537'
~~~~

Great! Not only did our broken test fail immediately...

...but it gave us a *counterexample*, an input on which our test
function reproducibly fails!


# The magic of QuickCheck

The beauty here is several-fold:

* We write a simple Haskell function that accepts some inputs and
  returns a `Bool`

* QuickCheck generates random test cases for us, and tests our
  function

* If a test case fails, it tells us what the inputs were


# So what?

Unit test way:

* A pile of unit tests that are small variations on a theme

QuickCheck way:

* One property that you expect to hold universally true

* Automatically, randomly generated test inputs

* Counterexamples that help you pinpoint your bugs


# What else?

There's a problem with random inputs when a test fails:

* They're often *big*.

* Big things are difficult for humans to deal with.

* Big values usually take longer to look through.

Starting from a random failing input:

* We'd like to find the *smallest* input that will cause a test to
  fail.

QuickCheck calls this *shrinking*.


#

# Micro-lab: shrink a BigChar

Grab the following source file:

~~~~
curl -O http://www.scs.stanford.edu/16wi-cs240h/ShrinkChar.hs
~~~~

Using `ghci` to do some spelunking, work out a body for `shrinkChar`.

~~~~ {.haskell}
instance Arbitrary BigChar where
  arbitrary      = choose (Big '0',Big '\x10FFFF')
  shrink (Big c) = map Big (shrinkChar c)

-- Write a body for this.
shrinkChar c = undefined
~~~~

You have 5 minutes.


# Generating vs filtering values

Here are two different approaches to generating test values.

First, generate them directly (look at line 2):

~~~~ {.haskell}
prop_encodeOne2 = do
  c <- choose ('\0', '\xFFFF')
  return $ length (encodeChar c) == 1
~~~~

Second, generate any old value, but *filter* such that we get only the
ones that make sense:

~~~~ {.haskell}
-- These two are basically the same, modulo verbosity.

prop_encodeOne4 (Big c) =
  (c < '\x10000') ==> length (encodeChar c) == 1

prop_encodeOne5 = do
  Big c <- arbitrary `suchThat` (< Big '\x10000')
  return $ length (encodeChar c) == 1
~~~~


# Generating vs filtering

It is *usually* more efficient to generate only the values you'll
need, and do no filtering.

Sometimes, it's easier to identify good values when you see them (by
filtering) than to figure out how to generate them.

If QuickCheck has to generate too many values that fail a `suchThat`
or other filter, it will **give up** and may not run as many tests as
you want.

* For both efficiency *and* to ensure that QuickCheck can generate
  enough values to test, it's worth trying to generate only good
  values.


# Mini-lab: more code!

Grab the following source code:

~~~~
curl -O http://www.scs.stanford.edu/16wi-cs240h/Utf16.hs
~~~~

Write a definition for `decodeUtf16`:

~~~~ {.haskell}
decodeUtf16 :: [Word16] -> [Char]
~~~~

Decide on some (two?) QuickCheck tests, write them, and run them.

You have 10 minutes.


# Sizing a test

Test data generators have an implicit size parameter, hidden inside
the `Gen` type.

QuickCheck starts by generating small test cases; it increases the
size as testing progresses.

The meaning of "size" is specific to the needs of an `Arbitrary`
instance.

* The `Arbitrary` instance for lists interprets it as "the maximum
  length of a list of arbitrary values".

We can find the current size using the `sized` function, and modify it
locally using `resize`:

~~~~ {.haskell}
sized  :: (Int -> Gen a) -> Gen a
resize ::  Int -> Gen a  -> Gen a
~~~~


# Lifting

We're hopefully by now familiar with the `Functor` class:

~~~~ {.haskell}
class Functor f  where
    fmap :: (a -> b) -> f a -> f b
~~~~

This takes a pure function and "lifts" it into the functor `f`.

In general, "lifting" takes a concept and transforms it to work in a
different (sometimes more general) setting.

For instance, we can define lifting of functions with the `Monad`
class too:

~~~~ {.haskell}
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f action = do
  b <- action
  return (f b)
~~~~


# fmap and liftM

Notice the similarities between the type signatures:

~~~~ {.haskell}
fmap  :: (Functor f) => (a -> b) -> f a -> f b
liftM :: (Monad m)   => (a -> b) -> m a -> m b
~~~~

All instances of `Monad` can possibly be instances of `Functor`.
Ideally, they'd be defined in terms of each other:

~~~~ {.haskell}
class (Functor m) => Monad m where
    {- blah blah -}
~~~~

For historical reasons, the two classes are separate, so we write
separate instances for them and just reuse the code:

~~~~ {.haskell}
instance Monad MyThingy where
    {- whatever -}

instance Functor MyThingy where
    fmap = liftM
~~~~


# Why the apparent digression?

It turns out that lifting pure functions into monads is really common.

So common, in fact, that `Control.Monad` defines a bunch of extra
combinators for us.

~~~~ {.haskell}
liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m b
liftM2 f act1 act2 = do
  a <- act1
  b <- act2
  return (f a b)
~~~~

These combinators go all the way up to `liftM5`.

Look familiar? Useful?


# A tighter Arbitrary instance

Before:

~~~~ {.haskell}
data Point a = Point a a

instance (Arbitrary a) => Arbitrary (Point a) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (Point x y)
~~~~

After:

~~~~ {.haskell}
import Control.Monad (liftM2)

instance (Arbitrary a) => Arbitrary (Point a) where
    arbitrary = liftM2 Point arbitrary arbitrary
~~~~


# Micro-lab: shrinking a Point

QuickCheck provides us with machinery to shrink tuples.

Make use of this machinery to shrink a `Point`.

~~~~
curl -O http://www.scs.stanford.edu/16wi-cs240h/TestPoint.hs
~~~~

Take 3 minutes.

~~~~ {.haskell}
import Control.Monad
import Test.QuickCheck

data Point a = Point a a
               deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Point a) where
    arbitrary = liftM2 Point arbitrary arbitrary
    -- TODO: provide a body for shrink
    shrink = undefined
~~~~



# Testing a recursive data type

Suppose we have a tree type:

~~~~ {.haskell}
data Tree a = Node (Tree a) (Tree a)
            | Leaf a
              deriving (Show)
~~~~

Here's an obvious `Arbitrary` instance:

~~~~ {.haskell}
instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = oneof [
                  liftM  Leaf arbitrary
                , liftM2 Node arbitrary arbitrary
                ]
~~~~

The `oneof` combinator chooses a generator at random.

~~~~ {.haskell}
oneof :: [Gen a] -> Gen a
~~~~


# What's up, Doc?

Potential trouble:

* This generator may not terminate at all!

* It's likely to produce *huge* trees.

We can use the `sample` function to generate and print some arbitrary
data.

~~~~ {.haskell}
sample :: (Show a) => Gen a -> IO ()
~~~~

This helps us to explore what's going on.


# A safer instance

Here's where the sizing mechanism comes to the rescue.

~~~~ {.haskell}
instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = sized tree

tree :: (Arbitrary a) => Int -> Gen (Tree a)
tree 0 = liftM Leaf arbitrary
tree n = oneof [
           liftM  Leaf arbitrary
         , liftM2 Node subtree subtree
         ]
  where subtree = tree (n `div` 2)
~~~~


# Where all this is going

QuickCheck is pretty great. Take the time to learn to use it.

It's a little harder to learn to use it well than unit tests, but it
pays off big time.

Furthermore:

* We really want to see you provide QuickCheck tests with future labs
  and your final projects.

Enjoy!
