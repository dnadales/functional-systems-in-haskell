% Library-level optimization


# We love inlining

In GHC-land, inlining a function is a big deal for performance.

Function application might be cheap:

~~~~ {.haskell}
foo = toUpper

myUpper1 = map foo
~~~~

But *not applying a function at all* has to be cheaper:

~~~~ {.haskell}
myUpper2 = map toUpper
~~~~


# List traversal

Here are a couple of classic definitions from the Prelude:

~~~~ {.haskell}
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

and :: [Bool] -> Bool
and = foldr (&&) True
~~~~

There's an efficiency problem with the definition of `all`.  Can you
spot it?


# A rewrite

Why is this more efficient than its predecessor?

~~~~ {.haskell}
all' p = go
  where go (x:xs)
           | p x       = go xs
           | otherwise = False
        go _           = True
~~~~


# A rewrite

Why is this more efficient than its predecessor?

~~~~ {.haskell}
all' p = go
  where go (x:xs)
           | p x       = go xs
           | otherwise = False
        go _           = True
~~~~

The answer: in the original definition, `map` generates an
"intermediate" list that `and` immediately consumes.

~~~~ {.haskell}
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p
~~~~

Our rewrite does away with the intermediate list.  This can make a big
difference to performance.


# Back to the inliner

If the inliner can see the body of `all'`, it can expand both `all'`
and `p` at the callsite.

Given a definition like this:

~~~~ {.haskell}
allUpper = all' isUpper
~~~~

The inliner could turn it into:

~~~~
allUpper = go
  where go (x:xs)
           | isUpper x = go xs
           | otherwise = False
        go _           = True
~~~~

That's about as efficient as we could hope for.


# Deforestation

This business of getting rid of intermediate data structures is called
[*deforestation*](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.51.646).

Notice that although our manually deforested loop is efficient, it's
harder to follow than this:

~~~~ {.haskell}
allUpper = all' isUpper
~~~~

Fortunately for us, GHC can do some deforestation automatically.


# The build function

For almost 20 years, GHC has been able to deforest compositions of
`foldr`-style primitive recursion.

It does so using a special building block function:

~~~~ {.haskell}
build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []
~~~~

This is called a *list producer*, and it's never used in real code.
Instead, it's a hint to GHC's inliner.


# Controlling the inliner

We use a special pragma to give instructions to GHC's inliner:

~~~~ {.haskell}
{-# INLINE [1] build #-}
~~~~

The simplifier is run repeatedly to perform Core-to-Core
transformations.

Each run of the simplifier has a different *phase number*.  The phase
number decreases towards zero.

You can use `-dverbose-core2core` to see the sequence of phase numbers
for successive runs of the simplifier.

So `INLINE[k] f` means "do not inline `f` until phase `k`, but from
phase `k` down to zero, be very keen to inline it".


# foldr and the inliner

There's a pragma associated with the definition of `foldr` too:

~~~~ {.haskell}
{-# INLINE [0] foldr #-}
~~~~

This ensures that `foldr` will not be inlined until the last stage of
the simplifier.

Why would we care about that?


# Enter the rewrite rule

GHC allows us to take advantage of Haskell's purity in a novel way: it
exposes a *rewrite engine* that we can use.

~~~~ {.haskell}
{-# RULES "map/map"
    forall f g xs.  map f (map g xs) = map (f.g) xs
#-}
~~~~

This tells GHC:

* The name of the rule is `map/map` (can be anything, only used when
  debugging rewrite rules).

* When you see two uses of `map` composed, combine them into a single
  `map` with the two functions composed.

Thus we eliminate an intermediate list.  Nice!


# A rewrite rule for map

~~~~ {.haskell}
{-# RULES "map" [~1]
    forall f xs.
    map f xs = build $ \c n -> foldr (mapFB c f) n xs
 #-}
~~~~

The `~` phase annotation is new.  `INLINE[~k] f` means "be very keen
to inline `f` until (but not including) phase `k`, but from phase `k`
onwards do not inline it".

(Rewrite rules and the inliner use the same phase annotations.)

What's `mapFB`?


# Η (eta) expansion and mapFB

There's a really simple equivalence we've never talked about:

~~~~ {.haskell}
\x -> f x  == f
~~~~

This is called η-equivalence (Greek letter "eta").

* If we rewrite from left to right, it's called η-contraction.

* Rewriting from right to left is called η-expansion.

~~~~ {.haskell}
mapFB :: (elt -> lst -> lst)
      -> (a -> elt)
      -> a -> lst -> lst
mapFB c f = \x ys -> c (f x) ys
{-# INLINE [0] mapFB #-}
~~~~

This `mapFB` function has the `x` and `ys` parameters η-expanded out,
and the `(:)` constructor replaced with `c`.

(If my recollection is correct) we care about the η-expansion of `x`
and `ys` because the rewrite engine needs to see all arguments to an
expression before it will fire a rule.


# The rewrite rule for mapFB

Once we've rewritten `map` to `mapFB`, we can fuse repeated
`map`-based traversals together.

~~~~ {.haskell}
{-# RULES "mapFB"
    forall c f g.
    mapFB (mapFB c f) g = mapFB c (f.g)
 #-}
~~~~


# And back to a list again

~~~~ {.haskell}
{-# RULES "mapList" [1]
    forall f.
    foldr (mapFB (:) f) []  = map f
 #-}
~~~~

This reverses the `foldr`/`mapFB` rule from a few slides back.

Okay, but where are we going with all this?


# The foldr/build rule

Here's the critical rule to make this rewrite stuff work.

~~~~ {.haskell}
{-# RULES "foldr/build"
    forall k z (g :: forall b. (a -> b -> b) -> b -> b) .
    foldr k z (build g) = g k z
 #-}
~~~~

By now we've seen 4 rewrite rules spread across even more slides.
Confused yet? You ought to be!


# How it all works

The rules for `map` work like this (straight from the GHC commentary,
no less).

* Up to (but *not* including) phase 1, we use the "map" rule to
  rewrite all saturated applications of `map` with its `build`/`foldr`
  form, hoping for fusion to happen.

* In phases 1 and 0, we switch off that rule, inline `build`, and
  switch on the "mapList" rule, which rewrites the `foldr`/`mapFB`
  thing back into plain `map`.

* It's important that these two rules aren't both active at once
  (along with `build`'s unfolding) else we'd get an infinite loop
  in the rules.  Hence the activation control below.

* The "mapFB" rule optimises compositions of `map`.

This same pattern is followed by many other functions: `append`,
`filter`, `iterate`, `repeat`, etc.


# A worked example, 1

Let's manually apply our rewrite rules to this expression:

~~~~ {.haskell}
map toUpper (map toLower xs)
~~~~

Applying "map" to the inner expression:

~~~~ {.haskell}
map toUpper (map toLower xs)

-- RULES "map"

map toUpper (build (\c n -> foldr (mapFB c toLower) n xs))
~~~~

Applying "map" again, this time to the outer expression:

~~~~ {.haskell}
map toUpper (build (\c n -> foldr (mapFB c toLower) n xs)) =

-- RULES "map"

build (\c1 n1 ->
       foldr (mapFB c1 toUpper) n1
	     (build (\c0 n0 ->
		     foldr (mapFB c0 toLower) n0 xs)))
~~~~


# A worked example, 2

Applying "foldr/build":

~~~~ {.haskell}
build (\c1 n1 ->
       foldr (mapFB c1 toUpper) n1
             (build (\c0 n0 ->
                     foldr (mapFB c0 toLower) n0 xs)))

-- RULES "map"

build (\c1 n1 -> (\c0 n0 -> foldr (mapFB c0 toLower) n0 xs)
                 (mapFB c1 toUpper) n1)

-- Substitute for c0 and n0

build (\c1 n1 -> foldr (mapFB (mapFB c1 toUpper) toLower) n1 xs)
~~~~


# A worked example, 3

Applying "mapFB":

~~~~ {.haskell}
build (\c1 n1 -> foldr (mapFB (mapFB c1 toUpper) toLower) n1 xs)

-- RULES "mapFB"

build (\c1 n1 -> foldr (mapFB c1 (toUpper . toLower) n1 xs)
~~~~

Inlining `build`:

~~~~ {.haskell}
build (\c1 n1 -> foldr (mapFB c1 (toUpper . toLower) n1 xs)) (:) []

-- INLINE build

foldr (mapFB (:) (toUpper . toLower) [] xs)
~~~~

Applying "mapList":

~~~~ {.haskell}
foldr (mapFB (:) (toUpper . toLower) [] xs)

-- RULES "mapList"

map (toUpper . toLower) xs
~~~~


# Problem?

This foldr/build business is pretty sweet, BUT...

...it only works for `foldr`-style loops.

...it's pretty fragile.

But we know that (strict) left folds are actually very common:

* `length`, `sum`, `mean`, *etc*.

So what's to be done?


# Induction

A list is an inductively-defined type:

* The base case is `[]`

* The $n+1$ case is `(:)`

It tells us how to *produce* more data.


# Turning data upside down: coinduction

Here's another way of dealing with the data:

~~~~ {.haskell}
{-# LANGUAGE Rank2Types #-}

data Stream a =
    forall s. Stream
    (s -> Step s a)   -- observer function
    !s                -- current state

data Step s a = Done
              | Skip !s
              | Yield !a !s
~~~~

The `Stream` type is *coinductive*.  It tells us how to *consume* more
data.

The implementor of the `Stream` type provides two things:

* The current state of the stream (invisible to consumers, since it's
  an existential type)

* An observation function, which a consumer uses to get another
  element from the stream


# From lists to streams, and back again

It's easy to convert between lists and streams.

~~~~ {.haskell}
streamList :: [a] -> Stream a
streamList s  = Stream next s
    where next []       = Done
          next (x:xs)   = Yield x xs

{-# INLINE [0] streamList #-}
~~~~

~~~~ {.haskell}
unstreamList :: Stream a -> [a]
unstreamList (Stream next s0) = unfold s0
  where unfold !s = case next s of
                      Done       -> []
                      Skip s'    -> unfold s'
                      Yield x s' -> x : unfold s'

{-# INLINE [0] unstreamList #-}
~~~~


# Left folds over streams

Not only can we easily write a right fold:

~~~~ {.haskell}
{-# LANGUAGE BangPatterns #-}

foldr :: (Char -> b -> b) -> b -> Stream Char -> b
foldr f z (Stream next s0) = go s0
  where
    go !s = case next s of
              Done -> z
              Skip s' -> go s'
              Yield x s' -> f x (go s')

{-# INLINE [0] foldr #-}
~~~~

We can just as simply write a left fold:

~~~~ {.haskell}
foldl' :: (b -> a -> b) -> b -> Stream a -> b
foldl' f z0 (Stream next s0) = go z0 s0
  where
    go !z !s = case next s of
                 Done       -> z
		 Skip s'    -> go z s'
		 Yield x s' -> go (f z x) s'

{-# INLINE [0] foldl' #-}
~~~~


# Streams vs lists

This stream representation is used internally by several modern
Haskell packages:

* `vector` - fast packed and unpacked arrays

* `text` - efficient support for Unicode text

But why?


# Conversion

We use rewrite rules to eliminate intermediate conversions.

~~~~ {.haskell}
stream :: Text -> Stream Char
{-# INLINE [0] stream #-}

unstream :: Stream Char -> Text
{-# INLINE [0] unstream #-}

{-# RULES "STREAM stream/unstream fusion"
    forall s.
    stream (unstream s) = s
 #-}
~~~~


# Mapping once again

The `map` function for `Text` is defined in terms of the `map`
function over a `Stream Char`.

~~~~ {.haskell}
import qualified Data.Text.Fusion as S

map :: (Char -> Char) -> Text -> Text
map f t = unstream (S.map f (stream t))
{-# INLINE [1] map #-}
~~~~

Why? So we can fuse the intermediate data structures away.

~~~~ {.haskell}
{-# RULES "STREAM map/map fusion"
    forall f g s.
    S.map f (S.map g s) = S.map (\x -> f (g x)) s
 #-}
~~~~


# But why?

We can turn multiple traversals, with intermediate data structures,
into a *single* traversal, with *no* intermediate structures.

* The "stream/unstream" rule leaves only compositions of non-recursive
  functions behind

* We can combine streams using simpler rules, such as "map/map" above

* The final array will be fused from the combined single-pass stream
  pipeline


# The keys to good performance with streams

We have to get the inliner and rewrite rules firing at exactly the
right times, or we lose these nice properties.

That's a subtle business.  Fortunately, it's the library writer's job,
*not* that of the user of the library.

On the other hand, users of the library will see the best performance
if they know how to exploit its behaviour.

* For instance, single-pass algorithms and pipelines win big.


# Is stream fusion awesome? Coding

The programming model is most definitely a pain in the ass.

~~~~ {.haskell}
data I s = I1 !s
         | I2 !s {-# UNPACK #-} !Char
         | I3 !s

intersperse :: Char -> Stream Char -> Stream Char
intersperse c (Stream next0 s0) = Stream next (I1 s0)
    where
      next (I1 s)   = case next0 s of
        Done           -> Done
        Skip s'        -> Skip (I1 s')
        Yield x s'     -> Skip (I2 s' x)
      next (I2 s x) = Yield x (I3 s)
      next (I3 s)   = case next0 s of
        Done           -> Done
        Skip s'        -> Skip    (I3 s')
        Yield x s'     -> Yield c (I2 s' x)
{-# INLINE [0] intersperse #-}
~~~~


# Is stream fusion awesome? Performance

In quite a few cases, for the `text` library, I ended up writing
hand-rolled loops because GHC wasn't doing a good enough job at
eliminating heap allocation.

For instance:

~~~~ {.haskell}
drop :: Int -> Text -> Text
drop n t@(Text arr off len)
    | n <= 0    = t
    | n >= len  = empty
    | otherwise = loop 0 0
  where loop !i !cnt
            | i >= len || cnt >= n   = Text arr (off+i) (len-i)
            | otherwise              = loop (i+d) (cnt+1)
            where d = iter_ t i
{-# INLINE [1] drop #-}

{-# RULES
    "TEXT drop -> fused" [~1]
    forall n t.
    drop n t = unstream (S.drop n (stream t))

    "TEXT drop -> unfused" [1]
    forall n t.
    unstream (S.drop n (stream t)) = drop n t
 #-}
~~~~
