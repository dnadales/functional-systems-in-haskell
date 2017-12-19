Simple lenses
=============

> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE TupleSections #-}
> module MyLenses where
>
> import Data.Functor.Identity
> import Data.Functor.Const

These notes are related to the [Zippers and
Lenses](http://www.scs.stanford.edu/16wi-cs240h/slides/lenses-slides.html)
section.

> data Focused t a b = Focused
>     { focused :: a
>     , rebuild :: b -> t
>     }
>

> type Focuser s t a b = s -> Focused t a b

First exercise: implment `unfocus'`, `view'`, and `over'` whose types are shown
below (we use `'` at the end to distinguish them from the `Lens` functions we
will implement later on):

```haskell
unfocus' :: Focused s a a -> s
view' :: Focuser s t a b -> s -> a
over' :: Focuser s t a b -> (a -> b) -> s -> t
```

Solution:

> unfocus' :: Focused s a a -> s
> unfocus' (Focused e r) = r e
>
> view' :: Focuser s t a b -> s -> a
> view' l s = focused (l s)
> 
> over' :: Focuser s t a b -> (a -> b) -> s -> t
> over' l f s = let Focused e r = l s
>              in r (f e)
>

Second exercise: implement `_1'` and `_2'` (formerly known as `focusFst` and
`focusSnd`) using the functions above:

```haskell
_1' :: Focuser (a, b) (c, b) a c
_2' :: Focuser (a, b) (a, c) b c
```

Solution:

> _1' :: Focuser (a, b) (c, b) a c
> _1' (a, b) = Focused a (\c -> (c, b))
>
> _2' :: Focuser (a, b) (a, c) b c
> _2' (a, b) = Focused b (\c -> (a, c))
>

Third exercise: implement (within two minutes) `focusHead`:

```haskell
focusHead :: Focuser [a] [a] a a
```

> focusHead :: Focuser [a] [a] a a
> focusHead xs = Focused (head xs) (:tail xs)
>

More generic lenses
===================

> type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

Fourth exercise: implement `over` and `view` using this new type, and the
`Identity` and `Const` functors:

```haskell
over :: Lens s t a b -> (a -> c) -> s -> t
view :: Lens s t a b -> s -> a
```

> over :: Lens s t a b -> (a -> b) -> s -> t
> over l f s = runIdentity $ l (Identity . f) s
>
> view :: Lens s t a b -> s -> a
> view l s = getConst $ l Const s

Fifth exercise, implement `_1` and `_2` using the `Lens` type. You can use the
`TupleSection` language extension, which allows to write `(a,)` instead of `\b
-> (a, b)`:

```haskell
_1 :: Lens (a, b) (c, a) a c
```

> _1 :: Lens (a, b) (c, b) a c
> _1 f (a, b) = (,b) <$>  f a
>
> _2 :: Lens (a, b) (a, c) b c
> _2 f (a, b) = (a,) <$> f b

Compare this with the implementation of `_1'`.

```haskell
_1' :: Focuser (a, b) (c, b) a c
_1' (a, b) = Focused a (\c -> (c, b))
```

In the implementation above we needed to return two functions, and we didn't
even had the `Functor` constraint.

Sixth exercise, let's implement `_head` now:

```haskell
_head :: Lens [a] [a] a a
```

Solution:

> _head :: Lens [a] [a] a a
> _head f (x:xs) = (:xs) <$> f x
> _head _ _ = error "_head called on an empty list"
>

Composing accesses:
