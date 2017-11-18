module ZippersAndLenses where

data PairHole a b = HoleFst b
                  | HoleSnd a
                  deriving (Show, Eq)

data PairZipper a b c = PZ c (PairHole a b) deriving (Show, Eq)
--                         ^
--                         |
--                         --- This is so that we can store the value to work with it

focusFst :: (a, b) -> PairZipper a b a
focusFst (a, b) = PZ a (HoleFst b)

focusSnd :: (a, b) -> PairZipper a b b
focusSnd (a, b) = PZ b (HoleSnd a)

-- The inverse conversion
unfocusFst :: PairZipper c b a -> (a, b)
unfocusFst (PZ a (HoleFst b)) = (a, b)
unfocusFst _                  = undefined

unfocusSnd :: PairZipper a c b -> (a,b)
unfocusSnd (PZ b (HoleSnd a)) = (a,b)
unfocusSnd _                  = undefined

-- * Viewing the focused value.
view :: PairZipper a b c -> c
view (PZ v _) = v

-- > λ> view (focusFst ("hello",0))
-- > "hello"
-- > λ> view (focusSnd ("hello",0))
-- > 0

-- * Editing the focused value
over :: (c -> d)
     -> PairZipper a b c
     -> PairZipper a b d
over f (PZ c r) = PZ (f c) r

-- Exercise: use this to change "foo" into "bar" in ("foo", 84).
--
-- > over (const "bar") . focusFst $ ("foo", 84)
-- > unfocusFst . over (const "bar") . focusFst $ ("foo", 84)
--
-- And now increment the value by 1.
--
-- > unfocusSnd . over (+1) . focusSnd $ ("foo", 84)

-- * Problems with the approach so far

-- 0. We have to specify what field we're focusing at both ends of the pipeline.
