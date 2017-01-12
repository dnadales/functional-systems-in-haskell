-- | The Dreaded Monomorphism Restriction.

module DMR where

plus = (+)

-- This won't compile: @myInt :: Int@, but the signature below will:
myInt :: Double
myInt = plus 3 2

myDouble :: Double
myDouble = plus 3.5 2.7

-- | A first approximation of the restriction can be stated as "you cannot
-- overload a function onless you provide a type signature".

-- | This will compile, and show type:
--
-- > f0 :: Show a => a -> String
f0 x = show x

-- | This won't compile:
--
-- > Ambiguous type variable ‘a0’ arising from a use of ‘show’
-- > prevents the constraint ‘(Show a0)’ from being solved.
--
-- Adding the type signature solves the problem.
f1 :: (Show a) => a -> String
f1 = \x -> show x

-- | The same applies to this definition:
f2 :: (Show a) => a -> String
f2 = show
