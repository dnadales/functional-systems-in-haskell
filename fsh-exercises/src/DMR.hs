-- | The Dreaded Monomorphism Restriction.

module DMR where

import           Data.List

plus = (+)

-- If you don't use plus in functions @myInt@ and @myDouble@ below, the type
-- will default to:
--
-- > Integer -> Integer -> Integer
--
-- Instead of what we would expect:
--
-- > Num a => a -> a -> a

myInt :: Double
myInt = plus 3 2

myDouble :: Double
myDouble = plus 3.5 2.7

-- | A first approximation of the restriction can be stated as "you cannot
-- overload a function unless you provide a type signature".

-- | This will compile, and show type:
--
-- > f0 :: Show a => a -> String
f0 x = show x

-- | This won't compile:
--
-- > f1 = \x -> show x
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

-- | However if you define @f1@ and @f2@ in @ghci@ the will compile, but show type:
--
-- > () -> String
--
-- What's going on?


-- * Avoiding repeated computations by means of rule 1:
lenLen xs = (len, len)
  where
    len = genericLength xs

-- If you ask the type of the functions above it will give:
--
-- > :t lenLen
-- > lenLen :: Num t => [a] -> (t, t)
-- > genericLength :: Num a => [b] -> a

-- However the type of @lenLen@ could have been more general:
lenLen' :: (Num b, Num c) => [a] -> (b, c)
lenLen' xs = (genericLength xs, genericLength xs)

lenLen'' :: (Num b, Num c) => [a] -> (b, c)
lenLen'' xs = (len, len)
  where
    len :: Num a => a -- This is needed to make the code compile! Otherwise the
                      -- monomorphic restriction rules will infer @len :: b@
    len = genericLength xs

-- if you comment the type of @len@ in @lenLen''@ above, then it won't compile
-- because the @len@ will be given type @b@. It seems that by making @len :: b@
-- we're avoiding computing len twice, since @len :: Num a => a@ means that
-- @len@ is no longer a constant but a function (Num a) is the implicit
-- parameter.

-- * Preventing ambiguitiy by means of rule 1.

myParse t = n
  where [(n, s)] = reads t

-- * Not defautable constraints

-- | @mSort@ without a default type will result in the error:
--
-- > Ambiguous type variable ‘a0’ arising from a use of ‘sort’
--   prevents the constraint ‘(Ord a0)’ from being solved.
mSort :: (Ord a) => [a] -> [a]
mSort = sort
