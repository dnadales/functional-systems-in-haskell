{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module GenericProgramming where

class Function f a b | f a -> b where
    apply :: f -> a -> b

instance Function (a -> b) a b where
    apply = id

pairToList :: (Function f b c, Function f a c)
           => f -> (a, b) -> [c]
pairToList f (a, b) = [apply f a, apply f b]

-- we would like to be able to call something like
--
-- > pairToList show (True, Just 3)
--
-- However this:
--
-- > x :: [String]
-- > x = pairToList show (Just (3 :: Int), True)
--
-- ... will result it the following error:
--
-- >     • Ambiguous type variable ‘a0’ arising from a use of ‘pairToList’
-- >       prevents the constraint ‘(Function
-- >                                   (a0 -> String) (Maybe Int) String)’ from being solved.
-- >         (maybe you haven't applied a function to enough arguments?)
-- >       Probable fix: use a type annotation to specify what ‘a0’ should be.
-- >       These potential instance exist:
-- >         instance Function (a -> b) a b
-- >

data ShowF = ShowF
instance (Show a) => Function ShowF a String where
    apply _ = show -- we are ignoring the 'ShowF' argument here.

data FromEnumF = FromEnumF
instance (Enum a) => Function FromEnumF a Int where
    apply _ = fromEnum

strList :: [String]
strList = pairToList ShowF (Just (3 :: Int), True)

-- And this seems to work! Why???
enuList :: [Int]
enuList = pairToList FromEnumF (False, 7 :: Int)

-- ??? What is the idea behind this?
-- class TupleFoldr f z t r | f z t -> r where
--     tupleFoldr :: f -> z -> t -> r

-- instance TupleFoldr (,)
