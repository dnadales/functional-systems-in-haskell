{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module GenericProgramming where

import           Data.Data
import           Data.Function
import           Data.Typeable
import           Unsafe.Coerce

class Function f a b where
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

-- * The derive data typeable extension

-- Using 'Typeable' to make a safe 'cast' function.
mCast :: (Typeable a, Typeable b) => a -> Maybe b
mCast a = fix $ \ ~(Just b) -> if typeOf a == typeOf b
                              then Just $ unsafeCoerce a
                              else Nothing

-- ** Using 'Typeable': 'mkT'

-- | 'mkT' applies a version of 'f' that works on type 'a', if 'b' can be
-- casted to 'a'. Otherwise it behaves like the identity function.
mkT :: (Typeable a, Typeable b) => (b -> b) -> a -> a
mkT f a =
    case mCast f of
        Nothing -> a
        Just g  -> g a -- 'g' is applied to 'a`, so g :: a -> a
                       -- then:
                       -- mCast f :: Maybe (a -> a)

newtype Salary = Salary Double deriving (Show, Data, Typeable)

raiseSalary :: (Typeable a) => a -> a
raiseSalary = mkT $ \(Salary s) -> Salary (s * 1.04)

-- Try:
--
-- > raiseSalary ()
-- > raiseSalary "hello"
-- > raiseSalary 7
-- > raiseSalary (Salary 7)
--

-- ** Using 'Typeable': 'mkQ'

-- | Function that computes over one type or returns a default value.
mkQ :: (Typeable a, Typeable b) => r -> (b -> r) -> a -> r
mkQ r f = maybe r f . mCast

-- ** Functions on multiple types: 'extQ'

-- | @extQ q f x@ applies @f@ to @x@ if type @a@ can be casted to @b@,
-- otherwise returns @q x@.
extQ :: (Typeable a, Typeable b)
    => (a -> r) -- ^ Default function to apply, if @a@ cannot be casted to @b@
    -> (b -> r) -- ^ Function to apply if @a@ __can__ be casted to @b@
    -> a -> r
extQ q f a = case mCast a of
    Nothing -> q a
    Just b  -> f b

-- What's the use of @extQ@?

-- | Show only the types we know.
myShow :: Typeable a => a -> String
myShow = mkQ "unknown type" (show :: Int -> String)
         `extQ` (show :: Bool -> String) -- Note how @extQ@ is used to extend
                                         -- the show function to another type
         `extQ` (const "A double!" :: Double -> String) -- And to another

-- >  myShow (8::Int)
-- > "8"
--
-- > myShow (8::Double)
-- > "A double!"
