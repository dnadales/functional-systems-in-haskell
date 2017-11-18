{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeOperators             #-}

module GenericProgramming where

import           Control.Exception
import           Control.Monad.Identity
import           Control.Monad.State.Lazy
import           Data.Data
import           Data.Function
import           Data.Monoid
import           Data.Typeable
import           GHC.Generics
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

-- | function that computes over one type or returns a default value.
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
myShowT :: Typeable a => a -> String
myShowT = mkQ "unknown type" (show :: Int -> String)
          `extQ` (show :: Bool -> String) -- Note how @extQ@ is used to extend
                                          -- the show function to another type
          `extQ` (const "A double!" :: Double -> String) -- And to another.

-- >  myShow (8::Int)
-- > "8"
--
-- > myShow (8::Double)
-- > "A double!"

-- * The 'ExistentialQuantification' extension.
data Step s a = Done | Skip !s | Yield !a !s

data Stream a = forall s. Stream (s -> Step s a) !s

-- | Context on existential variables, like hidden dictionary fields.
data Showable = forall a . Show a => Showable a

instance Show Showable where
    show (Showable x) = "Showable " ++ show x

-- A 'Showable' value has both a value of type 'a' and a dictionary for 'Show'.

-- ** Example: Dynamic Type

data Dynamic = forall a . Typeable a => Dynamic a

toDyn :: Typeable a => a -> Dynamic
toDyn = Dynamic

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic (Dynamic b) = mCast b

-- ** Making hierarchical exceptions
data Boom = Boom deriving (Show, Typeable)
instance Exception Boom -- use default methods

f :: Int -> String
f 42 = "you got it!"
f _  = throw Boom

-- Now let's define a hierarchy of exceptions.
data AppError = forall e . Exception e => AppError e deriving (Typeable)

instance Show AppError where show (AppError e) = "App error: " ++ show e
instance Exception AppError

data DBError = DBError deriving (Show, Typeable)

instance Exception DBError where
    -- toException :: DBError -> SomeException
    toException = toException . AppError -- Here we declare that an 'DBError' is an 'AppError'.
    -- fromException :: SomeException -> Maybe DBError
    fromException se = do
        AppError e <- fromException se -- here we try to use @fromException ::
                                       -- SomeException -> Maybe (AppError e)@
        cast e  -- why not just @return e@? Because e could be any type (and
                -- you can't just do @AppError e :: AppError DBError@, because
                -- 'AppError' takes no type parameters!). Hence the 'cast' trick!

g :: IO String
g = throw DBError

safeIO :: IO String -> IO String
safeIO g = g `catch` h
    where h :: AppError -> IO String
          h _ = return "AppError"

safeIODB :: IO String -> IO String
safeIODB g = g `catch` h
    where h :: DBError -> IO String
          h _ = return "DBError"

data UserError = UserError deriving (Show, Typeable)

instance Exception UserError where
    toException = toException . AppError
    fromException = appErrorToExeption

appErrorToExeption :: (Typeable e) => SomeException -> Maybe e
appErrorToExeption se = do
    AppError e <- fromException se
    cast e

h :: IO String
h = throw UserError

-- > λ> safeIO g
-- > "AppError"
-- > λ> safeIO h
-- > "AppError"
-- > λ> safeIODB g
-- > "DBError"
-- > λ> safeIODB h
-- > *** Exception: App error: UserError

-- * The 'Data' class
raiseSalaries :: (Data x) => x -> x
raiseSalaries x = runIdentity $ gfoldl step return (raiseSalary x)
    where
      step :: Data d => Identity (d -> b) -> d -> Identity b
      step cdb d = cdb <*> pure (raiseSalaries d)

-- Wow! A generic traversal!
--
-- > λ> raiseSalaries $ Just (1, Salary 4, True, (Salary 7, ()))
-- > Just (1,Salary 4.16,True,(Salary 7.28,()))

newtype Q r a = Q { unQ :: r }

qappend :: (Monoid r) => Q r a -> Q r b -> Q r c
qappend (Q r0) (Q r1) = Q (r0 <> r1)

-- Let's sum the salaries in a structure.
sumSalaries :: (Data x) => x -> Double
sumSalaries x = getSum $ unQ $ gfoldl step (\_ -> toQ x) x
    where step tot d = tot `qappend` Q (Sum $ sumSalaries d)
          toQ = mkQ (Q $ Sum 0) $ \(Salary s) -> Q $ Sum s

-- This is great!
--
-- > λ> sumSalaries $ Just (1, Salary 4, True, (Salary 7, ()))
-- > 11.0

-- ** Can we do the tricks above at compile time?

data MyType = MyCons deriving (Show, Generic, Eq)

-- Welcome to The Matrix:
--
-- > λ> :t from MyCons
-- > from MyCons
-- >  :: D1
-- >       ('MetaData "MyType" "GenericProgramming" "main" 'False)
-- >       (C1 ('MetaCons "MyCons" 'PrefixI 'False) U1)
-- >       x
--

data MyTree a = Leaf a | Fork !(MyTree a) !(MyTree a) deriving (Show, Generic, Eq, Data)

-- Try:
--
-- > :t from (Leaf "hello")
--
-- or:
--
-- > :t from (Fork (Leaf 10) (Leaf 0))
--
-- > λ> datatypeName (from MyCons)
-- > "MyType"

-- data X = X


-- ** How can we use all this generic programming features?
class MyShow a where
    myShow :: a -> String
    default myShow :: (Generic a, MyShow1 (Rep a)) => a -> String
    myShow a = myShow1 $ from a

data T0 = T0 deriving (Generic, Show)

instance MyShow T0
instance MyShow a => MyShow (MyTree a)
instance MyShow a => MyShow [a]

instance MyShow Char where
    myShow = show

-- Try:
--
-- > λ> myShow (Leaf "hello")
-- > "Leaf : 'h' : 'e' : 'l' : 'l' : 'o' []"
--

class MyShow1 f where myShow1 :: f p -> String

instance (MyShow1 f) => MyShow1 (M1 i c f) where
    myShow1 m1 = myShow1 (unM1 m1)

instance (MyShow1 f, MyShow1 g) => MyShow1 (f :+: g) where
    myShow1 (L1 a) = myShow1 a
    myShow1 (R1 a) = myShow1 a

-- When we hit a constructor, we want to print the name.
instance {-# OVERLAPS #-} (Constructor c, MyShow1 f) => MyShow1 (C1 c f) where
    myShow1 m1 = conName m1 ++ myShow1 (unM1 m1)

-- When we have no-constructors arguments we don't show anything.
instance MyShow1 U1 where myShow1 _ = ""

-- When we have multiple constructor arguments, shown the all
instance (MyShow1 f, MyShow1 g) => MyShow1 (f :*: g) where
    myShow1 (fp :*: gp) = myShow1 fp ++ myShow1 gp

instance (MyShow c) => MyShow1 (K1 i c) where
    myShow1 k1 = ' ' : myShow (unK1 k1)

-- * Typeable and Data in Haskell

-- Examples extracted from:
--
-- > http://chrisdone.com/posts/data-typeable
--
data Foo = MkFoo deriving (Typeable, Data, Show)

-- ** Typeable class

-- *** Use case 3: Reifying from generic code to concrete

-- reify: make (something abstract) more concrete or real.

-- | Given a char, return its string representation, or unknown otherwise.
char :: Typeable a => a -> String
char = maybe "unknown" printC . cast
    where printC :: Char -> String
          printC = show

-- Examples:
--
-- > λ> char 'a'
-- > "'a'"
-- > λ> char 10
-- > "unknown"
-- > λ> char ['1']
-- > "unknown"
-- > λ> char '1'
-- > "'1'"

-- ** The Data class

-- > λ> dataTypeOf MkFoo
-- > DataType {tycon = "Foo", datarep = AlgRep [MkFoo]}
-- > λ> dataTypeOf (Leaf "hello")
-- > DataType {tycon = "MyTree", datarep = AlgRep [Leaf,Fork]}
-- > λ> dataTypeConstrs $ dataTypeOf (Leaf "hello")
-- > [Leaf,Fork]

-- *** Use-case 5: make a real value from its constructor

-- > fromConstrB :: Data a => (forall d. Data d => d) -> Constr -> a
--
-- This won't work
--
-- > fromConstrB "bye" (toConstr (Leaf "hello"))
--
-- Because @"bye" :: String@ and not @forall d. Data d => d@: the callee
-- determines what the type 'd' will be.

data Wrap a = W a deriving (Data, Show)

-- This will work:
--
-- > λ> fromConstrB (fromConstr (toConstr (1 :: Int))) (toConstr (W 0 :: Wrap Int)) :: Wrap Int
-- > W 1
--
-- But this wont!
-- > λ> fromConstrB (fromConstr (toConstr ("hello" :: String))) (toConstr (W "hello" :: Wrap String)) :: Wrap String
-- > W "*** Exception: Data.Data.fromConstr

data Bar a b = MkBar a b deriving (Data, Show)

x = evalState
     (fromConstrM
       (do i <- get -- Here we use the monad state to keep track of how many times this function has been called.
           modify (+1)
           return
             (case i of
               0 -> fromConstr (toConstr (22::Int))
               1 -> fromConstr (toConstr 'b')))
       (toConstr (MkBar (4 :: Int) 'a')))
     0 :: Bar Int Char
