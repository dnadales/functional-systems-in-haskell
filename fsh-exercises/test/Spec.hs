{-# LANGUAGE TemplateHaskell #-}
import Testing
import System.Random
import Control.Arrow (first)
import Data.Char
import Test.QuickCheck hiding ((.&.))
import Data.Word (Word16)
import Control.Monad

-- | To access these functions in the REPL via stack use:
--
-- > stack ghci fsh-exercises:fsh-exercises-test
--
-- To reload use:
--
-- > :r
--
-- from the REPL.

prop_encodeOne :: Char -> Bool
prop_encodeOne c = length (encodeChar c) == 1

newtype BigChar = Big Char
  deriving (Eq, Show)

-- How to make big char an instance of random?

-- | Rememeber that the instance above is for illustration purposes only. Use
-- GeneralizedNewType deriving for this.
instance Random BigChar where
  -- | Remember: random :: RandomGen g => g -> (BigChar, g)
  -- first :: a b c -> a (b, d) (c, d)
  -- My head blew here:
  random = first Big `fmap` random
  -- | Remember: randomR :: RandomGen g => (a, a) -> g -> (a, g)
  randomR (Big a, Big b) = first Big `fmap` randomR (a,b)

instance Arbitrary BigChar where
  -- | Remember: arbitrary :: Gen BigChar
  arbitrary = choose (Big '\0', Big '\x10FFFF')
  shrink (Big c) = map Big (shrinkChar c)

shrinkChar :: Char -> [Char]
shrinkChar c = [chr (round (toRational (ord c) * i)) | i <- [0, 0.25, 0.5, 0.75, 0.875]]
  
prop_encodeOne3 (Big c) = length (encodeChar c) == 1

-- | Properties of decodeUtf16
prop_decode (Big c) = (decodeUtf16 . encodeChar) c == [c]

-- * Random points!
data Point a = Point a a
  deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Point a) where
  arbitrary = liftM2 Point arbitrary arbitrary
  shrink (Point x y) = map (uncurry Point) $ zip (shrink x) (shrink y)

-- * Random trees!
data Tree a = Node (Tree a) (Tree a)
            | Leaf a
              deriving (Show)

instance (Arbitrary a) => Arbitrary (Tree a) where
  -- | If you try this:
  --  
  -- > arbitrary = oneof [ liftM Leaf arbitrary
  -- >                   , liftM2 Node arbitrary arbitrary]
  --
  -- And then run:
  -- > sample (arbitrary :: Gen (Tree Int))
  --
  -- You might get potentially infinite trees.
  --
  -- So sized to the rescue!
  --
  -- > sized  :: (Int -> Gen a) -> Gen a
  arbitrary = sized arbTree

arbTree :: (Arbitrary a) => Int -> Gen (Tree a)
arbTree 0 = liftM Leaf arbitrary
arbTree n = oneof [liftM Leaf arbitrary, liftM2 Node arbSubTree arbSubTree]
  where arbSubTree = arbTree (n `div` 2)


-- Then you can use 'resize' to resize the trees:
--
-- > resize ::  Int -> Gen a  -> Gen a
-- > sample (resize 1 $ arbitrary :: Gen (Tree Int))

return []
runTests = $quickCheckAll

main = runTests

