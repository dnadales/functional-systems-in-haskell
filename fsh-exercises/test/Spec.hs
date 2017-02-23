{-# LANGUAGE TemplateHaskell #-}
import Testing
import System.Random
import Control.Arrow (first)
import Data.Char
import Test.QuickCheck hiding ((.&.))
import Data.Word (Word16)

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

prop_decode (Big c) = (decodeUtf16 . encodeChar) c == [c]

return []
runTests = $quickCheckAll

main = runTests

-- TODO: write properties of decodeUtf16
