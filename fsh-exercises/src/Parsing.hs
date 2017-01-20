-- | Examples and exercises taken from:
--
--    http://www.scs.stanford.edu/16wi-cs240h/slides/parsing-slides.html

module Parsing where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe

-- * A dumb parser

type DumbParser s a = s -> Maybe (a,s)

string :: String -> DumbParser String String
string pattern input =
  ((,) pattern) <$> stripPrefix pattern input

number :: DumbParser String Int
number input = let res = reads input :: [(Int, String)] in
  case res of
    [x] -> Just x
    _ -> Nothing

number' :: DumbParser String Int
number' s = case reads h of
             [(n,_)] -> Just (n,t)
             _       -> Nothing
  where (h,t) = span isDigit s


version :: DumbParser String (Int, Int)
version input = do
  (_, rest0) <- string "HTTP/" input
  (major, rest1) <- number' rest0
  (_, rest2) <- string "." rest1
  (minor, rest3) <- number' rest2
  (_, rest4) <- string "\r\n" rest3
  return ((major, minor), rest4)


-- * A monadic parser

-- | To define parser as a monad instance we need to create a data-type for it:
newtype Parser s a = P { runP :: s -> Maybe (a, s) }


instance Monad (Parser s) where
  (>>=) = bind
  return = shove

bind :: Parser s a -> (a -> Parser s b) -> Parser s b
bind p0 fp1 = P $ \input -> do
  (x, input') <- (runP p0) input
  (runP (fp1 x)) input'

shove :: a -> Parser s a
shove x = P $ Just . (,) x

instance Applicative (Parser s) where
  pure = return
  (<*>) = ap
  -- If you want to implement this manually:
  --
  -- > pfa <*> pa =  pfa >>= \f -> pa >>= \x -> return $ f x
  --
  -- With `do` notation:
  --
  -- > pfa <*> pa = do
  -- >     f <- pfa
  -- >     x <- pa
  -- >     return $ f x
  --

instance Functor (Parser s) where
  fmap f pa = pa >>= \x -> return $ f x

-- | Reimplementation of `string` and `number`:
string1 :: String -> Parser String String
string1 pattern  =P $ \input ->  ((,) pattern) <$> stripPrefix pattern input

number1 :: Parser String Int
number1 = P $ \input ->
  let (h,t) = span isDigit input in
    case reads h of
      [(n,_)] -> Just (n,t)
      _       -> Nothing

-- | Second version of the parser using monads:
version1 :: Parser String (Int, Int)
version1 = do
  string1 "HTTP/"
  maj <- number1
  string1 "."
  min <- number1
  string1 "\r\n"
  return (maj, min)

-- | Third version using applicatives:
version2 = (,) <$> ((string1 "HTTP/") *> number1 <* (string1 "."))
                    <*> number1 <* (string1 "\r\n")


-- * Alternative instance for our parser.
instance Alternative (Parser s) where
  empty = P $ \_ -> Nothing
  pa <|> pb = P $ \input -> case runP pa input of
                              Nothing -> runP pb input
                              result -> result
