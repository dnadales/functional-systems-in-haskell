-- | Exercises for the slides here:
--
--    http://www.scs.stanford.edu/16wi-cs240h/slides/basics-slides.html

module Basics where

data Move = Rock | Paper | Scissors
         deriving (Eq, Read, Show, Enum, Bounded)

parseMove :: String -> Maybe Move
parseMove "Rock"     = Just Rock
parseMove "Paper"    = Just Paper
parseMove "Scissors" = Just Scissors
parseMove _          = Nothing

-- | But you can also use `reads`.
parseMove' :: String -> Maybe Move
parseMove' m =
  case reads m of
    [(mv, "")] -> Just mv
    _          -> Nothing

-- | Note that the definitions of @parseMove@ and @parseMove'@ are not
-- equivalent. The latter will accept trailing whitespaces. Look at this other
-- definition instead.
parseMove'' :: String -> Maybe Move
parseMove'' str | [(m, rest)] <- reads str, ok rest = Just m
                 | otherwise = Nothing
  where ok = all (`elem` " \r\n")

-- * Strict evaluation.
factorial n0 = loop 1 n0
  where loop acc 0 = acc
        loop acc m = loop (acc * m) (m - 1)

-- Can we define a strict version of factorial using @$!@?
factorialStrict n0 = loop 1 n0
  where loop acc 0 = acc
        loop acc m = loop ((acc *) $! m) (m - 1)

-- | Is the version given by the instructor better?
--
-- Indeed it is. Because in my version I'm evaluating @m@ to weak head normal
-- form, which does not do anything, since it is a number already. In case of
-- the version below, we are reducing @acc * m@ to WHNF, which prevents the
-- formation of thunks!
factorialStrict' n0 = loop 1 n0
    where loop acc n | n > 1     = (loop $! acc * n) (n - 1)
                     | otherwise = acc

