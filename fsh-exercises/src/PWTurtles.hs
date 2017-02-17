{-# LANGUAGE OverloadedStrings #-}
-- | Playing with the Turtle library.

module PWTurtles where

import Turtle

-- Some exercises from the slides:
tuple :: Pattern (Int, Int)
tuple = do
  "("
  n <- decimal
  ","
  m <- decimal
  ")"
  return (n, m)
