#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle
                                    -- #!/bin/bash
{-# LANGUAGE OverloadedStrings #-}  --
                                    --
import Turtle

say = echo

main = say "Hello, world!"         
