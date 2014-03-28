module Main where

import Stuttering.Parser

main = do
  ast <- parseFile "example.stut"
  print ast
