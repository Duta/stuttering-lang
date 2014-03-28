module Main where

import ParseStuttering

main = do
  ast <- parseFile "example.stut"
  print ast
