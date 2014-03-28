module Main where

import Stuttering.Parser
import Stuttering.JavaOutput

main = parseFile "example.stut" >>= putStrLn . output
