module Main where

import Stuttering.Parser
import Stuttering.JavaOutput
import Stuttering.Optimizer

main = parseFile "example.stut" >>= putStrLn . output . optimize
