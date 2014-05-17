module Main where

import Stuttering.Parser     (parseFile)
import Stuttering.JavaOutput (output)
import Stuttering.Optimizer  (optimize)
import System.Environment    (getArgs)

main = getArgs >>= mapM_ ((>>= putStrLn . output . optimize) . parseFile)
