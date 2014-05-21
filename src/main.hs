module Main where

import Stuttering.Parser      (parseFile)
import Stuttering.Interpreter (interpret)
import Stuttering.Optimizer   (optimize)
import System.Environment     (getArgs)

main = getArgs >>= mapM_ ((>>= interpret . optimize) . parseFile)
