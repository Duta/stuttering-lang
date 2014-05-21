module Main where

import Stuttering.Parser      (parseFile)
-- import Stuttering.JavaOutput (output)
import Stuttering.Interpreter (interpret)
import Stuttering.Optimizer   (optimize)
import System.Environment     (getArgs)

-- main = getArgs >>= mapM_ ((>>= putStrLn . output . optimize) . parseFile)
main = getArgs >>= mapM_ ((>>= interpret . optimize) . parseFile)
