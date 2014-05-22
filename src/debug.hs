module Debug where

import Stuttering.Parser      (parseFile)
import Stuttering.Optimizer   (optimize)
import System.Environment     (getArgs)

main = getArgs >>= mapM_ ((>>= print . optimize) . parseFile)
