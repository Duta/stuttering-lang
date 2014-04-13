module Stuttering.Optimizer
( optimize
) where

import Stuttering.Parser

optimize :: Stmt -> Stmt
optimize (Seq stmts)       = Seq stmts
optimize (Assign id expr)  = Assign id expr
optimize (Print expr)      = Print expr
optimize (If cond s1 s2)   = If cond s1 s2
optimize (While cond stmt) = While cond stmt
