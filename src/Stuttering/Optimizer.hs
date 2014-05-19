module Stuttering.Optimizer
( optimize
) where

import Stuttering.AST

optimize :: Stmt -> Stmt
optimize = cfStmt

cfStmt :: Stmt -> Stmt
cfStmt (Seq stmts)         = Seq $ map cfStmt stmts
cfStmt (Assign ident expr) = Assign ident (cfExpr expr)
cfStmt (Print expr)        = Print (cfExpr expr)
cfStmt (While cond stmt)   = While (cfExpr cond) (cfStmt stmt)
cfStmt (If cond s1 s2)     = case (cfExpr cond, cfStmt s1, cfStmt s2) of
  (BoolLit True,  optS1, _) -> optS1
  (BoolLit False, _, optS2) -> optS2
  (optCond, optS1, optS2)   -> If optCond optS1 optS2

cfExpr :: Expr -> Expr
cfExpr (Var ident)         = Var ident
cfExpr (IntLit int)        = IntLit int
cfExpr (BoolLit bool)      = BoolLit bool
cfExpr (StringLit str)     = StringLit str
cfExpr (UnaryOp op expr)   = case (op, cfExpr expr) of
  (Negate, IntLit int) -> IntLit $ -int
  (Not, BoolLit bool)  -> BoolLit $ not bool
  (_, optExpr)         -> UnaryOp op optExpr
cfExpr (BinaryOp op e1 e2) = case (op, cfExpr e1, cfExpr e2) of
  (Add, IntLit int1, IntLit int2)            -> IntLit $ int1 + int2
  (Add, StringLit str1, StringLit str2)      -> StringLit $ str1 ++ str2
  (Add, StringLit str, IntLit int)           -> StringLit $ str ++ show int
  (Add, StringLit str, BoolLit bool)         -> StringLit $ str ++ show bool
  (Add, IntLit int, StringLit str)           -> StringLit $ show int ++ str
  (Add, BoolLit bool, StringLit str)         -> StringLit $ show bool ++ str
  (Subtract, IntLit int1, IntLit int2)       -> IntLit $ int1 - int2
  (Multiply, IntLit int1, IntLit int2)       -> IntLit $ int1 * int2
  (Divide, IntLit int1, IntLit int2)         -> IntLit $ int1 `div` int2
  (And, optE1, BoolLit True)                 -> optE1
  (And, BoolLit True, optE2)                 -> optE2
  (And, optE1, BoolLit False)                -> BoolLit False
  (And, BoolLit False, optE2)                -> BoolLit False
  (Or, optE1, BoolLit True)                  -> BoolLit True
  (Or, BoolLit True, optE2)                  -> BoolLit True
  (Or, optE1, BoolLit False)                 -> optE1
  (Or, BoolLit False, optE2)                 -> optE2
  (Greater, IntLit int1, IntLit int2)        -> BoolLit $ int1 > int2
  (Less, IntLit int1, IntLit int2)           -> BoolLit $ int1 < int2
  (GreaterOrEqual, IntLit int1, IntLit int2) -> BoolLit $ int1 >= int2
  (LessOrEqual, IntLit int1, IntLit int2)    -> BoolLit $ int1 <= int2
  (Equal, IntLit int1, IntLit int2)          -> BoolLit $ int1 == int2
  (Equal, BoolLit bool1, BoolLit bool2)      -> BoolLit $ bool1 == bool2
  (_, optE1, optE2)                          -> BinaryOp op optE1 optE2
