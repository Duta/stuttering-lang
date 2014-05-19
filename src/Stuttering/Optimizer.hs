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
  (BoolConst True,  optS1, _) -> optS1
  (BoolConst False, _, optS2) -> optS2
  (optCond, optS1, optS2)     -> If optCond optS1 optS2

cfExpr :: Expr -> Expr
cfExpr (Var ident) = Var ident
cfExpr (IntConst int) = IntConst int
cfExpr (BoolConst bool) = BoolConst bool
cfExpr (UnaryOp op expr) = case (op, cfExpr expr) of
  (Negate, IntConst int) -> IntConst $ -int
  (Not, BoolConst bool)  -> BoolConst $ not bool
  (_, optExpr)           -> UnaryOp op optExpr
cfExpr (BinaryOp op e1 e2) = case (op, cfExpr e1, cfExpr e2) of
  (Add, IntConst int1, IntConst int2)            -> IntConst $ int1 + int2
  (Subtract, IntConst int1, IntConst int2)       -> IntConst $ int1 - int2
  (Multiply, IntConst int1, IntConst int2)       -> IntConst $ int1 * int2
  (Divide, IntConst int1, IntConst int2)         -> IntConst $ int1 `div` int2
  (And, optE1, BoolConst True)                   -> optE1
  (And, BoolConst True, optE2)                   -> optE2
  (And, optE1, BoolConst False)                  -> BoolConst False
  (And, BoolConst False, optE2)                  -> BoolConst False
  (Or, optE1, BoolConst True)                    -> BoolConst True
  (Or, BoolConst True, optE2)                    -> BoolConst True
  (Or, optE1, BoolConst False)                   -> optE1
  (Or, BoolConst False, optE2)                   -> optE2
  (Greater, IntConst int1, IntConst int2)        -> BoolConst $ int1 > int2
  (Less, IntConst int1, IntConst int2)           -> BoolConst $ int1 < int2
  (GreaterOrEqual, IntConst int1, IntConst int2) -> BoolConst $ int1 >= int2
  (LessOrEqual, IntConst int1, IntConst int2)    -> BoolConst $ int1 <= int2
  (Equal, IntConst int1, IntConst int2)          -> BoolConst $ int1 == int2
  (Equal, BoolConst bool1, BoolConst bool2)      -> BoolConst $ bool1 == bool2
  (_, optE1, optE2)                              -> BinaryOp op optE1 optE2
