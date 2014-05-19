module Stuttering.Optimizer
( optimize
) where

import Stuttering.Parser

optimize :: Stmt -> Stmt
optimize = cfStmt

-- Perform constant folding on a statement
cfStmt :: Stmt -> Stmt
cfStmt (Seq stmts)       = Seq (map cfStmt stmts)
cfStmt (Assign id expr)  = Assign id (cfAExpr expr)
cfStmt (Print expr)      = Print (cfAExpr expr)
cfStmt (If cond s1 s2)   = case (cfBExpr cond, cfStmt s1, cfStmt s2) of
  (BoolConst True,  os1, _) -> os1
  (BoolConst False, _, os2) -> os2
  (cfCond, os1, os2)       -> If cfCond os1 os2
cfStmt (While cond stmt) = While (cfBExpr cond) (cfStmt stmt)

-- Perform constant folding on a AExpr
cfAExpr :: AExpr -> AExpr
cfAExpr (Var str)          = Var str
cfAExpr (IntConst int)     = IntConst int
cfAExpr (Neg expr)         = case cfAExpr expr of
  IntConst int -> IntConst $ -int
  cfExpr      -> Neg cfExpr
cfAExpr (ABinary op e1 e2) = case (cfAExpr e1, cfAExpr e2) of
  (IntConst i1, IntConst i2) -> IntConst $ case op of
    Add      -> i1 + i2
    Subtract -> i1 - i2
    Multiply -> i1 * i2
    Divide   -> i1 `div` i2
  (oe1, oe2)                 -> ABinary op oe1 oe2

-- Perform constant folding on a BExpr
cfBExpr :: BExpr -> BExpr
cfBExpr (BoolConst bool)    = BoolConst bool
cfBExpr (Not expr)          = Not (cfBExpr expr)
cfBExpr (BBinary And e1 e2) = case (cfBExpr e1, cfBExpr e2) of
  (BoolConst True, x)  -> x
  (x, BoolConst True)  -> x
  (BoolConst False, _) -> BoolConst False
  (_, BoolConst False) -> BoolConst False
  (oe1, oe2)           -> BBinary And oe1 oe2
cfBExpr (BBinary Or e1 e2)  = case (cfBExpr e1, cfBExpr e2) of
  (BoolConst False, x) -> x
  (x, BoolConst False) -> x
  (BoolConst True, _)  -> BoolConst True
  (_, BoolConst True)  -> BoolConst True
  (oe1, oe2)           -> BBinary And oe1 oe2
cfBExpr (RBinary op e1 e2)  = case (cfAExpr e1, cfAExpr e2) of
  (IntConst i1, IntConst i2) -> BoolConst $ case op of
    Greater        -> i1 >  i2
    Less           -> i1 <  i2
    Equal          -> i1 == i2
    GreaterOrEqual -> i1 >= i2
    LessOrEqual    -> i1 <= i2
  (oe1, oe2)                 -> RBinary op oe1 oe2
