module Stuttering.Optimizer
( optimize
) where

import Stuttering.Parser
import Control.Applicative

optimize :: Stmt -> Stmt
optimize = optStmt

optStmt :: Stmt -> Stmt
optStmt (Seq stmts)       = Seq (map optStmt stmts)
optStmt (Assign id expr)  = Assign id (optAExpr expr)
optStmt (Print expr)      = Print (optAExpr expr)
optStmt (If cond s1 s2)   = If (optBExpr cond) (optStmt s1) (optStmt s2)
optStmt (While cond stmt) = While (optBExpr cond) (optStmt stmt)

optAExpr :: AExpr -> AExpr
optAExpr (Var str) = Var str
optAExpr (IntConst int) = IntConst int
optAExpr (Neg expr) = case optAExpr expr of
  IntConst int -> IntConst $ -int
  optExpr -> Neg optExpr
optAExpr (ABinary op e1 e2) = case (optAExpr e1, optAExpr e2) of
  (IntConst i1, IntConst i2) -> IntConst $ case op of
    Add      -> i1 + i2
    Subtract -> i1 - i2
    Multiply -> i1 * i2
    Divide   -> i1 `div` i2
  (oe1, oe2) -> ABinary op oe1 oe2

optBExpr :: BExpr -> BExpr
optBExpr (BoolConst bool)   = BoolConst bool
optBExpr (Not expr)         = Not (optBExpr expr)
optBExpr (BBinary And e1 e2) = case (optBExpr e1, optBExpr e2) of
  (BoolConst True, x) -> x
  (x, BoolConst True) -> x
  (BoolConst False, _) -> BoolConst False
  (_, BoolConst False) -> BoolConst False
  (oe1, oe2) -> BBinary And oe1 oe2
optBExpr (BBinary Or e1 e2) = case (optBExpr e1, optBExpr e2) of
  (BoolConst False, x) -> x
  (x, BoolConst False) -> x
  (BoolConst True, _) -> BoolConst True
  (_, BoolConst True) -> BoolConst True
  (oe1, oe2) -> BBinary And oe1 oe2
optBExpr (RBinary op e1 e2) = case (optAExpr e1, optAExpr e2) of
  (IntConst i1, IntConst i2) -> BoolConst $ case op of
    Greater        -> i1 >  i2
    Less           -> i1 <  i2
    Equal          -> i1 == i2
    GreaterOrEqual -> i1 >= i2
    LessOrEqual    -> i1 <= i2
  (oe1, oe2) -> RBinary op oe1 oe2
