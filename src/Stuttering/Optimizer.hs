module Stuttering.Optimizer
( optimize
) where

import Stuttering.Parser

optimize :: Stmt -> Stmt
optimize = optStmt

optStmt :: Stmt -> Stmt
optStmt (Seq stmts)       = Seq (map optStmt stmts)
optStmt (Assign id expr)  = Assign id (optAExpr expr)
optStmt (Print expr)      = Print (optAExpr expr)
optStmt (If cond s1 s2)   = If (optBExpr cond) (optStmt s1) (optStmt s2)
optStmt (While cond stmt) = While (optBExpr cond) (optStmt stmt)

optAExpr :: AExpr -> AExpr
optAExpr (Var str)          = Var str
optAExpr (IntConst int)     = IntConst int
optAExpr (Neg expr)         = Neg (optAExpr expr)
optAExpr (ABinary op e1 e2) = ABinary op (optAExpr e1) (optAExpr e2)

optBExpr :: BExpr -> BExpr
optBExpr (BoolConst bool)   = BoolConst bool
optBExpr (Not expr)         = Not (optBExpr expr)
optBExpr (BBinary op e1 e2) = BBinary op (optBExpr e1) (optBExpr e2)
optBExpr (RBinary op e1 e2) = RBinary op (optAExpr e1) (optAExpr e2)
