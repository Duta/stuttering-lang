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
optAExpr (Var str)          = Var str
optAExpr (IntConst int)     = IntConst int
optAExpr (Neg expr)         = Neg (optAExpr expr)
optAExpr expr@(ABinary _ _ _) = maybe expr IntConst $ constAExpr expr

optBExpr :: BExpr -> BExpr
optBExpr (BoolConst bool)   = BoolConst bool
optBExpr (Not expr)         = Not (optBExpr expr)
optBExpr (BBinary op e1 e2) = BBinary op (optBExpr e1) (optBExpr e2)
optBExpr (RBinary op e1 e2) = RBinary op (optAExpr e1) (optAExpr e2)

constAExpr :: AExpr -> Maybe Integer
constAExpr (Var _)                  = Nothing
constAExpr (IntConst int)           = Just int
constAExpr (Neg expr)               = (*(-1)) <$> constAExpr expr
constAExpr (ABinary Add      e1 e2) = (+) <$> constAExpr e1 <*> constAExpr e2
constAExpr (ABinary Subtract e1 e2) = (-) <$> constAExpr e1 <*> constAExpr e2
constAExpr (ABinary Multiply e1 e2) = (*) <$> constAExpr e1 <*> constAExpr e2
constAExpr (ABinary Divide   e1 e2) = (div) <$> constAExpr e1 <*> constAExpr e2


