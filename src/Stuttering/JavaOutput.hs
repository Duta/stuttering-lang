module Stuttering.JavaOutput
( output
) where

import Stuttering.AST
import Data.List

singleIndent = "  "
indent = concat . flip replicate singleIndent

output :: Stmt -> String
output stmt =
  "public class Main {\n" ++
  indent 1 ++ "public static void main(String[] args) {\n" ++
  showStmt 2 stmt ++
  indent 1 ++ "}\n" ++
  "}"

showStmt :: Int -> Stmt -> String
showStmt n (Seq stmts)       = concatMap (showStmt n) stmts
showStmt n (Assign str expr) = indent n ++ str ++ " = " ++ showExpr expr ++ ";\n"
showStmt n (Print expr)      = indent n ++ "System.out.println(" ++ showExpr expr ++ ");\n"
showStmt n (If cond s1 s2)   = indent n ++ "if(" ++ showExpr cond ++ ") {\n"
                            ++ showStmt (n+1) s1
                            ++ indent n ++ "} else {\n"
                            ++ showStmt (n+1) s2
                            ++ indent n ++ "}\n"
showStmt n (While cond stmt) = indent n ++ "while(" ++ showExpr cond ++ ") {\n"
                            ++ showStmt (n+1) stmt
                            ++ indent n ++ "}\n"

showExpr :: Expr -> String
showExpr (Var ident)         = ident
showExpr (IntLit int)        = show int
showExpr (BoolLit bool)      = if bool then "true" else "false"
showExpr (StringLit str)     = show str
showExpr (UnaryOp op expr)   = showUnaryOp op ++ "(" ++ showExpr expr ++ ")"
showExpr (BinaryOp op e1 e2) = "(" ++ showExpr e1 ++ ") " ++ showBinaryOp op ++ " (" ++ showExpr e2 ++ ")"

showUnaryOp :: UnaryOp -> String
showUnaryOp Negate = "-"
showUnaryOp Not    = "!"

showBinaryOp :: BinaryOp -> String
showBinaryOp Add            = "+"
showBinaryOp Subtract       = "-"
showBinaryOp Multiply       = "*"
showBinaryOp Divide         = "/"
showBinaryOp And            = "&&"
showBinaryOp Or             = "||"
showBinaryOp Greater        = ">"
showBinaryOp Less           = "<"
showBinaryOp Equal          = "=="
showBinaryOp GreaterOrEqual = ">="
showBinaryOp LessOrEqual    = "<="
