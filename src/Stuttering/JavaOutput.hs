module Stuttering.JavaOutput
( output
) where

import Stuttering.Parser
import Data.List

singleIndent = "  "
indent = concat . flip replicate singleIndent

output :: Stmt -> String
output stmt =
  "public class Main {\n" ++
  indent 1 ++ "public static void main(String[] args) {\n" ++
  outputStmt 2 stmt ++
  indent 1 ++ "}\n" ++
  "}"

outputStmt :: Int -> Stmt -> String
outputStmt n (Seq stmts)       = outputStmts n stmts
outputStmt n (Assign str expr) = outputAssign n str expr
outputStmt n (Print expr)      = outputPrint n expr
outputStmt n (If cond s1 s2)   = outputIf n cond s1 s2
outputStmt n (While cond stmt) = outputWhile n cond stmt

outputStmts :: Int -> [Stmt] -> String
outputStmts n = concatMap (outputStmt n)

outputAssign :: Int -> String -> AExpr -> String
outputAssign n str expr =
  indent n ++ str ++ " = " ++ outputAExpr expr ++ ";\n"

outputPrint :: Int -> AExpr -> String
outputPrint n expr =
  indent n ++ "System.out.println(" ++ outputAExpr expr ++ ");\n"

outputIf :: Int -> BExpr -> Stmt -> Stmt -> String
outputIf n cond s1 s2 =
  indent n ++ "if(" ++ outputBExpr cond ++ ") {\n" ++
  outputStmt (n + 1) s1 ++
  indent n ++ "} else {\n" ++
  outputStmt (n + 1) s2 ++
  indent n ++ "}\n"

outputWhile :: Int -> BExpr -> Stmt -> String
outputWhile n cond stmt =
  indent n ++ "while(" ++ outputBExpr cond ++ ") {\n" ++
  outputStmt (n + 1) stmt ++
  indent n ++ "}\n"

outputAExpr :: AExpr -> String
outputAExpr (Var str)          = str
outputAExpr (IntConst int)     = show int
outputAExpr (Neg expr)         = '-' : outputAExpr expr
outputAExpr (ABinary op e1 e2) = outputABinary op e1 e2

outputABinary :: ABinOp -> AExpr -> AExpr -> String
outputABinary op e1 e2 = unwords [outputAExpr e1, outputABinOp op, outputAExpr e2]

outputABinOp :: ABinOp -> String
outputABinOp Add      = "+"
outputABinOp Subtract = "-"
outputABinOp Multiply = "*"
outputABinOp Divide   = "/"

outputBExpr :: BExpr -> String
outputBExpr (BoolConst bool)   = outputBool bool
outputBExpr (Not expr)         = "!(" ++ outputBExpr expr ++ ")"
outputBExpr (BBinary op e1 e2) = outputBBinary op e1 e2
outputBExpr (RBinary op e1 e2) = outputRBinary op e1 e2

outputBBinary :: BBinOp -> BExpr -> BExpr -> String
outputBBinary op e1 e2 = unwords [outputBExpr e1, outputBBinOp op, outputBExpr e2]

outputBBinOp :: BBinOp -> String
outputBBinOp And = "&&"
outputBBinOp Or  = "||"

outputRBinary :: RBinOp -> AExpr -> AExpr -> String
outputRBinary op e1 e2 = unwords [outputAExpr e1, outputRBinOp op, outputAExpr e2]

outputRBinOp :: RBinOp -> String
outputRBinOp Greater = ">"
outputRBinOp Less    = "<"
outputRBinOp Equal   = "=="

outputBool :: Bool -> String
outputBool True  = "true"
outputBool False = "false"
