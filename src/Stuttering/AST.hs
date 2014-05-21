module Stuttering.AST
( Expr(..)
, UnaryOp(..)
, BinaryOp(..)
, Stmt(..)
, Value(..)
) where

data Expr = Var String
          | IntLit Integer
          | BoolLit Bool
          | StringLit String
          | UnaryOp UnaryOp Expr
          | BinaryOp BinaryOp Expr Expr
            deriving (Show, Eq)

data UnaryOp = Negate
             | Not
               deriving (Show, Eq)

data BinaryOp = Add
              | Subtract
              | Multiply
              | Divide
              | Mod
              | And
              | Or
              | Greater
              | Less
              | Equal
              | GreaterOrEqual
              | LessOrEqual
                deriving (Show, Eq)

data Stmt = Seq [Stmt]
          | Assign String Expr
          | Print Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
            deriving (Show, Eq)

data Value = IntVal Integer
           | BoolVal Bool
           | StringVal String
