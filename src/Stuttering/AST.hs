module Stuttering.AST
( Expr(..)
, UnaryOp(..)
, BinaryOp(..)
, Stmt(..)
, Type(..)
) where

data Expr = Var String
          | IntConst Integer
          | BoolConst Bool
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

data Type = Int
          | Bool
            deriving (Show, Eq)
