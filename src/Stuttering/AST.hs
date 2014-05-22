module Stuttering.AST
( Expr(..)
, UnaryOp(..)
, BinaryOp(..)
, Stmt(..)
, RValue(..)
, LValue(..)
, ValueMap
) where

import qualified Data.Map as M

data Expr = Var String
          | IntLit Integer
          | BoolLit Bool
          | StringLit String
          | StructLit
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
              | Field
                deriving (Show, Eq)

data Stmt = Seq [Stmt]
          | Assign Expr Expr
          | Print Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
            deriving (Show, Eq)

data RValue = VarAccess String
            | StructAccess RValue String
              deriving (Show, Eq)

data LValue = Int Integer
            | Bool Bool
            | String String
            | Struct ValueMap
              deriving (Show, Eq)

type ValueMap = M.Map String LValue
