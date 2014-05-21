module Stuttering.Interpreter
( interpret
) where

import Stuttering.AST
import Control.Monad
import qualified Data.Map as M

type ValueMap = M.Map String Value

interpret :: Stmt -> IO ()
interpret = void . interpretStmt M.empty

runtimeError :: String -> a
runtimeError = error . ("Runtime error: "++)

interpretStmt :: ValueMap -> Stmt -> IO ValueMap
interpretStmt m (Seq stmts)         = foldM interpretStmt m stmts
interpretStmt m (Assign ident expr) = case evalExpr m expr of
  Right val            -> return $ M.insert ident val m
  Left errMsg          -> runtimeError errMsg
interpretStmt m (Print expr)        = case evalExpr m expr of
  Right val            -> putStrLn (repr val) >> return m
  Left errMsg          -> runtimeError errMsg
interpretStmt m (If cond s1 s2)     = case evalExpr m cond of
  Right (BoolVal bool) -> interpretStmt m $ if bool then s1 else s2
  Right _              -> runtimeError "Non-boolean value in if condition"
  Left errMsg          -> runtimeError errMsg
interpretStmt m (While cond stmt)   = case evalExpr m cond of
  Right (BoolVal bool) -> if bool
    then interpretStmt m stmt >>= flip interpretStmt (While cond stmt)
    else return m
  Right _              -> runtimeError "Non-boolean value in while condition"
  Left errMsg          -> runtimeError errMsg

evalExpr :: ValueMap -> Expr -> Either String Value
evalExpr m (Var ident)         = if M.member ident m
  then Right $ m M.! ident
  else Left $ "Attempted to access undefined variable " ++ ident
evalExpr m (IntLit int)        = Right $ IntVal int
evalExpr m (BoolLit bool)      = Right $ BoolVal bool
evalExpr m (StringLit str)     = Right $ StringVal str
evalExpr m (UnaryOp op expr)   = case evalExpr m expr of
  Right val            -> applyUnaryOp op val
  Left errMsg          -> Left errMsg
evalExpr m (BinaryOp op e1 e2) = case (evalExpr m e1, evalExpr m e2) of
  (Right v1, Right v2) -> applyBinaryOp op v1 v2
  (Left errMsg, _)     -> Left errMsg
  (_, Left errMsg)     -> Left errMsg

applyUnaryOp :: UnaryOp -> Value -> Either String Value
applyUnaryOp Negate (IntVal val) = Right . IntVal $ negate val
applyUnaryOp Negate _            = Left "Non-int value in negation"
applyUnaryOp Not (BoolVal val)   = Right . BoolVal $ not val
applyUnaryOp Not _               = Left "Non-boolean value in 'not'"

applyBinaryOp :: BinaryOp -> Value -> Value -> Either String Value
applyBinaryOp Add (IntVal int1) (IntVal int2)            = Right . IntVal $ int1 + int2
applyBinaryOp Add (StringVal str1) (StringVal str2)      = Right . StringVal $ str1 ++ str2
applyBinaryOp Add (StringVal str) (IntVal int)           = Right . StringVal $ str ++ show int
applyBinaryOp Add (StringVal str) (BoolVal bool)         = Right . StringVal $ str ++ show bool
applyBinaryOp Add (IntVal int) (StringVal str)           = Right . StringVal $ show int ++ str
applyBinaryOp Add (BoolVal bool) (StringVal str)         = Right . StringVal $ show bool ++ str
applyBinaryOp Add _ _                                    = Left "Non-int value(s) in addition"
applyBinaryOp Subtract (IntVal int1) (IntVal int2)       = Right . IntVal $ int1 - int2
applyBinaryOp Subtract _ _                               = Left "Non-int value(s) in subtraction"
applyBinaryOp Multiply (IntVal int1) (IntVal int2)       = Right . IntVal $ int1 * int2
applyBinaryOp Multiply _ _                               = Left "Non-int value(s) in multiplication"
applyBinaryOp Divide (IntVal int1) (IntVal int2)         = Right . IntVal $ int1 `div` int2
applyBinaryOp Divide _ _                                 = Left "Non-int value(s) in division"
applyBinaryOp Mod (IntVal int1) (IntVal int2)            = Right . IntVal $ int1 `mod` int2
applyBinaryOp Mod _ _                                    = Left "Non-int value(s) in 'mod'"
applyBinaryOp And v1 (BoolVal True)                      = Right v1
applyBinaryOp And (BoolVal True) v2                      = Right v2
applyBinaryOp And _ (BoolVal False)                      = Right $ BoolVal False
applyBinaryOp And (BoolVal False) _                      = Right $ BoolVal False
applyBinaryOp And _ _                                    = Left "Non-boolean value(s) in 'and'"
applyBinaryOp Or _ (BoolVal True)                        = Right $ BoolVal True
applyBinaryOp Or (BoolVal True) _                        = Right $ BoolVal True
applyBinaryOp Or v1 (BoolVal False)                      = Right v1
applyBinaryOp Or (BoolVal False) v2                      = Right v2
applyBinaryOp Or _ _                                     = Left "Non-boolean value(s) in 'or'"
applyBinaryOp Greater (IntVal int1) (IntVal int2)        = Right . BoolVal $ int1 > int2
applyBinaryOp Greater _ _                                = Left "Non-int value(s) in greater-than comparison"
applyBinaryOp Less (IntVal int1) (IntVal int2)           = Right . BoolVal $ int1 < int2
applyBinaryOp Less _ _                                   = Left "Non-int value(s) in less-than comparison"
applyBinaryOp GreaterOrEqual (IntVal int1) (IntVal int2) = Right . BoolVal $ int1 >= int2
applyBinaryOp GreaterOrEqual _ _                         = Left "Non-int value(s) in greater-than-or-equal comparison"
applyBinaryOp LessOrEqual (IntVal int1) (IntVal int2)    = Right . BoolVal $ int1 <= int2
applyBinaryOp LessOrEqual _ _                            = Left "Non-int value(s) in less-than-or-equal comparison"
applyBinaryOp Equal (IntVal int1) (IntVal int2)          = Right . BoolVal $ int1 == int2
applyBinaryOp Equal (BoolVal bool1) (BoolVal bool2)      = Right . BoolVal $ bool1 == bool2
applyBinaryOp Equal _ _                                  = Left "Incomparable values in equality comparison"

repr :: Value -> String
repr (IntVal int)    = show int
repr (BoolVal bool)  = if bool then "right" else "wrong"
repr (StringVal str) = str
