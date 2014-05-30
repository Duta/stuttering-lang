module Stuttering.Interpreter
( interpret
) where

import Stuttering.AST
import Control.Monad
import Data.List
import qualified Data.Map as M

interpret :: Stmt -> IO ()
interpret = void . interpretStmt M.empty

runtimeError :: String -> a
runtimeError = error . ("Runtime error: "++)

interpretStmt :: ValueMap -> Stmt -> IO ValueMap
interpretStmt m (Seq stmts)         = foldM interpretStmt m stmts
interpretStmt m (Assign ident expr) = case (evalLExpr ident, evalRExpr m expr) of
  (Right lval, Right val)              -> case getAssignedVal m lval val of
    Right val'  -> return $ M.insert (getLValIdent lval) val' m
    Left errMsg -> runtimeError errMsg
  (Left errMsg, _)                     -> runtimeError errMsg
  (_, Left errMsg)                     -> runtimeError errMsg
interpretStmt m (Print expr)        = case evalRExpr m expr of
  Right val   -> putStrLn (repr val) >> return m
  Left errMsg -> runtimeError errMsg
interpretStmt m (If cond s1 s2)     = case evalRExpr m cond of
  Right (Bool bool) -> interpretStmt m $ if bool then s1 else s2
  Right _           -> runtimeError "Non-boolean value in if condition"
  Left errMsg       -> runtimeError errMsg
interpretStmt m (While cond stmt)   = case evalRExpr m cond of
  Right (Bool bool) -> if bool
    then interpretStmt m stmt >>= flip interpretStmt (While cond stmt)
    else return m
  Right _           -> runtimeError "Non-boolean value in while condition"
  Left errMsg       -> runtimeError errMsg

getAssignedVal :: ValueMap -> LValue -> RValue -> Either String RValue
getAssignedVal _ (VarAccess _) val               = return val
getAssignedVal m lval@(StructAccess _ field) val = liftM (Struct . M.insert field val) $ getLValMap m lval

getLValIdent :: LValue -> String
getLValIdent (VarAccess ident)     = ident
getLValIdent (StructAccess lval _) = getLValIdent lval

getLValMap :: ValueMap -> LValue -> Either String ValueMap
getLValMap m (VarAccess _)             = Right m
getLValMap m (StructAccess lval field) = case lval of
  VarAccess ident      -> lookupStruct m ident field
  StructAccess _ ident -> do
    m' <- getLValMap m lval
    lookupStruct m' ident field
  where
    lookupStruct m ident field = if M.member ident m
      then case m M.! ident of
            Struct m' -> Right m'
            _         -> Left $ ident ++ " is not a struct"
      else Left $ "Attempted to use undefined struct " ++ ident

evalLExpr :: Expr -> Either String LValue
evalLExpr (Var ident)            = Right $ VarAccess ident
evalLExpr (BinaryOp Field e1 e2) = case (evalLExpr e1, evalLExpr e2) of
  (Right lval, Right (VarAccess ident)) -> Right $ StructAccess lval ident
  _                                     -> Left "Attempted to use rvalue in struct access"
evalLExpr _                      = Left "Attempted to use rvalue in lvalue context"

evalRExpr :: ValueMap -> Expr -> Either String RValue
evalRExpr m (Var ident)         = if M.member ident m
  then Right $ m M.! ident
  else Left $ "Attempted to access undefined variable " ++ ident
evalRExpr m (IntLit int)        = Right $ Int int
evalRExpr m (BoolLit bool)      = Right $ Bool bool
evalRExpr m (StringLit str)     = Right $ String str
evalRExpr m StructLit           = Right $ Struct M.empty
evalRExpr m (UnaryOp op expr)   = case evalRExpr m expr of
  Right val            -> applyUnaryOp op val
  Left errMsg          -> Left errMsg
evalRExpr m (BinaryOp Field expr (Var field)) = do
  lval <- evalLExpr expr
  m' <- getLValMap m lval
  let ident = getLValIdent lval
  struct <- evalRExpr m' (Var ident)
  case struct of
    Struct vals -> evalRExpr vals (Var field)
    _           -> fail $ ident ++ " is not a struct"
evalRExpr m (BinaryOp op e1 e2) = case (evalRExpr m e1, evalRExpr m e2) of
  (Right v1, Right v2) -> applyBinaryOp op v1 v2
  (Left errMsg, _)     -> Left errMsg
  (_, Left errMsg)     -> Left errMsg

repr :: RValue -> String
repr (Int int)     = show int
repr (Bool bool)   = if bool then "true" else "false"
repr (String str)  = str
repr (Struct vals) = ("("++) . (++")")
                   . intercalate ", "
                   . fmap (\(k,v) -> k ++ ": " ++ repr v)
                   $ M.assocs vals

applyUnaryOp :: UnaryOp -> RValue -> Either String RValue
applyUnaryOp Negate (Int val) = Right . Int $ negate val
applyUnaryOp Negate _            = Left "Non-int value in negation"

applyUnaryOp Not (Bool val)   = Right . Bool $ not val
applyUnaryOp Not _               = Left "Non-boolean value in 'not'"

applyBinaryOp :: BinaryOp -> RValue -> RValue -> Either String RValue
applyBinaryOp Add (Int int1) (Int int2)            = Right . Int $ int1 + int2
applyBinaryOp Add (String str1) (String str2)      = Right . String $ str1 ++ str2
applyBinaryOp Add (String str) (Int int)           = Right . String $ str ++ show int
applyBinaryOp Add (String str) (Bool bool)         = Right . String $ str ++ show bool
applyBinaryOp Add (Int int) (String str)           = Right . String $ show int ++ str
applyBinaryOp Add (Bool bool) (String str)         = Right . String $ show bool ++ str
applyBinaryOp Add _ _                              = Left "Non-int value(s) in addition"

applyBinaryOp Subtract (Int int1) (Int int2)       = Right . Int $ int1 - int2
applyBinaryOp Subtract _ _                         = Left "Non-int value(s) in subtraction"

applyBinaryOp Multiply (Int int1) (Int int2)       = Right . Int $ int1 * int2
applyBinaryOp Multiply _ _                         = Left "Non-int value(s) in multiplication"

applyBinaryOp Divide (Int int1) (Int int2)         = Right . Int $ int1 `div` int2
applyBinaryOp Divide _ _                           = Left "Non-int value(s) in division"

applyBinaryOp Mod (Int int1) (Int int2)            = Right . Int $ int1 `mod` int2
applyBinaryOp Mod _ _                              = Left "Non-int value(s) in 'mod'"

applyBinaryOp And v1 (Bool True)                   = Right v1
applyBinaryOp And (Bool True) v2                   = Right v2
applyBinaryOp And _ (Bool False)                   = Right $ Bool False
applyBinaryOp And (Bool False) _                   = Right $ Bool False
applyBinaryOp And _ _                              = Left "Non-boolean value(s) in 'and'"

applyBinaryOp Or _ (Bool True)                     = Right $ Bool True
applyBinaryOp Or (Bool True) _                     = Right $ Bool True
applyBinaryOp Or v1 (Bool False)                   = Right v1
applyBinaryOp Or (Bool False) v2                   = Right v2
applyBinaryOp Or _ _                               = Left "Non-boolean value(s) in 'or'"

applyBinaryOp Greater (Int int1) (Int int2)        = Right . Bool $ int1 > int2
applyBinaryOp Greater _ _                          = Left "Non-int value(s) in greater-than comparison"

applyBinaryOp Less (Int int1) (Int int2)           = Right . Bool $ int1 < int2
applyBinaryOp Less _ _                             = Left "Non-int value(s) in less-than comparison"

applyBinaryOp GreaterOrEqual (Int int1) (Int int2) = Right . Bool $ int1 >= int2
applyBinaryOp GreaterOrEqual _ _                   = Left "Non-int value(s) in greater-than-or-equal comparison"

applyBinaryOp LessOrEqual (Int int1) (Int int2)    = Right . Bool $ int1 <= int2
applyBinaryOp LessOrEqual _ _                      = Left "Non-int value(s) in less-than-or-equal comparison"

applyBinaryOp Equal (Int int1) (Int int2)          = Right . Bool $ int1 == int2
applyBinaryOp Equal (Bool bool1) (Bool bool2)      = Right . Bool $ bool1 == bool2
applyBinaryOp Equal _ _                            = Left "Incomparable values in equality comparison"

applyBinaryOp Field _ _                            = Left "Invalid values in field access"
