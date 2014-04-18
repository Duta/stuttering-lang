module Stuttering.Parser
( parseString
, parseFile
, BExpr(..)
, BBinOp(..)
, RBinOp(..)
, AExpr(..)
, ABinOp(..)
, Stmt(..)
) where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
             deriving (Show, Eq)

data BBinOp = And
            | Or
              deriving (Show, Eq)

data RBinOp = Greater
            | Less
            | Equal
            | GreaterOrEqual
            | LessOrEqual
              deriving (Show, Eq)

data AExpr = Var String
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
             deriving (Show, Eq)

data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
              deriving (Show, Eq)

data Stmt = Seq [Stmt]
          | Assign String AExpr
          | Print AExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
            deriving (Show, Eq)

languageDef = emptyDef
  { Token.commentStart = "umm,"
  , Token.commentEnd = "yeah..."
  , Token.commentLine = "um,"
  , Token.identStart = letter
  , Token.identLetter = alphaNum
  , Token.reservedNames =
    [ "basically"
    , "is"
    , "like"
    , "so"
    , "yeah"
    , "right"
    , "wrong"
    , "print"
    , "if"
    , "then"
    , "otherwise"
    ]
  , Token.reservedOpNames =
    [ "plus"
    , "minus"
    , "times"
    , "over"
    , "is"
    , "like"
    , "negative"
    , "not"
    , "and"
    , "or"
    , "smaller"
    , "bigger"
    , "than"
    , "equals"
    ]
  }

lexer = Token.makeTokenParser languageDef

-- Parses an identifier
identifier   = Token.identifier lexer
-- Parses a reserved name
reserved     = Token.reserved lexer
-- Parses an operator
reservedOp   = Token.reservedOp lexer
-- Parses surrounding parentheses:
--   parens p
-- Takes care of the parentheses and
-- uses p to parse what's inside them
parens       = Token.parens lexer
-- Parses an integer
integer      = Token.integer lexer
-- Parses a separator (a question mark)
sep          = Token.symbol lexer "?"
-- Parses whitespace
whiteSpace   = Token.whiteSpace lexer
-- Parses a scope block
block parser = do
  reserved "basically"
  parsed <- parser
  reserved "so"
  reserved "yeah"
  return parsed

stutteringParser :: Parser Stmt
stutteringParser = whiteSpace >> statement

statement :: Parser Stmt
statement = parens statement
        <|> sequenceOfStmt
        <|> statement'

sequenceOfStmt :: Parser Stmt
sequenceOfStmt = do
  list <- block $ sepBy1 statement' sep
  return $ Seq list

statement' :: Parser Stmt
statement' = ifStmt
         <|> whileStmt
         <|> assignStmt
         <|> printStmt

ifStmt :: Parser Stmt
ifStmt = do
  reserved "if"
  cond <- bExpression
  reserved "then"
  stmt1 <- statement
  reserved "otherwise"
  stmt2 <- statement
  return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt = do
  reserved "while"
  cond <- bExpression
  stmt <- statement
  return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt = do
  var <- identifier
  reservedOp "is"
  reservedOp "like"
  expr <- aExpression
  return $ Assign var expr

printStmt :: Parser Stmt
printStmt = do
  reserved "print"
  expr <- aExpression
  return $ Print expr

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators :: OperatorTable Char st AExpr
aOperators =
  [ [Prefix (reservedOp "negative" >> return Neg)]
  , [Infix  (reservedOp "times"    >> return (ABinary Multiply)) AssocLeft]
  , [Infix  (reservedOp "over"     >> return (ABinary Divide)) AssocLeft]
  , [Infix  (reservedOp "plus"     >> return (ABinary Add)) AssocLeft]
  , [Infix  (reservedOp "minus"    >> return (ABinary Subtract)) AssocLeft]
  ]

bOperators :: OperatorTable Char st BExpr
bOperators =
  [ [Prefix (reservedOp "not" >> return Not)]
  , [Infix  (reservedOp "and" >> return (BBinary And)) AssocLeft]
  , [Infix  (reservedOp "or"  >> return (BBinary Or)) AssocLeft]
  ]

aTerm :: Parser AExpr
aTerm = parens aExpression
    <|> liftM Var identifier
    <|> liftM IntConst integer

bTerm :: Parser BExpr
bTerm = parens bExpression
    <|> (reserved "right" >> return (BoolConst True))
    <|> (reserved "wrong" >> return (BoolConst False))
    <|> rExpression

rExpression :: Parser BExpr
rExpression = do
  a1 <- aExpression
  op <- relation
  a2 <- aExpression
  return $ RBinary op a1 a2

relation :: Parser RBinOp
relation = greaterThanRelation
       <|> lessThanRelation
       <|> equalRelation

lessThanRelation :: Parser RBinOp
lessThanRelation = do
  reservedOp "is"
  reservedOp "smaller"
  reservedOp "than"
  return Less

greaterThanRelation :: Parser RBinOp
greaterThanRelation = do
  reservedOp "is"
  reservedOp "bigger"
  reservedOp "than"
  return Greater

equalRelation :: Parser RBinOp
equalRelation = do
  reservedOp "equals"
  return Equal

parseString :: String -> Stmt
parseString str =
  case parse stutteringParser "" str of
    Left e -> error $ show e
    Right r -> r

parseFile :: String -> IO Stmt
parseFile file = do
  program <- readFile file
  case parse stutteringParser "" program of
   Left e -> print e >> fail "parse error"
   Right r -> return r