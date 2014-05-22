module Stuttering.Parser
( parseString
, parseFile
) where

import Stuttering.AST
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef :: LanguageDef st
languageDef = emptyDef
  { Token.commentStart    = "umm,"
  , Token.commentEnd      = "yeah..."
  , Token.commentLine     = "um,"
  , Token.identStart      = letter <|> char '_'
  , Token.identLetter     = alphaNum <|> char '_'
  , Token.reservedNames   =
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
    , "a"
    , "struct"
    ]
  , Token.reservedOpNames =
    [ "plus"
    , "minus"
    , "times"
    , "over"
    , "mod"
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
    , "'s"
    ]
  }

lexer :: Token.TokenParser st
lexer = Token.makeTokenParser languageDef

-- Parses an identifier
identifier = Token.identifier lexer
-- Parses a reserved name
reserved = Token.reserved lexer
-- Parses an operator
reservedOp = Token.reservedOp lexer
-- Parses a multi-word operator
reservedOps (x:[]) = reservedOp x
reservedOps (h:t)  = reservedOp h >> reservedOps t
-- Parses surrounding parentheses:
--   parens p
-- Takes care of the parentheses and
-- uses p to parse what's inside them
parens = Token.parens lexer
-- Parses an integer
integer = Token.integer lexer
-- Parses a string literal
stringLiteral = Token.stringLiteral lexer
-- Parses a separator (a question mark)
sep = Token.symbol lexer "?"
-- Parses whitespace
whiteSpace = Token.whiteSpace lexer
-- Parses a scope block
block parser = do
  reserved "basically"
  parsed <- parser
  reserved "so"
  reserved "yeah"
  return parsed

stutteringParser :: Parser Stmt
stutteringParser = do
  whiteSpace
  stmt <- statement
  eof
  return stmt

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
  cond <- expression
  reserved "then"
  stmt1 <- statement
  reserved "otherwise"
  stmt2 <- statement
  return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt = do
  reserved "while"
  cond <- expression
  stmt <- statement
  return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt = do
  var <- expression
  reservedOp "is"
  reservedOp "like"
  expr <- expression
  return $ Assign var expr

printStmt :: Parser Stmt
printStmt = do
  reserved "print"
  expr <- expression
  return $ Print expr

expression :: Parser Expr
expression = buildExpressionParser operators terminals

operators :: OperatorTable Char st Expr
operators =
  [ [Infix (reservedOp "'s"     >> return (BinaryOp Field)) AssocLeft]
  , [Prefix (reservedOp "negative" >> return (UnaryOp Negate))]
  , [Prefix (reservedOp "not"      >> return (UnaryOp Not))]
  , [Infix (reservedOp "times"  >> return (BinaryOp Multiply)) AssocLeft]
  , [Infix (reservedOp "over"   >> return (BinaryOp Divide))   AssocLeft]
  , [Infix (reservedOp "mod"    >> return (BinaryOp Mod))      AssocLeft]
  , [Infix (reservedOp "plus"   >> return (BinaryOp Add))      AssocLeft]
  , [Infix (reservedOp "minus"  >> return (BinaryOp Subtract)) AssocLeft]
  , [Infix (reservedOp "equals" >> return (BinaryOp Equal))    AssocLeft]
  , [Infix (try (reservedOps ["is", "bigger",  "than"]) >> return (BinaryOp Greater)) AssocLeft]
  , [Infix (try (reservedOps ["is", "smaller", "than"]) >> return (BinaryOp Less))    AssocLeft]
  , [Infix (reservedOp "and"    >> return (BinaryOp And))      AssocLeft]
  , [Infix (reservedOp "or"     >> return (BinaryOp Or))       AssocLeft]
  ]

terminals :: Parser Expr
terminals = parens expression
        <|> liftM Var identifier
        <|> liftM IntLit integer
        <|> liftM StringLit stringLiteral
        <|> (reserved "right" >> return (BoolLit True))
        <|> (reserved "wrong" >> return (BoolLit False))
        <|> (reserved "a" >> reserved "struct" >> return StructLit)

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
