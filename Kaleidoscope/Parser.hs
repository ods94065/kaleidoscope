{-# LANGUAGE FlexibleContexts #-}

module Kaleidoscope.Parser (parse, parseFile, parseInput) where

import Control.Applicative ((<*), (*>), (<*>), (<$>))
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Data.Functor.Identity
import Text.Parsec.Combinator
import Text.Parsec.Expr
import Text.Parsec.Prim hiding (parse)
import qualified Text.Parsec as P

import Kaleidoscope.Lexer


data BinOp = Add | Sub | Mul | Div | LessThan deriving (Eq, Show)

data Expr =
  NumExpr Double
  | VarExpr S.ByteString
  | BinOpExpr BinOp Expr Expr
  | CallExpr S.ByteString [Expr]
  deriving (Eq, Show)

data Prototype = Prototype S.ByteString [S.ByteString] deriving (Eq, Show)

data Statement =
  ExternStatement Prototype
  | DefStatement Prototype Expr
  | ExprStatement Expr
  deriving (Eq, Show)

data Module = Module [Statement] deriving (Eq, Show)

type Parser = Parsec [Token] ()


nextPos :: P.SourcePos -> Token -> [Token] -> P.SourcePos
nextPos pos _ [] = pos
nextPos pos _ (token : _) =
  let tp@(Posn l c) = tPos token
  in P.setSourceColumn (P.setSourceLine pos l) c

satisfy :: (Stream [Token] Identity Token) => (Token -> Bool) -> Parsec [Token] u Tok
satisfy f = tokenPrim show nextPos maybeMatch
  where
    maybeMatch token = if f token then Just (tTok token) else Nothing

tok :: (Stream [Token] Identity Token) => Tok -> Parsec [Token] u Tok
tok t = satisfy ((== t) . tTok)

betweenParens :: Parser a -> Parser a
betweenParens = between (tok LParen) (tok RParen)

id_ :: Parser S.ByteString
id_ = parsecMap idValue $ satisfy (isId . tTok)
  where
    isId (Id _) = True
    isId _ = False

num :: Parser Double
num = parsecMap numLitValue $ satisfy (isNumLit . tTok)
  where
    isNumLit (NumLit _) = True
    isNumLit _ = False

numExpr :: Parser Expr
numExpr = NumExpr <$> num <?> "number"

functionCall :: Parser Expr
functionCall = CallExpr <$> id_ <*> args <?> "function call"
  where
    args = betweenParens $ expr `sepBy` (tok Comma)

varExpr :: Parser Expr
varExpr = VarExpr <$> id_ <?> "variable"

identifierExpr :: Parser Expr
identifierExpr = try functionCall <|> varExpr

parenExpr :: Parser Expr
parenExpr = betweenParens expr

primaryExpr :: Parser Expr
primaryExpr = numExpr <|> identifierExpr <|> parenExpr

binary op fun assoc = Infix (tok op >> return fun) assoc

opTable = [
  [binary Star (BinOpExpr Mul) AssocLeft,
   binary Slash (BinOpExpr Div) AssocLeft
  ],
  [binary Plus (BinOpExpr Add) AssocLeft,
   binary Minus (BinOpExpr Sub) AssocLeft
  ],
  [binary LAngle (BinOpExpr LessThan) AssocLeft]]

expr :: Parser Expr
expr = buildExpressionParser opTable primaryExpr <?> "expression"

prototype :: Parser Prototype
prototype = Prototype <$> id_ <*> betweenParens (id_ `sepBy` (tok Comma))

externStatement :: Parser Statement
externStatement = ExternStatement <$> (tok Extern *> prototype)
                  <?> "extern declaration"

defStatement :: Parser Statement
defStatement = DefStatement <$> (tok Def *> prototype) <*> expr
               <?> "function definition"

exprStatement :: Parser Statement
exprStatement = ExprStatement <$> expr <?> "top-level expression"

statement :: Parser Statement
statement = statement' <* skipMany (tok Semicolon) <?> "statement"
  where
    statement' = externStatement <|> defStatement <|> exprStatement

module_ :: Parser Module
module_ = module' <* eof <?> "module"
  where
    module' = Module <$> many statement

parse :: Parser a -> L.ByteString -> Either P.ParseError a
parse p s = P.parse p "<string>" (scan s)

parseFile :: FilePath -> IO (Either P.ParseError Module)
parseFile path = P.parse module_ path <$> scanFile path

parseInput :: IO (Either P.ParseError Module)
parseInput = P.parse module_ "<stdin>" <$> scanInput
