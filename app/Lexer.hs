module Lexer
  ( lexer
  , Token(..)
  ) where

import           Data.Char
--import Data.List ()
import           ParserCon

data Token = Pi | S | P | C | Z | TId String | TInt Int | TLet | TAssign | LBr | RBr | Sep
  deriving (Eq, Show)

string :: Eq a => [a] -> Parser a [a]
string = foldr (liftA2 (:) . lit) (pure [])

-- tNum :: Parser Char Token
-- tNum = TInt . read <$> pmany1 (satisfy isDigit)

tAlnum :: Parser Char Token
tAlnum = fmap TId $ (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)

skipSpace :: Parser Char String
skipSpace = many (satisfy isSpace)

skipComment :: Parser Char Char
skipComment =
  const 'C' <$ lit '-' <* lit '-' <* pmany (satisfy (/= '\n')) <*> lit '\n'

debuglexer :: String -> [([Token], String)]
debuglexer = parseAll $ pmany1 (skipCommentOrSpace *> pTok) <* skipSpace

skipCommentOrSpace :: Parser Char String
skipCommentOrSpace =
  "Comments" <$ pmany (skipComment <|> (' ' <$ pmany1 (satisfy isSpace)))

skipCommentOrSpace2 = pmany1 (satisfy isSpace <|> skipComment)


-- pmany1?

pTok :: Parser Char Token
pTok =
  Pi
    <$  string "Pi"
    <|> S
    <$  lit 'S'
    <|> P
    <$  lit 'P'
    <|> Z
    <$  lit 'Z'
    <|> C
    <$  lit 'C'
    <|> Sep
    <$  lit ','
    <|> RBr
    <$  lit ')'
    <|> LBr
    <$  lit '('
    <|> TLet
    <$  string "let"
    <|> TAssign
    <$  lit '='
    <|> TInt
    .   read
    <$> pmany1 (satisfy isDigit)
    <|> tAlnum

lexer :: String -> Maybe [Token]
lexer = parse $ pmany1 (skipCommentOrSpace *> pTok) <* skipSpace

