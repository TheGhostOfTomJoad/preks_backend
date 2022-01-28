module Parser where

import           Lexer
import           ParserCon
import           Vector
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts, GADTs, DataKinds, KindSignatures #-}



type Ident = String

data PrekOrId = Pr ParsedPrek | Pid Ident
    deriving (Show ,Eq)

data ParsedPrek
  = PScucc
  | PConstZero Int
  | PProj Int Int
  | PComp  PrekOrId [PrekOrId]
  | PPrekC PrekOrId PrekOrId
  deriving (Show ,Eq)



pInBr :: Parser Token a -> Parser Token a
pInBr p = lit LBr *> p <* lit RBr

parsePrek :: Parser Token ParsedPrek
parsePrek =
    PScucc
        <$  lit S
        <|> PConstZero
        <$  lit Z
        <*> pInt
        <|> PProj
        <$  lit Pi
        <*> pInt
        <*> pInt
        <|> PComp
        <$  lit C
        <*  lit LBr
        <*> parsePrekOrId
        <*  lit Sep
        <*> pPrekOrIdList
        <*  lit RBr
        <|> PPrekC
        <$  lit P
        <*  lit LBr
        <*> parsePrekOrId
        <*  lit Sep
        <*> parsePrekOrId
        <*  lit RBr


parsePrekOrId :: Parser Token PrekOrId
parsePrekOrId = Pr <$> parsePrek <|> Pid <$> pIdent

mIntToken :: Token -> Maybe Int
mIntToken (TInt n) = Just n
mIntToken _        = Nothing

mIdentToken :: Token -> Maybe String
mIdentToken (TId i) = Just i
mIdentToken _       = Nothing


pIdent :: Parser Token String
pIdent = try mIdentToken

pInt :: Parser Token Int
pInt = try mIntToken


pAList :: Parser Token r -> Parser Token [r]
pAList p = lit LBr *> pIntersperse p (lit Sep) <* lit RBr

pIntList :: Parser Token [Int]
pIntList = pAList pInt

pPrekOrIdList :: Parser Token [PrekOrId]
pPrekOrIdList = pAList parsePrekOrId





data Assign = Assign Ident ParsedPrek
    deriving (Show, Eq)

data ParsedFCall = ParsedFCall PrekOrId [Int]
    deriving (Show, Eq)

data ParsedProg = ParsedProg [Assign] [ParsedFCall]
    deriving (Show, Eq)

pAssign :: Parser Token Assign
pAssign = Assign <$ lit TLet <*> pIdent <* lit TAssign <*> parsePrek

pFcall :: Parser Token ParsedFCall
pFcall = ParsedFCall <$> parsePrekOrId <*> pIntList

pProgramm :: Parser Token ParsedProg
pProgramm = ParsedProg <$> many pAssign <*> many pFcall


