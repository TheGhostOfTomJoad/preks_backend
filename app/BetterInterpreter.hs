--{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module BetterInterpreter where

import           CLParser
import           Control.Monad.Except           ( Except
                                                , MonadError(..)
                                                , runExcept
                                                )
import           Converter
import qualified Data.HashMap.Strict           as M
import           Data.Type.Equality
import           Lexer
import           Parser
import           ParserCon
import           Vector


type PrekMap = M.HashMap String (Prek, ACPrek)


extendError :: MonadError [Char] m => [Char] -> m a -> m a
extendError ident m = catchError
  m
  (\x -> throwError $ "Error in  the definition of " ++ ident ++ ": " ++ x)


insertPrekHelper :: MonadError [Char] m => PrekOrId -> PrekMap -> m Prek
insertPrekHelper (Pid s) store = case M.lookup s store of
  Nothing       -> throwError $ s ++ " is not definied"
  Just (def, _) -> return def
insertPrekHelper (Pr PScucc        ) store = return Succ
insertPrekHelper (Pr (PConstZero i)) store = return $ ConstZero i
insertPrekHelper (Pr (PProj i1 i2 )) store = return (Proj i1 i2)
insertPrekHelper (Pr (PPrekC g h)) store =
  PrekC <$> insertPrekHelper g store <*> insertPrekHelper h store
insertPrekHelper (Pr (PComp g h)) store =
  Comp <$> insertPrekHelper g store <*> mapM (`insertPrekHelper` store) h

insertPrek :: MonadError [Char] m => ParsedPrek -> PrekMap -> m Prek
insertPrek p = insertPrekHelper $ Pr p


-- evalAs :: MonadError [Char] m => [Assign] -> m PrekMap
-- evalAs []                          = return M.empty
-- evalAs ((Assign ident pPrek) : xs) = do
--   prekMap          <- evalAs xs
--   newPrekWithoutId <- extendError ident (insertPrek pPrek prekMap)
--   newCheckedPrek   <- extendError ident (convert newPrekWithoutId)
--   return (M.insert ident (newPrekWithoutId, newCheckedPrek) prekMap)

-- this does not work

evalAs :: MonadError [Char] m => [Assign] -> m PrekMap
evalAs = foldM evalAsHelper M.empty

evalAsHelper
  :: MonadError [Char] m
  => PrekMap
  -> Assign
  -> m (M.HashMap String (Prek, ACPrek))
evalAsHelper prekMap (Assign ident pPrek) = do
  newPrekWithoutId <- extendError ident (insertPrek pPrek prekMap)
  newCheckedPrek   <- extendError ident (convert newPrekWithoutId)
  return (M.insert ident (newPrekWithoutId, newCheckedPrek) prekMap)


evalParsedProg :: ParsedProg -> Except String [Int]
evalParsedProg (ParsedProg assign fCalls) = do
  preks <- evalAs assign
  mapM (`evalFcall` preks) fCalls

evalFcall :: ParsedFCall -> PrekMap -> Except String Int
evalFcall (ParsedFCall (Pid s) arg) preks = case M.lookup s preks of
  Nothing -> throwError $ s ++ " is not defined but used in a Function Call."
  Just (_, pr) -> evalSafe2 pr (fromList arg)
evalFcall (ParsedFCall (Pr s) arg) prekStore = do
  pr <- extendError "s" (insertPrek s prekStore)
  extendError "s" $ evalSafe3 pr arg

paraNat
  :: (Vec n Int -> Int)
  -> (Vec (Suc (Suc n)) Int -> Int)
  -> Vec (Suc n) Int
  -> Int
paraNat g h (0 `VCons` xs) = g xs
paraNat g h (x `VCons` xs) =
  let smaller = ((x - 1) `VCons` xs) in h $ paraNat g h smaller `VCons` smaller

evalSafe :: CheckedPrek n -> Vec n Int -> Int
evalSafe CConstZero      natList        = 0
evalSafe CSucc           (VCons n VNil) = n + 1
evalSafe (CProj n      ) natList        = natList ! n
evalSafe (CComp h gList) natList = evalSafe h $ fmap (`evalSafe` natList) gList
evalSafe (CPrekC g h) natList =
  -- Wikipedia Order of Arguments
  paraNat (evalSafe g) (evalSafe h) natList

evalSafe2 :: MonadError [Char] f => ACPrek -> AVec Int -> f Int
evalSafe2 (ACPrek n f) (AVec m vec) = fmap
  (\proof -> gcastWith proof (evalSafe f vec))
  (testEquality'
    n
    m
    "Mismatching arity of a function and an argument in a function call"
  )

evalSafe3 :: MonadError [Char] m => Prek -> [Int] -> m Int
evalSafe3 f args = do
  f2 <- convert f
  evalSafe2 f2 (fromList args)

parseInterpret :: String -> Either [Char] [Int]
parseInterpret str = case lexer str of
  Nothing     -> throwError "Lexer Error"
  Just tokens -> case parse pProgramm tokens of
    Nothing -> throwError "Parser Error"
    Just (ParsedProg assignments calls) ->
      runExcept $ evalParsedProg $ ParsedProg assignments calls

cLProg :: IO ()
cLProg = do
  fn   <- parseCommandLine
  code <- readFile (filename fn ++ ".prek")
  case parseInterpret code of
    Left  err -> putStr err
    Right res -> print res
