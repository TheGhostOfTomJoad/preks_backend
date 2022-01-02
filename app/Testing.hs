{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}


module Testing where

import           BetterInterpreter
import           Control.Monad.Except
import           Parser
import           Test.QuickCheck

nicetestExample :: IO (Either [Char] [Int])
nicetestExample = do
    code <- readFile "example.prek"
    return $ parseInterpret code

-- prop_assign :: [Assign] -> Bool
-- prop_assign assign = runExcept (evalAs assign) == (runExcept (evalAs2 assign))
