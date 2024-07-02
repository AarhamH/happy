module Operators where

import Values
import Control.Monad.Except

operators :: [(String, [Values] -> ThrowsError Values)]
operators = [ ("+", numberOp (+))
            , ("-", numberOp (-))
            , ("*", numberOp (*))
            , ("/", numberOp div)
            , ("mod", numberOp mod)
            , ("quotient", numberOp quot)
            , ("remainder", numberOp rem) 
            ]

numberOp :: (Integer -> Integer -> Integer) -> [Values] -> ThrowsError Values
numberOp op [] = throwError $ ArgumentNumber 2 []
numberOp op singleVal@[_] = throwError $ ArgumentNumber 2 singleVal
numberOp op params = mapM numberUnpacker params >>= return . Number . foldl1 op

numberUnpacker :: Values -> ThrowsError Integer
numberUnpacker (Number n) = return n
numberUnpacker (String s) = let parsed = reads s in if null parsed 
                                                    then throwError $ TypeMismatch "number" $ String s
                                                    else return $ fst $ parsed !! 0
numberUnpacker (List [n]) = numberUnpacker n
numberUnpacker notNum = throwError $ TypeMismatch "number" notNum

