module Operators where

import Values
import Control.Monad.Except
import qualified Data.Functor

operators :: [(String, [Values] -> ThrowsError Values)]
operators = [ ("+", numberOp (+))
            , ("-", numberOp (-))
            , ("*", numberOp (*))
            , ("/", numberOp div)
            , ("mod", numberOp mod)
            , ("quotient", numberOp quot)
            , ("remainder", numberOp rem)
            , ("=", boolOp (==))
  ]

numberOp :: (Integer -> Integer -> Integer) -> [Values] -> ThrowsError Values
numberOp _ [] = throwError $ ArgumentNumber 2 []
numberOp _ singleVal@[_] = throwError $ ArgumentNumber 2 singleVal
numberOp op params = mapM numberUnpacker params Data.Functor.<&> (Number . foldl1 op)

numberUnpacker :: Values -> ThrowsError Integer
numberUnpacker (Number n) = return n
numberUnpacker (String s) = let parsed = reads s in if null parsed
                                                    then throwError $ TypeMismatch "number" $ String s
                                                    else return $ fst $ head parsed
numberUnpacker (List [n]) = numberUnpacker n
numberUnpacker notNum = throwError $ TypeMismatch "number" notNum

boolOpBase :: (Values -> ThrowsError a) -> (a -> a -> Bool) -> [Values] -> ThrowsError Values
boolOpBase unpackerf op args = if length args /= 2
                           then throwError $ ArgumentNumber 2 args
                           else do left <- unpackerf $ head args
                                   right <- unpackerf $ args !! 1
                                   return $ Bool $ left `op` right

boolOp :: (Integer -> Integer -> Bool) -> [Values] -> ThrowsError Values
boolOp = boolOpBase numberUnpacker
