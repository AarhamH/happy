module Operators where

import Values
import Control.Monad.Except
import ListConstructors
import qualified Data.Functor

operators :: [(String, [Values] -> ThrowsError Values)]
operators = [ ("+", numberOp (+))
            , ("-", numberOp (-))
            , ("*", numberOp (*))
            , ("/", numberOp div)
            , ("mod", numberOp mod)
            , ("quotient", numberOp quot)
            , ("remainder", numberOp rem)
            , ("=", eqOp (==))
            , ("~=", eqOp (/=))
            , ("<=", eqOp (<=))
            , (">=", eqOp (>=))
            , ("<", eqOp (<))
            , (">", eqOp (>))
            , ("&&", boolOp (&&))
            , ("||", boolOp (||))
            , ("string=?", stringOp (==))
            , ("string~=?", stringOp (/=))
            , ("string>?", stringOp (>))
            , ("string<?", stringOp (<))
            , ("string>=?", stringOp (>=))
            , ("string<=?", stringOp (<=))
            , ("head", listHead)
            , ("tail", listTail)
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

boolUnpacker :: Values -> ThrowsError Bool
boolUnpacker (Bool bool) = return bool
boolUnpacker errVal = throwError $ TypeMismatch "Boolean" errVal

stringUnpacker :: Values -> ThrowsError String
stringUnpacker (String str) = return str
stringUnpacker (Number num) = return $ show num
stringUnpacker (Bool bool) = return $ show bool
stringUnpacker notStr = throwError $ TypeMismatch "string" notStr

boolOpBase :: (Values -> ThrowsError a) -> (a -> a -> Bool) -> [Values] -> ThrowsError Values
boolOpBase unpackerf op args = if length args /= 2
                           then throwError $ ArgumentNumber 2 args
                           else do left <- unpackerf $ head args
                                   right <- unpackerf $ args !! 1
                                   return $ Bool $ left `op` right

eqOp :: (Integer -> Integer -> Bool) -> [Values] -> ThrowsError Values
eqOp = boolOpBase numberUnpacker

boolOp :: (Bool -> Bool -> Bool) -> [Values] -> ThrowsError Values
boolOp = boolOpBase boolUnpacker

stringOp :: (String -> String -> Bool) -> [Values] -> ThrowsError Values
stringOp = boolOpBase stringUnpacker