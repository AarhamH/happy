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
            , ("cons", listConstruct)
            , ("weq", weakEq)
            ]

numberOp :: (Integer -> Integer -> Integer) -> [Values] -> ThrowsError Values
numberOp _ [] = throwError $ ArgumentNumber 2 []
numberOp _ singleVal@[_] = throwError $ ArgumentNumber 2 singleVal
numberOp op fparams = mapM numberUnpacker fparams Data.Functor.<&> (Number . foldl1 op)

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

listHead :: [Values] -> ThrowsError Values
listHead [List(x:_)] = return x
listHead [ImproperList(x:_) _] = return x
listHead [badType] = throwError $ TypeMismatch "pair" badType
listHead badArgs = throwError $ ArgumentNumber 1 badArgs

listTail :: [Values] -> ThrowsError Values
listTail [List(_:xs)] = return $ List xs
listTail [ImproperList[_] xs] = return xs
listTail [ImproperList(_:xs) xs1] = return $ ImproperList xs xs1
listTail [badType] = throwError $ TypeMismatch "pair" badType
listTail badArgs = throwError $ ArgumentNumber 1 badArgs

listConstruct :: [Values] -> ThrowsError Values
listConstruct [x, List []] = return $ List [x]
listConstruct [x, List xs] = return $ List (x:xs)
listConstruct [x, ImproperList xs xs1] = return $ ImproperList (x:xs) xs1
listConstruct [x, y] = return $ ImproperList [x] y
listConstruct badList = throwError $ ArgumentNumber 2 badList

weakEq :: [Values] -> ThrowsError Values
weakEq [Bool a1, Bool a2] = return $ Bool $ a1 == a2
weakEq [Number a1, Number a2] = return $ Bool $ a1 == a2
weakEq [String a1, String a2] = return $ Bool $ a1 == a2
weakEq [Atom a1, Atom a2] = return $ Bool $ a1 == a2
weakEq [ImproperList xs x, ImproperList ys y] = weakEq [List $ xs ++ [x], List $ ys ++ [y]]
weakEq [List a1, List a2] = return $ Bool $ length a1 == length a2 && all pairs (zip a1 a2)
        where pairs (x,y) = case weakEq [x,y] of
                Right (Bool val) -> val
                _ -> False 
weakEq [_,_] = return $ Bool False
weakEq badList = throwError $ ArgumentNumber 2 badList