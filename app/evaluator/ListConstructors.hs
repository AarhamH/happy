module ListConstructors where

import Values
import Control.Monad.Except ( MonadError(throwError) )

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
listConstruct [x, List []] = return $ List[x]
listConstruct [x, List xs] = return $ List $ (x:xs)
listConstruct [x, ImproperList xs xs1] = return $ ImproperList (x:xs) xs1
listConstruct [x, y] = return $ ImproperList [x] y
listConstruct badList = throwError $ ArgumentNumber 2 badList
