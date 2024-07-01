module Operators where

import Values

operators :: [(String, [Values] -> Values)]
operators = [ ("+", numberOp (+))
            , ("-", numberOp (-))
            , ("*", numberOp (*))
            , ("/", numberOp div)
            , ("mod", numberOp mod)
            , ("quotient", numberOp quot)
            , ("remainder", numberOp rem) 
            ]

numberOp :: (Integer -> Integer -> Integer) -> [Values] -> Values
numberOp op a = Number $ foldl1 op $ map numberUnpacker a

numberUnpacker :: Values -> Integer
numberUnpacker (Number n) = n
numberUnpacker (String s) = let parsed = reads s in
    if null parsed
        then 0
        else fst $ parsed !! 0
numberUnpacker (List [n]) = numberUnpacker n
numberUnpacker _ = 0

