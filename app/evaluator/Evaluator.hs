module Evaluator where 

import Values
import Operators

apply :: String -> [Values] -> Values
apply f a = maybe (Atom "Not found") ($ a) $ lookup f operators

evaluateExpr :: Values -> Values
evaluateExpr val@(String _) = val
evaluateExpr val@(Number _) = val
evaluateExpr val@(Bool _) = val
evaluateExpr (List [Atom "quote", val]) = val
evaluateExpr (List (Atom func : args)) = apply func $ map evaluateExpr args
evaluateExpr _ = Atom "Not yet implemented"


