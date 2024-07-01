module Evaluator where 

import Values

evaluateExpr :: Values -> Values
evaluateExpr val@(String _) = val
evaluateExpr val@(Number _) = val
evaluateExpr val@(Bool _) = val
evaluateExpr (List [Atom "quote", val]) = val
evaluateExpr _ = Atom "Not yet implemented"
