module Evaluator where 

import Values

evaluate :: Values -> Values
evaluate val@(String _) = val
evaluate val@(Number _) = val
evaluate val@(Bool _) = val
evaluate (List [Atom "quote", val]) = val