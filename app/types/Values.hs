module Values where

data Values = Atom String
            | List [Values]
            | ImproperList [Values] Values
            | Number Integer
            | String String
            | Bool Bool

unwordsList :: [Values] -> String
unwordsList = unwords . map showValue 

showValue :: Values -> String 
showValue (Atom name) = name
showValue (String contents) = "\"" ++ contents ++ "\""
showValue (Number contents) = show contents
showValue (List contents) = "(" ++ unwordsList contents ++ ")"
showValue (ImproperList head tail) = "(" ++ unwordsList head ++ " . " ++ showValue tail ++ ")"
showValue (Bool True) = "#t"
showValue (Bool False) = "#f"

instance Show Values where show = showValue