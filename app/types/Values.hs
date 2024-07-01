module Values where

data Values = Atom String
            | List [Values]
            | ImproperList [Values] Values
            | Number Integer
            | String String
            | Bool Bool
            deriving (Show)