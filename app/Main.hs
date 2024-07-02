module Main where
import Parser
import Evaluator
import Values
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let evaled = fmap show $ readExpr (head args) >>= evaluateExpr
    putStrLn $ extractValue $ trapError evaled