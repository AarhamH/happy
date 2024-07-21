module Main where
import Parser
import Evaluator
import Operators
import Values
import Variables
import System.Environment (getArgs)
import System.IO

streamFlusher :: String -> IO ()
streamFlusher str =putStr str >> hFlush stdout

readInput :: String -> IO String
readInput str = streamFlusher str >> getLine

evaluateString :: IOEnvironment -> String -> IO String
evaluateString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= evaluateExpr env

responsePrint :: IOEnvironment -> String -> IO ()
responsePrint env expr = evaluateString env expr >>= putStrLn

killCondition :: Monad m => (a->Bool) -> m a -> (a -> m ()) -> m ()
killCondition predicate prompt action = do
    result <- prompt
    if predicate result
        then return ()
        else action result >> killCondition predicate prompt action

primitiveBindings :: IO IOEnvironment
primitiveBindings = nullEnv >>= flip bindVars (map makePrimitiveFunc operators)
     where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    runIOThrows (show <$> evaluateExpr env (List [Atom "load", String (head args)]))
        >>= hPutStrLn stderr

run :: IO ()
run = primitiveBindings >>= killCondition (== "quit") (readInput "Lisp>>> ") . responsePrint


main :: IO ()
main = do args <- getArgs
          if null args then run else runOne args