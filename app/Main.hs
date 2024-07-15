module Main where
import Parser
import Evaluator
import Values
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

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip responsePrint expr

run :: IO ()
run = nullEnv >>= killCondition (== "quit") (readInput "Lisp>>> ") . responsePrint

main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> run
               1 -> runOne $ head args
               _ -> putStrLn "Program takes only 0 or 1 argument"