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

evaluateString :: String -> IO String
evaluateString expr = return $ extractValue $ trapError (fmap show $ readExpr expr >>= evaluateExpr)

responsePrint :: String -> IO ()
responsePrint expr = evaluateString expr >>= putStrLn

killCondition :: Monad m => (a->Bool) -> m a -> (a -> m ()) -> m ()
killCondition predicate prompt action = do
    result <- prompt
    if predicate result
        then return ()
        else action result >> killCondition predicate prompt action

run :: IO ()
run = killCondition (== "q") (readInput ":) >> ") responsePrint

main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> run
               1 -> responsePrint $ head args
               _ -> putStrLn "Program takes only 0 or 1 argument"