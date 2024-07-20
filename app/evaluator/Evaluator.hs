module Evaluator where 
import Values
import Operators
import Control.Monad.Except
import Variables

apply :: Values -> [Values] -> ExceptT Errors IO Values
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func fparams varargs fbody fclosure) args =
     if num fparams /= num args && isNothing varargs
          then throwError $ ArgumentNumber (num fparams) args
          else liftIO (bindVars fclosure $ zip fparams args) >>= bindVarArgs varargs >>= evalBody
     where remainingArgs = drop (length fparams) args
           num = toInteger . length
           evalBody env = last <$> mapM (evaluateExpr env) fbody
           bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
                Nothing -> return env
apply _ _ = throwError $ Default "Unknown error"

makeFunc :: Monad m => Maybe String -> IOEnvironment -> [Values] -> [Values] -> m Values
makeFunc varargs env fparams fbody = return $ Func (map showValue fparams) varargs fbody env
makeNormalFunc :: IOEnvironment -> [Values] -> [Values] -> ExceptT Errors IO Values
makeNormalFunc = makeFunc Nothing
makeVarArgs :: Values -> IOEnvironment -> [Values] -> [Values] -> ExceptT Errors IO Values
makeVarArgs = makeFunc . Just . showValue

evaluateExpr :: IOEnvironment -> Values -> IOThrowsError Values
evaluateExpr env val@(String _) = return val
evaluateExpr env val@(Number _) = return val
evaluateExpr env val@(Atom id) = getVar env id
evaluateExpr env val@(Bool _) = return val
evaluateExpr env (List [Atom "quote", val]) = return val
evaluateExpr env (List [Atom "if", p, c, a]) = 
     do result <- evaluateExpr env p
        case result of
             Bool False -> evaluateExpr env a
             _  -> evaluateExpr env c
evaluateExpr env (List [Atom "set!", Atom var, form]) = evaluateExpr env form >>= setVar env var
evaluateExpr env (List [Atom "define", Atom var, form]) = evaluateExpr env form >>= defineVar env var
evaluateExpr env (List (Atom func : args)) = mapM (evaluateExpr env) args >>= liftThrows . apply func
evaluateExpr env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm