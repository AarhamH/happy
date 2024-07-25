module Variables where
import Values
import Control.Monad.Except
import Data.IORef

getVar :: IOEnvironment -> String -> IOThrowsError Values
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Whoops! Getting unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: IOEnvironment -> String -> Values -> IOThrowsError Values
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "LOL! Setting an unbound variable" var)
                                   (liftIO . (`writeIORef` value))
                                   (lookup var env)
                             return value

defineVar :: IOEnvironment -> String -> Values -> IOThrowsError Values
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

bindVars :: IOEnvironment -> [(String, Values)] -> IO IOEnvironment
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv fbindings env = fmap (++ env) (mapM addBinding fbindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)