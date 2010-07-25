{-# LANGUAGE
	FlexibleContexts,
	FlexibleInstances,
	MultiParamTypeClasses #-}
module PC.Base where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans

import Language.Haskell.Interpreter (InterpreterT)

import PC.Config

data CompilerState =
    CompilerState { msgDepth :: Int }

instance MonadState CompilerState (InterpreterT Compiler) where
  get = lift get
  put s = lift (put s)

type Compiler = StateT CompilerState (ReaderT Config IO)

runCompiler :: Compiler a -> Config -> CompilerState -> IO (a, CompilerState)
runCompiler compiler config compilerState =
  runReaderT (runStateT compiler compilerState) config

inDepth :: MonadState CompilerState m => m a -> m a
inDepth act =
     do let d = 2
	modify (\cs -> cs{ msgDepth = msgDepth cs + d })
	a <- act
	modify (\cs -> cs{ msgDepth = msgDepth cs - d })
	return a

say :: String -> InterpreterT Compiler ()
say s =
     do depth <- (lift get) >>= return . msgDepth
	let ws = replicate depth ' '
	liftIO $ putStrLn $ ws ++ s

