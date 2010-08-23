{-
  Copyright 2010 Timothy Carstens    carstens@math.utah.edu

  This file is part of the Potential Compiler.

    The Potential Compiler is free software: you can redistribute it and/or
    modify it under the terms of the GNU General Public License as published
    by the Free Software Foundation, version 3 of the License.

    The Potential Compiler is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
-}
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

type Compiler = StateT CompilerState (ReaderT Mode IO)

runCompiler :: Compiler a -> Mode -> CompilerState -> IO (a, CompilerState)
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

