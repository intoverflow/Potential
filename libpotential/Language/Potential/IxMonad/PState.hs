{-
  Copyright 2010 Timothy Carstens    carstens@math.utah.edu

  This file is part of the Potential Standard Library.

    The Potential Standard Library is free software: you can redistribute it
    and/or modify it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, version 3 of the License.

    The Potential Standard Library is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with the Potential Standard Library.  If not, see
    <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE
	NoImplicitPrelude,
	FunctionalDependencies,
	MultiParamTypeClasses,
	FlexibleInstances,
	FlexibleContexts,
	UndecidableInstances,
        TypeSynonymInstances,
        TypeFamilies
	#-}
module Language.Potential.IxMonad.PState
	( IxPState(..), PState, runCode, Code, isCode
	) where

import Prelude( undefined, ($), flip, String )
import Language.Potential.IxMonad.IxMonad
import Language.Potential.IxMonad.Constrained
import Language.Potential.IxMonad.Writer
import Language.Potential.Assembly
import Language.Potential.Label

class (IxMonad m) => IxPState m where
  psGet :: m Unmodeled x x x
  psPut :: y -> m Composable x y ()
  psModify :: (x -> y) -> m (Compose (Compose Unmodeled Unmodeled) Composable) x y ()
  psModify f = do a <- psGet
		  psPut (f a)

data PState ct x y a = PState { runPState :: x -> (a, y) }

instance IxFunctor PState where
  fmap f (ps) = PState $ \x ->  let (a, y) = runPState ps x
				in (f a, y)

instance IxMonad PState where
  unsafeReturn a = PState $ \s -> (a, undefined)
  ps >>= m = PState $ \s1 -> let (a, s2) = runPState ps s1
			     in runPState (m a) s2

instance IxPState PState where
  psGet   = unmodeled  $ PState $ \s -> (s, s)
  psPut s = composable $ PState $ \_ -> ((), s)

type Code c = LabelGen (IxConstrainedT c (IxWriterT [Instr] PState))

isCode :: (Code c) Unmodeled x x ()
isCode = return ()


instance (IxMonadWriter [Instr] (Code c)) =>
  IxCode (Code c) where type Constraints (Code c) = c
instance ( IxMonadWriter [Instr] (Code c) )
 => ASMable (Code c) where
  asm cnstrts nm code =
	let (_, asmcode, _) = runCode nm code cnstrts undefined
	in asmcode

runCode :: String -> Code c ct x y a -> c -> x -> (a, [Instr], y)
runCode nm code constr input =
	let (((a,_), w), y) =
		(flip runPState) input
		$ runIxWriterT
		$ (flip runIxConstrainedT) constr
		$ runLabel nm code
	in (a, w, y)


