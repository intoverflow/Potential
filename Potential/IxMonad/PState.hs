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
module Potential.IxMonad.PState
	( IxPState(..), PState, runCode, Code
	) where

import Prelude( undefined, ($) )
import Potential.IxMonad.IxMonad
import Potential.IxMonad.Constrained
import Potential.IxMonad.Writer
import Potential.Assembly

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

type Code c = IxConstrainedT c (IxWriterT [Instr] PState)

instance (IxMonadWriter [Instr] (Code c)) =>
  IxCode (Code c) where type Constraints (Code c) = c
instance (IxMonadWriter [Instr] (Code c)) => ASMable (Code c) where
  asm cnstrts code = let (_, asmcode, _) = runCode code cnstrts undefined
		     in asmcode

runCode :: Code c ct x y a -> c -> x -> (a, [Instr], y)
runCode code constr input =
	let ((a, w), y) =
		runPState (runIxWriterT (runIxConstrainedT code constr)) input
	in (a, w, y)


