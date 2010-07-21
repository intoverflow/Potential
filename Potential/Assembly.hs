{-# LANGUAGE
	NoImplicitPrelude,
	ExistentialQuantification,
	ScopedTypeVariables,
	GADTs,
	TypeSynonymInstances,
	TypeFamilies,
	Rank2Types,
	FlexibleContexts,
	FlexibleInstances,
	MultiParamTypeClasses
	#-}
module Potential.Assembly
	( Reg(..)
	, Instr(..)
	, Deref(..)
	, PState(..), psGet, psPut, psModify
	, runCode, Code
	, Function(..), isFn
	) where

import Prelude( String, Integer, undefined, (++), ($) )

import Data.Maybe
import Data.Word

import Potential.MachineState( Reg )
import Potential.IxCode
import Potential.Constraints
import Potential.IxMonad
import Potential.IxMonad.Constrained
import Potential.IxMonad.State
import Potential.IxMonad.Writer
import Potential.IxMonad.PState

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

instance IxCode (Code c) where type Constraints (Code c) = c
instance ASMable (Code c) Instr where
  asm cnstrts code = let (_, asmcode, _) = runCode code cnstrts undefined
		     in asmcode

runCode :: Code c ct x y a -> c -> x -> (a, [Instr], y)
runCode code constr input =
	let ((a, w), y) =
		runPState (runIxWriterT (runIxConstrainedT code constr)) input
	in (a, w, y)

data Function m assumes returns =
     Fn { fnname   :: String
	, body     :: (IxCode m, ASMable m Instr) =>
			m Terminal assumes returns ()
	}
isFn :: Function m assumes returns -> Function m assumes returns
isFn f = f


-- deref mem_location (%ebx, %ecx, 4) means [ebx + ecx*4 + mem_location]
-- i.e., this is at&t syntax
data Deref =
    Deref  Integer (Reg, Reg, Integer)
 |  Deref2 Integer Reg -- means int + (%reg)
 |  Deref3 Reg

data Instr =
    Cmt String
 |  Ld Deref Reg
 |  Sto Reg Deref
 |  Mov Reg Reg
 |  Push Reg
 |  Pop Reg
 |  Cmp Reg Reg
 |  forall m assumes returns . IxCode m => SJe (Function m assumes returns)
 |  Jne Reg
 |  Jmp Reg
 |  forall m assumes returns . IxCode m => SJmp (Function m assumes returns)
 |  Call Reg
 |  forall m assumes returns . IxCode m => SCall (Function m assumes returns)
 |  Ret
 |  Lidt Reg
 |  ShL Integer Reg
 |  ShR Integer Reg
 |  And Word64 Reg
 |  Or Reg Reg
 |  Enter Word16
 |  Leave
 |  Label String
 -- temporary instructions, used by the memory manager
 |  Alloc
 |  EnterRegion
 |  CloseRegion
 |  GoUpRegion
 |  ComeDownRegion
 |  TxOwnership


