{-# LANGUAGE
	NoImplicitPrelude,
	ExistentialQuantification,
	ScopedTypeVariables,
	GADTs
	#-}
module Potential.Assembly
	( Reg(..)
	, Instr(..)
	, Deref(..)
	, PState(..), psGet, psPut, psModify
	, runCode, Code
	, Function(..), isFn
	) where

import Prelude( String, Int, undefined, (++), ($) )

import Data.Maybe
import Data.Word

import Potential.MachineState( Reg )
import Potential.IxMonad
import Potential.IxMonad.Constrained
import Potential.IxMonad.State
import Potential.IxMonad.Writer

data PState ct a where
    PState  :: (ct x y) -> ( x -> (a, y) ) -> PState (ct x y) a

runPState :: PState (ct x y) a -> x -> (a, y)
runPState (PState ct f) = \x -> f x

psGet   = unmodeled $ PState undefined (\s -> (s, s))
psPut s = composable $ PState undefined (\_ -> ((), s))
psModify f = do a <- psGet
		psPut $ f a

instance IxFunctor PState where
  fmap f (PState ct ps) = PState ct $ \x -> let (a, y) = ps x
					    in (f a, y)

instance IxMonad PState where
  unsafeReturn a = PState undefined (\s -> (a, undefined))
  (p :: PState (ct x y) a) >>= (m :: a -> PState (ct' y z) b)
	= case p of
	    PState _ f ->
		    PState undefined $ \s1 ->
			let (a, s2)     = f s1
			    (a', s3)    = runPState (m a) s2
			in (a', s3)


type Code c = IxConstrainedT c (IxWriterT [Instr] PState)

runCode :: Code c (ct x y) a -> c -> x -> (a, [Instr], y)
runCode code constr input =
	let ((a, w), y) =
		runPState (runIxWriterT (runIxConstrainedT code constr)) input
	in (a, w, y)

data Function c assumes returns =
     Fn { fnname   :: String
	, body     :: Code c (Terminal assumes returns) ()
	}
isFn :: Function c assumes returns -> Function c assumes returns
isFn f = f


-- deref mem_location (%ebx, %ecx, 4) means [ebx + ecx*4 + mem_location]
-- i.e., this is at&t syntax
data Deref =
    Deref  Int (Reg, Reg, Int)
 |  Deref2 Int Reg -- means int + (%reg)
 |  Deref3 Reg

data Instr =
    Cmt String
 |  Ld Deref Reg
 |  Sto Reg Deref
 |  Mov Reg Reg
 |  Push Reg
 |  Pop Reg
 |  Cmp Reg Reg
 |  forall c assumes returns . SJe (Function c assumes returns)
 |  Jne Reg
 |  Jmp Reg
 |  forall c assumes returns . SJmp (Function c assumes returns)
 |  Call Reg
 |  forall c assumes returns . SCall (Function c assumes returns)
 |  Ret
 |  Lidt Reg
 |  ShL Int Reg
 |  ShR Int Reg
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


