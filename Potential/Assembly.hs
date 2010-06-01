{-# LANGUAGE
	NoImplicitPrelude,
	ExistentialQuantification
	#-}
module Potential.Assembly
	( Reg(..)
	, Instr(..)
	, Deref(..)
	, PState(..), psGet, psPut, psModify
	, runCode, Code
	, Function(..), isFn
	, composable, terminal, getFailure
	) where

import Prelude( String, Int, undefined, (++), ($) )

import Data.Maybe
import Data.Word

import Potential.MachineState( Reg )
import Potential.IxMonad
import Potential.IxMonad.Constrained
import Potential.IxMonad.State
import Potential.IxMonad.Writer

data PState x y ct a =
    PState  { runPState :: x -> (a, y) }
  | PFailed { getPFailure :: String }

psGet   = PState (\s -> (s, s))
psPut s = PState (\_ -> ((), s))
psModify f = do a <- psGet
		psPut $ f a

instance IxMonad PState where
  mixedReturn a = PState (\s -> (a, undefined))
  f >>>= m  = maybe (PState (\s1 -> let (a, s2)  = runPState f s1
					(a', s3) = runPState (m a) s2
				    in (a', s3)))
                    (PFailed) (getFailure f)
  fail e = PFailed { getPFailure = e }


getFailure :: PState x y ct a -> Maybe String
getFailure (PState _)  = Nothing
getFailure (PFailed f) = Just f

composable :: Code c x y Composable a -> Code c x y Composable a
composable p = p

terminal :: Code c x y Terminal a -> Code c x y Terminal a
terminal p = p

type Code c = IxConstrainedT c (IxWriterT [Instr] PState)
runCode :: Code c x y ct a -> c -> x -> (a, [Instr], y)
runCode code constr input =
	let ((a, w), y) =
		runPState (runIxWriterT (runIxConstrainedT code constr)) input
	in (a, w, y)

data Function c assumes returns =
     Fn { fnname   :: String
	, body     :: Code c assumes returns Terminal ()
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


