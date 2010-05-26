{-# LANGUAGE
	EmptyDataDecls,
	ExistentialQuantification
	#-}
module Potential.Assembly
	( Reg(..)
	, Instr(..)
	, Deref(..)
	, PState(..), Function(..), isFn
	, Composable, Terminal, composable, terminal, getFailure
	) where

import Data.Word

import Potential.MachineState( Reg )

data Composable
composable :: PState l c Composable s1 s2 s3 a -> PState l c Composable s1 s2 s3 a
composable p = p

data Terminal
terminal :: PState l c Terminal s1 s2 s3 a -> PState l c Terminal s1 s2 s3 a
terminal p = p

data PState l c ct s1 s2 s3 a =
    PState  { runPState :: c -> s1 -> (a, s2, [l]) }
  | PFailed { getPFailure :: String }

getFailure :: PState l c ct s1 s2 s3 a -> Maybe String
getFailure (PState _)  = Nothing
getFailure (PFailed f) = Just f

data Function c assumes returns =
     Fn { fnname   :: String
	, body     :: PState Instr c Terminal assumes returns returns ()
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


