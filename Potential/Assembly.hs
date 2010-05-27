{-# LANGUAGE
	NoImplicitPrelude,
	ExistentialQuantification
	#-}
module Potential.Assembly
	( Reg(..)
	, Instr(..)
	, Deref(..)
	, PState(..), pGet, pPut, pModify, pTell
	, Function(..), isFn
	, composable, terminal, getFailure
	) where

import Prelude( String, Int, undefined, (++) )

import Data.Maybe
import Data.Word

import Potential.MachineState( Reg )
import Potential.IxMonad.IxMonad

composable :: PState l c s1 s2 s3 Composable a -> PState l c s1 s2 s3 Composable a
composable p = p

terminal :: PState l c s1 s2 s3 Terminal a -> PState l c s1 s2 s3 Terminal a
terminal p = p

data PState l c s1 s2 s3 ct a =
    PState  { runPState :: c -> s1 -> (a, s2, [l]) }
  | PFailed { getPFailure :: String }

getFailure :: PState l c s1 s2 s3 ct a -> Maybe String
getFailure (PState _)  = Nothing
getFailure (PFailed f) = Just f

instance IxMonad (PState l c) where
  mixedReturn a = PState (\_ s -> (a, undefined, []))
  return a = PState (\_ s -> (a, s, []))
  f >>= m  = maybe (PState (\c s1 -> let (a, s2, l)   = runPState f c s1
					 (a', s3, l') = runPState (m a) c s2
				     in (a', s3, l ++ l')))
                    (PFailed) (getFailure f)
  m1 >> m2 = m1 >>= \_ -> m2
  fail e = PFailed { getPFailure = e }

pGet      = PState (\_ s -> (s, s, []))
pPut s    = PState (\_ _ -> ((), s, []))
pModify f = pGet >>= \x -> pPut (f x)
pTell l   = PState (\_ s -> ((), s, l))

data Function c assumes returns =
     Fn { fnname   :: String
	, body     :: PState Instr c assumes returns returns Terminal ()
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


