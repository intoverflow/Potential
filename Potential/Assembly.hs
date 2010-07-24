{-# LANGUAGE
	NoImplicitPrelude,
	ExistentialQuantification,
	ScopedTypeVariables,
	GADTs,
	TypeFamilies,
	Rank2Types,
	FlexibleContexts
	#-}
module Potential.Assembly
	( Reg(..)
	, Instr(..)
	, Deref(..)
	, Function(..), isFn
	, IxCode(..), ASMable(..)
	, funName
	) where

import Prelude( String, Integer, Int, foldl, undefined, (++), ($) )

import Data.Maybe
import Data.Word

import Potential.MachineState( Reg )
import Potential.Constraints
import Potential.IxMonad.IxMonad
import Potential.IxMonad.Writer

class (IxMonad m, IxMonadWriter [Instr] m) => IxCode m where type Constraints m

class IxCode m => ASMable m where
  asm :: Constraints m -> m ct x y a -> [Instr]

data Function m assumes returns =
     Fn { fnname   :: String
	, body     :: (IxCode m, ASMable m) =>
			m Terminal assumes returns ()
	}

isFn :: Function m assumes returns -> Function m assumes returns
isFn f = f

funName :: (Constraints m ~ ConstraintsOff) => Function m assumes returns -> String
funName f = fnname f


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


