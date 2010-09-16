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
	ExistentialQuantification,
	ScopedTypeVariables,
	GADTs,
	TypeFamilies,
	Rank2Types,
	FlexibleContexts,
	DeriveDataTypeable
	#-}
module Language.Potential.Assembly
	( Reg(..)
	, Instr(..)
	, Deref(..)
	, Function(..), isFn
	, IxCode(..), ASMable(..)
	, funName, getAssembly
	) where

import Prelude( String, Integer, Int, foldl, undefined, (++), ($) )

import Data.Maybe
import Data.Word
import Data.Typeable

import Language.Potential.Arch.Amd64.Model( Reg )
import Language.Potential.Constraints
import Language.Potential.IxMonad.IxMonad
import Language.Potential.IxMonad.Writer

class (IxMonad m, IxMonadWriter [Instr] m) => IxCode m where type Constraints m

class IxCode m => ASMable m where
  asm :: Constraints m -> String -> m ct x y a -> [Instr]

data Function m assumes returns =
     Fn { fnname   :: String
	, body     :: (IxCode m, ASMable m) =>
			m Terminal assumes returns ()
	}

isFn :: Function m assumes returns -> Function m assumes returns
isFn f = f

funName :: (Constraints m ~ ConstraintsOff) => Function m assumes returns -> String
funName f = fnname f

getAssembly :: (ASMable m, Constraints m ~ ConstraintsOff) =>
		Function m assumes returns -> [Instr]
getAssembly f = asm ConstraintsOff (fnname f) (body f)


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
 |  Lea Deref Reg
 |  Mov Reg Reg
 |  MovC Word64 Reg
 |  Push Reg
 |  Pop Reg
 |  CmpC Word64 Reg
 |  Cmp Reg Reg
 |  LJne String
 |  forall m assumes returns . IxCode m => SJe (Function m assumes returns)
 |  forall m assumes returns . IxCode m => SJne (Function m assumes returns)
 |  Jne Reg
 |  Jmp Reg
 |  forall m assumes returns . IxCode m => SJmp (Function m assumes returns)
 |  Call Reg
 |  forall m assumes returns . IxCode m => SCall (Function m assumes returns)
 |  Ret
 |  Lidt Reg
 |  ShL Integer Reg
 |  ShR Integer Reg
 |  And Reg Reg
 |  Or Reg Reg
 |  Add Reg Reg
 |  AddC Word64 Reg
 |  Sub Reg Reg
 |  SubC Word64 Reg
 |  Mul Reg Reg
 |  Enter Word16
 |  Leave
 |  Label String
 -- temporary instructions, used by the memory manager
 |  Alloc
 |  NewRegion
 |  KillRegion
 |  GoUpRegion
 |  ComeDownRegion
 |  TxOwnership
  deriving Typeable


