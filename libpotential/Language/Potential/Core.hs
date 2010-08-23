{-
  Copyright 2010 Timothy Carstens    carstens@math.utah.edu

  This file is part of the Potential Standard Library.

    The Potential Standard Library is free software: you can redistribute it
    and/or modify it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, version 3 of the License.

    The Potential Compiler is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE
	NoMonomorphismRestriction,
	NoImplicitPrelude,
	FlexibleContexts,
	TypeFamilies
	#-}
module Language.Potential.Core
	(
	-- useful things from the prelude
	  ($), undefined, (++), Show(..), id
	, fromIntegral, fromInteger, Bool(..)

	-- stuff from Core
	, comment
	, instr, forget, get, set, getConstraints, withConstraints
	, assertType, assertRegister, assertFunction
	, evaluateTypes

	-- stuff that comes from Potential.Assembly
	, Reg(..), Instr(..), Deref(..), Function(..)
	, Unmodeled, Composable, Terminal, unmodeled, composable, terminal

	-- stuff that comes from PState
	, IxCode, ASMable, asm, isCode, Code

	-- stuff that comes from IxMonad
	, Composition(..)
	, IxFunctor(..), IxMonad(..), IxMonadTrans(..)

	-- stuff that comes from Model
	, rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp, rflags
	, rip, r08, r09, r10, r11, r12, r13, r14, r15, rcmp
	, MS

	-- stuff that comes from SetGet
	, Set, Get, arg

	-- stuff that comes from Constraints
	, ConstraintsOn(..), ConstraintsOff(..)
	) where

import Prelude hiding ( return, fail, (>>), (>>=) )

import Language.Potential.IxMonad
import Language.Potential.IxMonad.Writer
import Language.Potential.IxMonad.PState
import Language.Potential.Size
import Language.Potential.Constraints
import Language.Potential.Arch.SetGet
import Language.Potential.Arch.Amd64.Model
import Language.Potential.Assembly


-- Type families evaluate lazily.
-- If they mention any quantified free variables lazily, this will
-- count as escaping.  If it turns out that evaluating the type functions
-- will not include such quantified free variables, you won't have
-- an escaping problem.
-- This function can be used to force lazy type functions to evaluate.
-- Highly relevant while working with regions.
evaluateTypes :: IxMonad m
   => m ct
	(MS rax rbx rcx rdx rsi rdi rbp rsp rflags
	    rip r08 r09 r10 r11 r12 r13 r14 r15 alloc cmp)
	(MS sax sbx scx sdx ssi sdi sbp ssp sflags
	    sip s08 s09 s10 s11 s12 s13 s14 s15 salloc scmp)
	a
   -> m ct
	(MS rax rbx rcx rdx rsi rdi rbp rsp rflags
	    rip r08 r09 r10 r11 r12 r13 r14 r15 alloc cmp)
	(MS sax sbx scx sdx ssi sdi sbp ssp sflags
	    sip s08 s09 s10 s11 s12 s13 s14 s15 salloc scmp)
	a
evaluateTypes a = a

isUsingMS :: (x ~ MS rax rbx rcx rdx rsi rdi rbp rsp rflags
		     rip r08 r09 r10 r11 r12 r13 r14 r15 alloc cmp, IxMonad m)
	=> m Unmodeled x x ()
isUsingMS = return ()

get field =
     do isUsingMS
	ms <- psGet
        let fdata = get' field ms
        return fdata
set field new =
     do isUsingMS
	psModify (\ms -> set' field new ms)

getConstraints :: IxCode m => m Unmodeled x x (Constraints m)
getConstraints = return undefined

withConstraints :: (IxCode m, Constraints m ~ ConstraintsOn)
			=> m ct x y b
			-> m ct x y b
withConstraints p = p

-- For logging instructions
instr :: (IxMonadWriter [Instr] m) => Instr -> m Unmodeled x x ()
instr i = tell [i]


-- Code comments in the rendered code
comment s = instr $ Cmt s


-- Asserting an argument is a register
assertRegister r = let _ = isArg r
		   in unmodeled $ return ()

assertFunction :: IxCode m => Function m assumes returns -> m Unmodeled x x ()
assertFunction fn = let _ = isFn fn
		    in unmodeled $ return ()


-- Forgetting

forget dst =
     do assertRegister dst
	set dst (undefined :: a)


-- Asserting the type of an entry in the machine state
assertType :: a -> a -> Code c Unmodeled x x ()
assertType _ _ = return ()


-- Forgetting the type of an entry in the machine state
primForget dst = set dst (undefined :: a)

