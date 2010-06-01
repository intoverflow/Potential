{-# LANGUAGE
	NoMonomorphismRestriction,
	NoImplicitPrelude
	#-}
module Potential.Core
	(
	-- useful things from the prelude
	  ($), undefined, (++), Show(..), id, fromIntegral, Bool(..)

	-- stuff from Core
	, comment
	, instr, forget, get, set, getConstraints, withConstraints
	, handleIsOpen, alloc, free, realloc
	, assertType, assertRegister, assertFunction

	-- stuff that comes from Potential.Assembly
	, Reg(..), Instr(..), Deref(..), Code, runCode, Function(..)
	, Composable, Terminal, composable, terminal

	-- stuff that comes from IxMonad
	, IxMonad(..)

	-- stuff that comes from MachineState
	, rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp, rflags
	, rip, r08, r09, r10, r11, r12, r13, r14, r15
	, arg, MS, Get
	, MSCmp(..)

	-- stuff that comes from Constraints
	, ConstraintsOn(..), ConstraintsOff(..)

	-- stuff that comes from Handles
	, Allocator, MaybeFree, MaybeAlloc, MaybeHandleIsOpen, HS, C
	) where

import Prelude hiding ( return, fail, (>>), (>>=) )

import Potential.IxMonad
import Potential.IxMonad.Writer
import Potential.Size
import Potential.Constraints
import Potential.MachineState
import Potential.Assembly
import Potential.Handles

-- In order to avoid madness, we don't export pGet, pPut, or pModify,
-- which have too much of an effect on our Hoare types to be safe to release
get field = lift $ lift $
     do ms <- psGet
        let fdata = get' field ms
        return fdata
set field new = lift $ lift $ psModify (\ms -> set' field new ms)

getConstraints :: Code c x x Composable c
getConstraints = return undefined

withConstraints :: Code ConstraintsOn x y ct b
		-> Code ConstraintsOn x y ct b
withConstraints p = p

-- For logging instructions
instr :: Instr -> Code c x x ct ()
instr i = tell [i]


-- Code comments in the rendered code
comment s = instr $ Cmt s


-- Asserting an argument is a register
assertRegister r = let _ = isArg r
		   in composable $ return ()

assertFunction fn = let _ = isFn fn
		    in composable $ return ()


-- Forgetting

forget dst =
     do assertRegister dst
	set dst (undefined :: a)


-- Managing handles
handleIsOpen h =
     do a <- get Alloc
	c <- getConstraints
	return $ handleIsOpen' c a h

alloc =
     do a <- get Alloc
	c <- getConstraints
	let (a', h) = alloc' c a
	set Alloc a'
	return h

free h =
     do a <- get Alloc
	c <- getConstraints
	set Alloc $ free' c a h
	return ()

realloc h =
     do a <- get Alloc
	c <- getConstraints
	set Alloc $ realloc' c a h
	return ()


-- Asserting the type of an entry in the machine state
assertType :: a -> a -> Code c s s Composable ()
assertType _ _ = return ()


-- Forgetting the type of an entry in the machine state
primForget dst = set dst (undefined :: a)

