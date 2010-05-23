{-# LANGUAGE
	NoMonomorphismRestriction,
	NoImplicitPrelude
	#-}
module Potential.Core
	(
	-- useful things from the prelude
	  ($), undefined, (++), Show(..), id, fromIntegral
	-- stuff from Core
	, comment
	, instr, get, set
	, handleIsOpen, alloc, free, realloc
	, assertType, assertRegisterType, assertRegister, assertFunction
	-- stuff from PotentialMonad
	, PotentialMonad, mixedReturn, return, (>>), (>>=), fail
	-- stuff that comes from Potential.Assembly
	, Reg(..), Instr(..), Deref(..), PState(..), Function(..)
	, Composable, Terminal, Failed, composable, terminal
	-- stuff that comes from MachineState
	, rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp, rflags
	, rip, r08, r09, r10, r11, r12, r13, r14, r15
	, arg, MS, Get
	, MSCmp(..)
	-- stuff that comes from Constraints
	, ConstraintsOn(..), ConstraintsOff(..)
	-- stuff that comes from Handles
	, Allocator, MaybeFree, MaybeHandleIsOpen, HS, C
	) where

import Prelude hiding ( return, fail, (>>), (>>=) )

import Potential.Size
import Potential.PotentialMonad
import Potential.Constraints
import Potential.MachineState
import Potential.Assembly
import Potential.Handles

-- In order to avoid madness, we don't export pGet, pPut, or pModify,
-- which have too much of an effect on our Hoare types to be safe to release
get field =
     do ms <- pGet
        let fdata = get' field ms
        return fdata
set field new = pModify (\ms -> set' field new ms)

getConstraints :: PState l c Composable x x c
getConstraints = return undefined

-- For logging instructions
instr :: Instr -> PState Instr c ct x x ()
instr i = pTell [i]


-- Code comments in the rendered code
comment s = instr $ Cmt s


-- Asserting an argument is a register
assertRegister r = let _ = isArg r
		   in composable $ return ()

assertFunction fn = let _ = isFn fn
		    in composable $ return ()


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
assertType :: a -> a -> PState Instr c Composable s s ()
assertType _ _ = return ()

assertRegisterType v t =
     do assertRegister v
	dv <- get v
	assertType t dv


-- Forgetting the type of an entry in the machine state
primForget dst = set dst (undefined :: a)

