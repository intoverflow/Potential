{-# LANGUAGE
	NoMonomorphismRestriction,
	NoImplicitPrelude,
	FlexibleContexts
	#-}
module Potential.Core
	(
	-- useful things from the prelude
	  ($), undefined, (++), Show(..), id, fromIntegral, Bool(..)

	-- stuff from Core
	, comment
	, instr, forget, get, set, getConstraints, withConstraints
	, assertType, assertRegister, assertFunction

	-- stuff that comes from Potential.Assembly
	, Reg(..), Instr(..), Deref(..), Code, runCode, Function(..)
	, Composable, Terminal, composable, terminal

	-- stuff that comes from IxMonad
	, IxMonad(..), IxMonadTrans(..)

	-- stuff that comes from MachineState
	, rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp, rflags
	, rip, r08, r09, r10, r11, r12, r13, r14, r15
	, arg, MS, Get
	, rcmp

	-- stuff that comes from Constraints
	, ConstraintsOn(..), ConstraintsOff(..)
	) where

import Prelude hiding ( return, fail, (>>), (>>=) )

import Potential.IxMonad
import Potential.IxMonad.Writer
import Potential.Size
import Potential.Constraints
import Potential.MachineState
import Potential.Assembly

get field = (comment $ "get " ++ show field) >> (lift $ lift $
     do ms <- psGet
        let fdata = get' field ms
        return fdata)
set field new = (comment $ "set " ++ show field) >> (lift $ lift $ psModify (\ms -> set' field new ms))

getConstraints :: Code c x x z Composable c
getConstraints = return undefined

withConstraints :: Code ConstraintsOn x y z ct b
		-> Code ConstraintsOn x y z ct b
withConstraints p = p

-- For logging instructions
instr :: (IxMonadWriter [Instr] m) => Instr -> m x x z ct ()
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


-- Asserting the type of an entry in the machine state
assertType :: a -> a -> Code c s s z Composable ()
assertType _ _ = return ()


-- Forgetting the type of an entry in the machine state
primForget dst = set dst (undefined :: a)

