{-# LANGUAGE
	NoImplicitPrelude,
	NoMonomorphismRestriction
	#-}
module Potential.Flow
	( primJmp, primCondJmp, primCall, primRet
	, sjmp, scall, ret
	) where

import Potential.Core
import Potential.Assembly
import Potential.Stack
import Potential.Pointer

-- Pass in the type of the function we're jumping to.  We take on its
-- post-conditions.
primJmp :: Code c Terminal x y ()
	-> Code c Terminal x y ()
primJmp _ = terminal $ unsafeReturn ()

primCondJmp :: Code c Terminal x y ()
	    -> Code c Terminal x y ()
	    -> Code c Terminal x y ()
primCondJmp jmpTo continueWith = continueWith

-- Pass in the function we're calling.  Note that it ret's to post-condition y,
-- so we must also leave the call instruction in that post-condition.  Note also
-- that it assumes condition x, so we must be in condition x just prior to the
-- call.
primCall :: Code c Terminal x y ()
	 -> Code c Composable x y ()
primCall _ = composable $ unsafeReturn ()

-- Pass in the type of the return function on the stack.  We'll assure that
-- the function which invokes ret leaves the machine in the proper state for
-- the ret to be safe.
primRet :: Code c Terminal x y ()
	-> Code c Terminal x x ()
primRet _ = terminal $ unsafeReturn ()


-- we do a forget to avoid an infinite type error
-- any way to get around this?
{-
jmp dst = instr ( Jmp (arg dst) )
	>>> pGet >>>= \ms ->
	primJmp (fromPtr64 $ get ms dst) $ primForget dst ms >>>= \retms ->
	pPut retms
-}

sjmp fn =
     do assertFunction fn
	instr ( SJmp fn )
	primJmp (body fn)

{-
call dst = instr ( Call (arg dst) )
	>>> pGet >>>= \ms ->
	let stack  = get ms rsp
	in primPush (undefined) stack >>>= \stack' ->
	let ms' = set ms rsp stack'
	in primCall (fromPtr64 $ get ms dst) $ primForget dst ms' >>>= \retms ->
	pPut retms
-}

scall fn =
     do assertFunction fn
	instr ( SCall fn )
	stack <- get rsp
	stack' <- primPush (undefined) stack
	set rsp stack'
	primCall (body fn)

ret =
     do instr ( Ret )
	stack <- get rsp
	(a', stack') <- primPop stack
	set rsp stack'
	retTo <- fromPtr64 a'
	primRet retTo

