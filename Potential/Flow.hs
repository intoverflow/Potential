{-# LANGUAGE
	NoMonomorphismRestriction,
	NoImplicitPrelude
	#-}
module Potential.Flow
	( primJmp, primCall, primRet
	, sjmp, scall, ret
	) where

import Potential.Core
import Potential.Assembly
import Potential.Stack
import Potential.Pointer

primJmp :: (PState Instr c Terminal x y b) -> PState Instr c Terminal x y ()
primJmp _ = terminal $ mixedReturn ()

primCall :: (PState Instr c Terminal x y b) -> PState Instr c Composable x y ()
primCall _ = composable $ mixedReturn ()

primRet :: (PState Instr c Terminal x y b) -> PState Instr c Terminal x x ()
primRet _ = terminal $ mixedReturn ()


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
	primRet (fromPtr64 a')


