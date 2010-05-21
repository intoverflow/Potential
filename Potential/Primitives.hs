{-# LANGUAGE
	NoImplicitPrelude,
	NoMonomorphismRestriction,
	Rank2Types,
	FlexibleContexts #-}
module Potential.Primitives
	( assertSameType
	, instr
	, comment, mov, push, pop, sjmp, scall, ret, enter, leave
	, assumeType, ptrIsValid
	, primJmp, assertInt64
	) where

import Prelude hiding ((>>), (>>=), return, fail)

import Control.Monad.Writer hiding ( (>>), (>>=), return, fail )

import Potential.Constraints
import Potential.MachineState
import Potential.Assembly
import Potential.PrimTypes
import Potential.Stack
import Potential.Size
import Potential.PMonad

instr :: Instr -> PState Instr c x x ()
instr i = pTell [i]

-- primitives (for use by the language implementation)
primJmp :: (PState Instr c x y b) -> PState Instr c x y ()
primJmp _ = mixedReturn ()

primRet :: (PState Instr c x y b) -> PState Instr c x x ()
primRet _ = mixedReturn ()

-- primForget can be used to avoid infinite type errors
primForget dst = set dst (undefined :: a)

-- assertions about our arguments
assertInt64 :: Int64 -> PState Instr c s s ()
assertInt64 _ = return ()

assertSameType :: a -> a -> PState Instr c s s ()
assertSameType _ _ = return ()

-- some instructions
comment str = instr $ Cmt str

mov src dst =
     do instr $ Mov (arg src) (arg dst)
	d <- get src
	set dst d


push src =
     do instr ( Push (arg src) )
	stack <- get rsp
	a' <- get src
	stack' <- primPush a' stack
	set rsp stack'

pop dst =
     do instr ( Pop (arg dst) )
	stack <- get rsp
	(a', stack') <- primPop stack
	set dst a'
	set rsp stack'

-- we do a forget to avoid an infinite type error
-- any way to get around this?
{-
jmp dst = instr ( Jmp (arg dst) )
	>>> pGet >>>= \ms ->
	primJmp (fromPtr64 $ get ms dst) $ primForget dst ms >>>= \retms ->
	pPut retms
-}

sjmp fn =
     do instr ( SJmp fn )
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
     do instr ( SCall fn )
	stack <- get rsp
	stack' <- primPush (undefined) stack
	set rsp stack'
	primJmp (body fn)

ret =
     do instr ( Ret )
	stack <- get rsp
	(a', stack') <- primPop stack
	set rsp stack'
	primRet (fromPtr64 a')

enter frame =
     do let l = fromIntegral $ toInt $ sz frame
	instr (Enter l)
	baseptr <- get rbp
	stack   <- get rsp
	stack' <- primPush baseptr stack
	(baseptr', stack'') <- makeFramePtr64 stack' frame
	set rbp baseptr'
	set rsp stack''

leave =
     do instr (Leave)
	baseptr <- get rbp
	stack <- getStackFromFramePtr64 baseptr
	(baseptr', stack') <- primPop stack
	set rbp baseptr'
	set rsp stack'

assumeType v t =
     do dv <- get v
	assertSameType t dv

ptrIsValid src =
     do ptr <- get src
	_ <- fromPtr64 ptr
	return ()

