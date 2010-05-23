{-# LANGUAGE
	NoMonomorphismRestriction,
	NoImplicitPrelude,
	TypeFamilies,
	FlexibleContexts #-}
module Potential.Stack
	( push, pop
	, enter, leave
	, Stack, primPop, primPush
	, FrameBasePtr64
	) where

import Prelude( ($) )

import Potential.Size
import Potential.Pointer
import Potential.Assembly

import Potential.Core


-- Stacks
data Stack a b = Stack a b

assertStack :: Stack a b -> Stack a b
assertStack = id

splitStack :: Stack a (Stack a' b') -> (a, Stack a' b')
splitStack _ = (undefined, undefined)

asStack :: a -> b -> Stack a b
asStack _ _ = undefined

assertPushableSize :: (SZ a' :<=? T64) c => a' -> PState l c Composable x x ()
assertPushableSize _ = return ()

primPush a' sp =
     do assertPushableSize a'
	stack <- fromPtr64 sp
	updatePtr64 sp $ asStack a' stack

primPop sp =
     do stack <- fromPtr64 sp
	let (a, stack') = splitStack stack
	sp' <- updatePtr64 sp stack'
	return (a, sp')


-- Stack frames
data FrameBasePtr64 h s t = FrameBasePtr64 h s t
instance HasSZ (FrameBasePtr64 h s t) where type SZ (FrameBasePtr64 h s t) = T64

getBPHandle :: FrameBasePtr64 h s t -> h
getBPHandle _ = undefined

getBPStack :: FrameBasePtr64 h s t -> s
getBPStack _ = undefined

makeFramePtr64 sp frame =
     do free (getPtrHandle sp)
        h' <- alloc
        return ( FrameBasePtr64 (getPtrHandle sp)
                                (assertStack $ getPtrData sp)
                                frame
               , Ptr64 h' (undefined :: Stack a' b')
               )

getStackFromFramePtr64 bp =
     do let h = getBPHandle bp
        realloc h
        return $ Ptr64 h (getBPStack bp)



-- Instructions
push src =
     do assertRegister src
	instr ( Push (arg src) )
        stack <- get rsp
        a' <- get src
        stack' <- primPush a' stack
        set rsp stack'

pop dst =
     do assertRegister dst
	instr ( Pop (arg dst) )
        stack <- get rsp
        (a', stack') <- primPop stack
        set dst a'
        set rsp stack'

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

