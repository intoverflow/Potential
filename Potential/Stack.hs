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
import Potential.Assembly

import Potential.Core


-- Stacks
data Stack a b = Stack a b
instance HasSZ (Stack a b) where type SZ (Stack a b) = T64

assertStack :: Stack a b -> Stack a b
assertStack = id

splitStack :: Stack a (Stack a' b') -> (a, Stack a' b')
splitStack _ = (undefined, undefined)

asStack :: a -> b -> Stack a b
asStack _ _ = undefined

assertPushableSize :: (SZ a' :<=? T64) c => a' -> Code c x x Composable ()
assertPushableSize _ = return ()

primPush a' sp =
     do assertPushableSize a'
	return $ asStack a' sp

primPop :: IxMonad m
	=> Stack t (Stack a' b')
	-> m x x Composable (t, Stack a' b')
primPop sp =
     do let (a, sp') = splitStack sp
	return (a, sp')


-- Stack frames
data FrameBasePtr64 h s t = FrameBasePtr64 s t
instance HasSZ (FrameBasePtr64 h s t) where type SZ (FrameBasePtr64 h s t) = T64

getBPStack :: FrameBasePtr64 h s t -> s
getBPStack _ = undefined

{-
makeFramePtr64 :: IxMonad m
		=> Ptr64 h (Stack a b)
		-> t
		-> m x x Composable (FrameBasePtr64 h (Ptr64 h (Stack a b)) t,
				     Ptr64 h (Stack a' b'))
-}
makeFramePtr64 sp frame =
     do let sp' = undefined :: Stack a' b'
        return ( FrameBasePtr64 sp frame, sp')

getStackFromFramePtr64 bp = return $ getBPStack bp



-- Instructions
push src =
     do assertRegister src
	instr $ Push (arg src)
        stack <- get rsp
        a'    <- get src
        stack' <- primPush a' stack
        set rsp stack'

pop dst =
     do assertRegister dst
	instr $ Pop (arg dst)
        sp <- get rsp
        (a', sp') <- primPop sp
        set dst a'
        set rsp sp'

enter frame =
     do let l = fromIntegral $ toInt $ sz frame
	lift $ instr $ Enter l
	baseptr <- lift $ get rbp
	stack   <- lift $ get rsp
	stack'  <- lift $ primPush baseptr stack
	(baseptr', stack'') <- makeFramePtr64 stack' frame
	lift $ set rbp baseptr'
	lift $ set rsp stack''

leave =
     do instr Leave
	baseptr <- get rbp
	stack <- getStackFromFramePtr64 baseptr
	(baseptr', stack') <- primPop stack
	set rbp baseptr'
	set rsp stack'

