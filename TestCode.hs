{-# LANGUAGE
	NoMonomorphismRestriction,
	NoImplicitPrelude #-}
module TestCode where

import Prelude ( ($), undefined, (++), show, fromInteger )

import Potential
import Potential.Flow (primCondJmp)
import Potential.Assembly (body)

-- a useful macro
swap r1 r2 =
     do comment $ "swapping " ++ show (arg r1) ++ " with " ++ show (arg r2)
	push r1
	mov r2 r1
	pop r2
	comment ("swap complete")

testSwap = asCode "testSwap" $
     do swap rax rbx
	ret

testMov = asCode "testMov" $
     do mov rax rbx
	ret

testPopSwap = asCode "testPopSwap" $
     do pop rax
	pop rbx
	swap rax rbx
	ret

testCmp = asCode "testCmp" $
     do pop rax
	pop rbx
	rabxCmp <- cmp rax rbx
	sje rabxCmp doesNothing (do
		pop rax
		mov rax rbx
		ret)

doesNothing = asCode "doesNothing" $
     do pop rbx
	mov rbx rax
	ret

testPrivLevelKernel = asCode "testPrivLevelKernel" $
     do assertPrivLevelKernel
	ret

