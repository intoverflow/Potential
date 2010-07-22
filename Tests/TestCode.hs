{-# LANGUAGE
	NoMonomorphismRestriction,
	NoImplicitPrelude #-}
module Tests.TestCode where

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
     do isCode
	swap rax rbx
	ret

testMov = asCode "testMov" $
     do isCode
	mov rax rbx
	ret

testPopSwap = asCode "testPopSwap" $
     do isCode
	pop rax
	pop rbx
	swap rax rbx
	ret

testCmp = asCode "testCmp" $
     do isCode
	pop rax
	pop rbx
	rabxCmp <- cmp rax rbx
	sje rabxCmp doesNothing (do
		pop rax
		mov rax rbx
		ret)

doesNothing = asCode "doesNothing" $
     do isCode
	pop rbx
	mov rbx rax
	ret

testPrivLevelKernel = asCode "testPrivLevelKernel" $
     do isCode
	assertPrivLevelKernel
	ret

