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

testSwap = defun $
     do isCode
	swap rax rbx
	ret

testMov = defun $
     do isCode
	mov rax rbx
	ret

testPopSwap = defun $
     do isCode
	pop rax
	pop rbx
	swap rax rbx
	ret

testCmp = defun $
     do isCode
	pop rax
	pop rbx
	rabxCmp <- cmp rax rbx
	sje rabxCmp doesNothing (do
		pop rax
		mov rax rbx
		ret)

doesNothing = defun $
     do isCode
	pop rbx
	mov rbx rax
	ret

testPrivLevelKernel = defun $
     do isCode
	assertPrivLevelKernel
	ret

