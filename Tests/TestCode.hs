module Tests.TestCode where

import Language.Potential

-- a useful macro
swap r1 r2 =
     do comment $ "swapping " ++ show (arg r1) ++ " with " ++ show (arg r2)
	push r1
	mov r2 r1
	pop r2
	comment ("swap complete")

testSwap = defun "testSawp" $
     do isCode
	swap rax rbx
	ret

testMov = defun "testMov" $
     do isCode
	mov rax rbx
	ret

testPopSwap = defun "testPopSwap" $
     do isCode
	pop rax
	pop rbx
	swap rax rbx
	ret

testCmp = defun "testCmp" $
     do isCode
	pop rax
	pop rbx
	compare rax rbx
		(JZ doesNothing)
		(do pop rax
		    mov rax rbx
		    ret)

doesNothing = defun "doesNothing" $
     do isCode
	pop rbx
	mov rbx rax
	ret

testPrivLevelKernel = defun "testPrivLevelKernel" $
     do isCode
	assertPrivLevelKernel
	ret

