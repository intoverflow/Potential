module Tests.TestFailure where

import Potential
import Potential.Flow (primCondJmp)
import Potential.Assembly (body)

testCmpFails = defun "testCmpFails" $
     do isCode
	pop rax
	pop rbx
	pop rcx
	rabxCmp <- cmp rax rbx
	racxCmp <- cmp rax rcx
	sje rabxCmp doesNothing (do -- should cause trouble
		pop rax
		mov rax rbx
		ret)

doesNothing = defun "doesNothing" $
     do isCode
	pop rbx
	mov rbx rax
	ret

