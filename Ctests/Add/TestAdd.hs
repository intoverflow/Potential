module Ctests.Add.TestAdd where

import Potential

_add2 = defun "_add2" $
     do isCode
	mov rdi rax
	add rsi rax
	ret

