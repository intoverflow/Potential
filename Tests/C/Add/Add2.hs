module Tests.C.Add.Add2 where

import Language.Potential

_add2 = defun "_add2" $
     do isCode
	mov rdi rax
	add rsi rax
	ret

