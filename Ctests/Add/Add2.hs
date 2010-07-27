module Ctests.Add.Add2 where

{-
compile --outdir=Ctests/Add --outfile="add2.S" Ctests.Add.Add2
-}

import Potential

_add2 = defun "_add2" $
     do isCode
	mov rdi rax
	add rsi rax
	ret

