module Ctests.Factorial.DoFactorial where

{-
compile --outdir=Ctests/Factorial --outfile="doFactorial.S" Ctests.Factorial.DoFactorial
-}

import Potential

_doFactorial = defun "_doFactorial" $
     do isCode
	-- rdi is the arg
	-- we return in rax
	push rbx

	loadInt 0 rbx
	loadInt 0 rax
	compare rdi rbx
		(JZ allDone)
		(do loadInt 1 rax
		    loadInt 1 rbx
		    sjmp doFactorial1)

doFactorial1 = defun "doFactorial1" $
     do isCode
	compare rdi rbx
		(JZ allDone)
		(do mul rdi rax
		    sub rbx rdi
		    sjmp doFactorial1)

allDone = defun "allDone" $
     do isCode
	pop rbx
	ret

