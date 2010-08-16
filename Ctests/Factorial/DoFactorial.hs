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
	isZero <- cmp rdi rbx
	sjne isZero doFactorial1 (sjmp allDone)

doFactorial1 = defun "doFactorial1" $
     do isCode
	loadInt 1 rbx
	isOne <- cmp rdi rbx
	sje isOne allDone (do
		mul rax rdi
		sub rbx rdi
		sjmp doFactorial1)

allDone = defun "allDone" $
     do isCode
	pop rbx
	ret

