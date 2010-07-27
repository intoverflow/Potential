module Ctests.IntGate.ChangeDpl where

{-
 compile --outdir=Ctests/IntGate --outfile=changeDpl.S Ctests.IntGate.ChangeDpl
-}

import Potential
import Potential.Machine.IntGate

_changeDpl = defun "_changeDpl" $
     do isMemSubRegion $ isMemRegion $ isCode
	comment "rdi is new dpl, rsi is *intDesc"
	push rax
	comment "get the first partial of the intDesc"
	proj_InterruptGate_0 rsi rax
	comment "set its dpl"
	inj_InterruptGate_0_dpl rdi rax
	comment "inject the updated partial into the intDesc"
	inj_InterruptGate_0 rax rsi
	pop rax
	ret

