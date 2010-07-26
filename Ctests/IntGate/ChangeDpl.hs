module Ctests.IntGate.ChangeDpl where

import Potential
import Potential.Machine.IntGate

_changeDpl = defun "_changeDpl" $
     do lift $ isCode
	comment "rdi is new dpl, rsi is *intDesc"
	nestMemoryRegion $ \sr ->
	     do push rax
		comment "get the first partial of the intDesc"
		proj_InterruptGate_0 rsi rax
		comment "set its dpl"
		inj_InterruptGate_0_dpl rdi rax
		comment "inject the updated partial into the intDesc"
		inj_InterruptGate_0 rax rsi sr
		pop rax
	ret

