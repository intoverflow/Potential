module Tests.TestArray where

import Potential
import Potential.Machine.IDT

testProjector = defun "testProjector" $
     do isMemRegion $ isCode
	proj_InterruptDescriptionTable_nMI rax rbx
	ret

testInjector = defun "testInjector" $
     do isMemRegion $ isCode
	nestMemoryRegion $
	     do lift $ scall doAlloc
		newInterruptDescriptionTable rbx
		inj_overflow_InterruptGate_0 r10 rax
		inj_overflow_InterruptGate_0 r10 rbx
	ret

doAlloc = defun "doAlloc" $
     do isMemRegion $ isCode
	newInterruptDescriptionTable rax
	ret

