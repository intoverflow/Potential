module Tests.TestArray where

import Potential
import Potential.Machine.IDT

testProjector = defun "testProjector" $
     do lift $ isCode
	proj_InterruptDescriptionTable_nMI rax rbx
	ret

testInjector = defun "testInjector" $
     do lift $ isCode
	nestMemoryRegion $ \sr ->
	     do scall doAlloc
		-- newInterruptDescriptionTable rax
		inj_overflow_InterruptGate_0 r10 rax sr
	ret

doAlloc = defun "doAlloc" $
     do lift $ isCode
	newInterruptDescriptionTable rax
	ret

