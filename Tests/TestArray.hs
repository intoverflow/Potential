module Tests.TestArray where

import Potential
import Potential.Machine.IDT

testProjector = defun "testProjector" $
     do lift $ isCode
	proj_InterruptDescriptionTable_nMI rax rbx
	ret

testInjector = defun "testInjector" $
     do lift $ isCode
	nestMemoryRegion $
	     do -- scall doAlloc
		newInterruptDescriptionTable rax
		inj_overflow_InterruptGate_0 r10 rax
	ret

-- fails because of change in how newPtr64' works
-- see Potential.Pointer
doAlloc = defun "doAlloc" $
     do lift $ isCode
	newInterruptDescriptionTable rax
	ret

