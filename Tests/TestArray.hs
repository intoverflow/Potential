{-# LANGUAGE
	NoImplicitPrelude,
	NoMonomorphismRestriction #-}
module Tests.TestArray where

import Potential
import Potential.Machine.IDT

testProjector = defun $
     do lift $ isCode
	proj_InterruptDescriptionTable_nMI rax rbx
	ret

testInjector = defun $
     do lift $ isCode
	nestMemoryRegion $ \sr ->
	     do scall doAlloc
		-- newInterruptDescriptionTable rax
		inj_overflow_InterruptGate_0 r10 rax sr
	ret

doAlloc = defun $
     do lift $ isCode
	newInterruptDescriptionTable rax
	ret

