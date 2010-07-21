{-# LANGUAGE
	NoImplicitPrelude,
	NoMonomorphismRestriction #-}
module Tests.TestArray where

import Potential
import Potential.Machine.IDT

testProjector =
     do proj_InterruptDescriptionTable_nMI rax rbx
	ret

testInjector =
     do nestMemoryRegion $ \sr ->
	     do -- scall doAlloc
		newInterruptDescriptionTable rax
		inj_overflow_InterruptGate_0 r10 rax sr
	ret

doAlloc = asCode "doAlloc" $
     do newInterruptDescriptionTable rax
	ret

