{-# LANGUAGE
	NoImplicitPrelude #-}
module TestStruct where

import Potential
import Potential.Machine.IDT

testProjector =
     do proj_InterruptDescriptionTable_nMI rax rbx
	lift $ ret

