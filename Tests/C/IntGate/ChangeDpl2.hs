module Tests.C.IntGate.ChangeDpl2 where

import Language.Potential
import Language.Potential.Arch.Amd64.Machine.IntGate
import Language.Potential.Arch.Amd64.Machine.IDT

_changeDpl2 = defun "_changeDpl2" $
     do isMemSubRegion $ isMemRegion $ isCode
	comment "rdi is new dpl, rsi is *intDesc"

	push rax

	comment "get the first partial of the intDesc"
	proj_InterruptGate_0 rsi rax

	comment "set its dpl"
	push rbx
	inj_InterruptGate_0_dpl rdi rax rbx
	pop rbx

	comment "inject the updated partial into the intDesc"
	inj_InterruptGate_0 rax rsi

	pop rax
	ret

_getDpl2 = defun "_getDpl2" $
     do isMemRegion $ isCode
	comment "rdi is *intDesc"
	comment "return the dpl into rax"
	proj_InterruptGate_0 rdi rax
	proj_InterruptGate_0_dpl rax rbx
	ret

_changeDoubleFaultOffsetHi2 = defun "_changeDoubleFaultOffsetHi2" $
     do isMemSubRegion $ isMemRegion $ isCode
	comment "rdi is new offset_hi, rsi is *IDT"

	comment "Getting the doubleFault cell into r12"
	lift $ proj_InterruptDescriptionTable_doubleFault rsi r12

	comment "Projecting 8-bytes in to rbx"
	proj_InterruptGate_8 r12 rbx

	comment "Upading offset_hi using r13 as a temp"
	inj_InterruptGate_8_offset_hi rdi rbx r13

	comment "Injecting our partial into the doubleFault"
	inj_doubleFault_InterruptGate_8 rbx rsi

	ret

_getDoubleFaultOffsetHi2 = defun "_getDoubleFaultOffsetHi2" $
     do isMemRegion $ isCode
	comment "return offset_hi into rax, rdi *IDT"
	
	comment "Getting the doubleFault cell into r12"
	proj_InterruptDescriptionTable_doubleFault rdi r12

	comment "Projecting 8-bytes in to rbx"
	proj_InterruptGate_8 r12 rax

	comment "Projecting offset_hi into rax"
	proj_InterruptGate_8_offset_hi rax rbx

	ret

