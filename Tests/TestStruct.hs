module Tests.TestStruct where

import Potential

[$struct| FlagsRegister where
    cf :: 1     -- carry
    const 1
    pf :: 1     -- parity
    const 0
    af :: 1     -- adjust
    const 0
    zf :: 1     -- zero
    sf :: 1     -- sign
    tf :: 1     -- trap
    ief :: 1	-- interrupt enable
    df :: 1     -- direction
    ovf :: 1	-- overflow
    iopl :: 2   -- current priv level
    const 0     -- nested task, not supported in x86-64 so we make it 0
    const 0
    rf :: 1     -- resume
    const 0     -- virtual 8086, always zero in IA-32e
    ac :: 1     -- alignment check
    vif :: 1    -- virtual interrupt
    vip :: 1    -- virtual interrupt pending
    idf :: 1	-- identification
    reserved 10
    reserved 32
|]

[$struct| InterruptGateXX where
    offset_lo :: 16
    segsel :: 16
    ist :: 3
    const 0
    const 0
    const 000
    const 1110          -- identifies InterruptGate type
    const 0
    dpl :: 2
    p :: 1
    offset_mid :: 16
    offset_hi :: 32
    reserved 32
|]

[$struct_diagram|

			InterruptGate

    |31----------------------------------------------------0|
    |                      reserved                         | 12
    |-------------------------------------------------------|

    |31----------------------------------------------------0|
    |                      offset_hi                        | 8
    |-------------------------------------------------------|

    |-31------16-|-15-|14-13|-12-|11---8|7---5|-4-|-3-|2---0|
    | offset_mid |  p | dpl | 0  | 1110 | 000 | 0 | 0 | ist | 4
    |------------|----|-----|----|------|-----|---|---|-----|
				    (
				    (
				    ( this is the Type field,
				    ( set up for interrupt gate

    |31------------------------16|15-----------------------0|
    |           segsel           |          offset_lo       | 0
    |----------------------------|--------------------------|

|]

testProjector = defun "testProjector" $
     do isMemRegion $ isCode
	proj_InterruptGate_8 rax rbx
	ret

testField = defun "testField" $
     do isCode
	push rcx
	proj_InterruptGate_0_dpl rax rcx
	pop rcx
	ret

testInjectField = defun "testInjectField" $
     do isCode
	push rcx
	inj_InterruptGate_8_offset_hi rax rbx rcx
	inj_InterruptGate_0_ist r10 r11 rcx
	pop rcx
	pop r10
	push rcx
	inj_InterruptGate_0_dpl r10 r11 rcx
	pop rcx
	ret

testNew = defun "testNew" $
     do isCode
	withMemoryRegion $ evaluateTypes $
	     do newInterruptGate r11
		mov r10 r11
	ret

init = defun "init" $
     do isMemRegion $ isCode
	nestMemoryRegion $
	     do newInterruptGate rax
		pop r11
		inj_InterruptGate_0 r11 rax
		-- This below causes failure *hooray!*
		-- inj_InterruptGate_8 r12 rax
	ret

init2 = defun "init" $
     do isMemRegion $ isCode
	nestMemoryRegion $
	     do lift $ nestMemoryRegion $
			     do newInterruptGate rax
				pop r10
				inj_InterruptGate_0 r10 rax
		pop r10
		inj_InterruptGate_8 r10 rax
	ret

