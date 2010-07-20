{-# LANGUAGE
	QuasiQuotes,
	NoMonomorphismRestriction,
	FlexibleContexts,
	NoImplicitPrelude,
	TypeFamilies #-}
module TestStruct where

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

testProjector = -- asCode "testProjector" $
     do proj_InterruptGate_8 rax rbx
	lift $ ret

testField = asCode "testField" $
     do proj_InterruptGate_0_dpl rax
	ret

testInjectField = asCode "testInjectField" $
     do inj_InterruptGate_8_offset_hi rax rbx
	inj_InterruptGate_0_ist r10 r11
	pop r10
	inj_InterruptGate_0_dpl r10 r11
	ret

testNew = asCode "testNew" $
     do withMemoryRegion $ evaluateTypes $
	     do newInterruptGate r11 []
		lift $ mov r10 r11
	ret

init = nestMemoryRegion $ \sr ->
     do newInterruptGate rax []
	lift $ pop r11
	inj_InterruptGate_0 r11 rax sr
	-- This below causes failure *hooray!*
	-- inj_InterruptGate_8 r12 rax sr

init2 = nestMemoryRegion $ \sr1 ->
     do nestMemoryRegion $ \sr2 ->
	     do newInterruptGate rax
		lift $ pop r10
		inj_InterruptGate_0 r10 rax sr2
	lift $ pop r10
	inj_InterruptGate_8 r10 rax sr1

