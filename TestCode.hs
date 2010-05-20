{-# LANGUAGE
	NoMonomorphismRestriction,
	NoImplicitPrelude #-}
module TestCode where

import Prelude ( ($), undefined, (++), show, fromInteger )

import Potential

import Potential.Machine.Flags
import Potential.Machine.IntGate
import Potential.Machine.IDT

-- a useful macro
swap r1 r2 =
     do comment $ "swapping " ++ show (arg r1) ++ " with " ++ show (arg r2)
	push r1
	mov r2 r1
	pop r2
	comment ("swap complete")

testSetDPL = asCode "testSetDPL" $
     do assumeType rbx (undefined :: PrivLevelUser)
	scall setDPL
	ret

testSetDPL2 = asCode "testSetDPL2" $
     do assumeType rbx (undefined :: PrivLevelUser)
	mov rax r10
	scall setDPL
	comment "Now rax has PrivLevelUser, r10 has unknown dpl"
	swap rax r10
	comment "Now rax has unknown dpl, r10 has PrivLevelUser"
	pop rbx
	assumeType rbx (undefined :: PrivLevelKernel)
	scall setDPL
	comment "Now rax has PrivLevelKernel, r10 has PrivLevelUser, but it's the same ptr"
	ret

test1 = asCode "test1" $
     do pop rax
	pop rbx
	-- # cmp rax rbx
	pop rbx
	-- # je rbx
	swap rax rbx
	ret

test2 = asCode "test2" $
     do enter (undefined :: InterruptGate offset_lo segsel ist dpl p offset_mid offset_hi)
	leave
	ret

{- Fails with unable to match types PrivLevelKernel and Ptr64 (InterruptGate ...)
test3 = asCode "test3" $
     do assumeType rbx (undefined :: PrivLevelKernel)
	push rbx
	pop rax
	sjmp setDPL
-}

test4 = asCode "test4" $
     do pop rax
	pop rbx
	scall setDPL
	ret

{- Fails with unable to match types PrivLevelKernel and Ptr64 (InterruptGate...)
test5 = asCode "test5" $
     do assumeType rbx (undefined :: PrivLevelKernel)
	push rbx
	pop rax
	scall setDPL
	ret
-}

testIDT = asCode "testIDT" $
     do push rcx
	push rbx
	setFieldInArray intOverflow dpl rax rbx rcx
	pop rbx
	pop rcx
	ret

testIDT2 = asCode "testIDT2" $
     do getFieldInArray intOverflow dpl rax rbx
	ret


-- the only way to know that this isn't failing is to try executing getType
testTypeFailure = asCode "testTypeFailure" $
     do assumeType rax (undefined :: InterruptGate a1 a2 a3 a4 a5 a6 a7)
	push rax
	pop rax
	ret

testCall = asCode "testCall" $
     do scall testIDT2
	ret

