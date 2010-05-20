{-# LANGUAGE
	NoMonomorphismRestriction,
	NoImplicitPrelude,
	FlexibleInstances,
	MultiParamTypeClasses,
	TypeFamilies,
	TemplateHaskell #-}
module Potential.Machine.IntGate where

import Prelude ( ($), (++), show, fromInteger )

import Potential.Size
import Potential.MachineState
import Potential.Primitives
import Potential.PrimTypes
import Potential.Functions
import Potential.PMonad

import Potential.BuildDataStructures
import Potential.DataStructures

import Potential.Machine.IntGateStruct

-- example:
-- :info InterruptGate
defineStruct intGate

data Present    = Present
data NotPresent = NotPresent
$(defineDataSize ''Present    1)
$(defineDataSize ''NotPresent 1)


-- rax contains Ptr64 to interrupt gate
-- rbx contains the new DPL
setDPL = asCode "setDPL" $
     do push rcx
	push rbx
	setField dpl rax rbx rcx
	pop rbx
	pop rcx
	ret

-- rax contains Ptr64 to interrupt gate
-- rax will contain the DPL
getDPL = asCode "getDPL" $
     do push rbx
	getField dpl rax rbx
	mov rbx rax
	pop rbx
	ret

-- rax contains Ptr64 to interrupt gate
-- rax will contain the offset(hi)
getOffset_hi = asCode "getOffset_hi" $
     do push rbx
	getField offset_hi rax rbx
	mov rbx rax
	pop rbx
	ret
