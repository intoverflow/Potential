{-# LANGUAGE
	NoMonomorphismRestriction,
	NoImplicitPrelude,
	FlexibleInstances,
	MultiParamTypeClasses,
	TypeFamilies,
	TemplateHaskell,
	QuasiQuotes #-}
module Potential.Machine.IntGate where

import Prelude ( ($), (++), show, fromInteger )

import Potential.Size
import Potential.Core
import Potential.Stack
import Potential.Flow
import Potential.Mov

import Potential.DataStructures

import Potential.Functions

-- example:
-- :info InterruptGate
[$struct| InterruptGate where
    offset_lo :: 16
    segsel :: 16
    ist :: 3
    const 00
    const 000
    const 1110		-- identifies InterruptGate type
    const 0
    dpl :: 2
    p :: 1
    offset_mid :: 16
    offset_hi :: 32
    reserved 32
|]

data Present    = Present
data NotPresent = NotPresent
defineDataSize ''Present    1
defineDataSize ''NotPresent 1


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
