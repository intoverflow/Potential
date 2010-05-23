{-# LANGUAGE
	NoImplicitPrelude #-}
module Potential
	( arg
	, rax, rbx, rcx, rdx, rsi, rdi
	, r08, r09, r10, r11, r12, r13, r14, r15
	, CB0(..), CB1(..), Ptr64, FrameBasePtr64, Int64, Stack
	, asm, renderFn, getType, getTypeOf
	, asCode, Function
	, comment, mov, push, pop, sjmp, scall, ret, enter, leave
	, cmp, sje
	, setField, getField, setFieldInArray, getFieldInArray
	, assertType, assertRegisterType, assertPtrIsValid
	, defineDataSize, dataSize, dataSizeT
	, constField, reservedField, field
	, mkStruct, defineStruct
	, HandleIsOpen, Free
	, (:<=), (:==), (:<)
	, (>>), (>>=), return, fail
	, C, HS, Allocator
	) where

import Potential.Array
import Potential.Assembly
import Potential.Bit
import Potential.BuildArray
import Potential.BuildDataStructures
import Potential.Constraints
import Potential.Core
import Potential.DataStructures
import Potential.Flow
import Potential.Functions
import Potential.Handles
import Potential.Integer
import Potential.MachineState
import Potential.Mov
import Potential.Pointer
import Potential.Printing
import Potential.Size
import Potential.Stack

import Potential.Machine.Flags

