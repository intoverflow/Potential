{-# LANGUAGE
	NoImplicitPrelude,
	MultiParamTypeClasses,
	FlexibleInstances,
	TypeFamilies,
	TemplateHaskell #-}
module Potential.Machine.Flags where

import Prelude (fromInteger)

import Potential.BuildDataStructures
import Potential.MachineState
import Potential.Primitives
import Potential.PrimTypes
import Potential.Size
import Potential.PMonad

import Potential.Machine.FlagsStruct

-- example:
-- :info FlagsRegister
defineStruct flags

data PrivLevelUser   = PrivLevelUser
data PrivLevelKernel = PrivLevelKernel
defineDataSize ''PrivLevelUser   2
defineDataSize ''PrivLevelKernel 2

assertPrivLevelKernel =
     do fl <- get rflags
	assertSameType (proj_iopl fl) PrivLevelKernel

