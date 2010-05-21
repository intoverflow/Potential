{-# LANGUAGE
	TemplateHaskell #-}
module Potential.Machine.FlagsStruct where

import Potential.BuildDataStructures
import Potential.PrimTypes

flags = mkStruct "FlagsRegister"
		   [ field 1 "cf"	-- carry
		   , $(constField 1) CB1
		   , field 1 "pf"	-- parity
		   , $(constField 1) CB0
		   , field 1 "af"	-- adjust
		   , $(constField 1) CB0
		   , field 1 "zf"	-- zero
		   , field 1 "sf"	-- sign
		   , field 1 "tf"	-- trap
		   , field 1 "if"	-- interrupt enable
		   , field 1 "df"	-- direction
		   , field 1 "of"	-- overflow
		   , field 2 "iopl"	-- current priv level
		   , $(constField 1) CB0	-- nested task, not in x86-64
		   , $(constField 1) CB0
		   , field 1 "rf"	-- resume
		   , $(constField 1) CB0	-- virtual 8086 (always zero in IA-32e
		   , field 1 "ac"	-- alignment check
		   , field 1 "vif"	-- virtual interrupt
		   , field 1 "vip"	-- virtual interrupt pending
		   , field 1 "id"	-- identification
		   , $(reservedField 10)
		   , $(reservedField 32)
		   ]

