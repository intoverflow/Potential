{-# LANGUAGE
	TemplateHaskell #-}
module Potential.Machine.FlagsStruct where

import Potential.BuildDataStructures
import Potential.PrimTypes

flags = mkStruct "FlagsRegister"
		   [ field 1 "cf"	-- carry flag
		   , $(constField 1) CB1
		   , field 1 "pf"
		   , $(constField 1) CB0
		   , field 1 "af"
		   , $(constField 1) CB0
		   , field 1 "zf"
		   , field 1 "sf"
		   , field 1 "tf"	-- trap
		   , field 1 "if"	-- interrupt enable
		   , field 1 "df"
		   , field 1 "of"
		   , field 2 "iopl"	-- current priv level
		   , $(constField 1) CB0	-- nested task
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

