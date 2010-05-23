{-# LANGUAGE
	TemplateHaskell #-}
module Potential.Machine.IntGateStruct where

import Potential.BuildDataStructures
import Potential.Bit

intGate = mkStruct "InterruptGate"
		   [ (field 16 "offset_lo")
		   , (field 16 "segsel")
		   , (field 3 "ist")
		   , ($(constField 2) CB0 CB0)
		   , ($(constField 3) CB0 CB0 CB0)
		   , ($(constField 4) CB1 CB1 CB1 CB0)	-- defines Interrupt Gate type
		   , ($(constField 1) CB0)
		   , (field 2  "dpl")
		   , (field 1  "p")
		   , (field 16 "offset_mid")
		   , (field 32 "offset_hi")
		   , ($(reservedField 32))
		   ]

