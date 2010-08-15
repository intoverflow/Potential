{-# LANGUAGE
	QuasiQuotes,
	TypeFamilies,
	NoMonomorphismRestriction #-}
module Potential.Arch.Amd64.Machine.IDT where

import Potential.Array
import Potential.Arch.Amd64.Machine.IntGate

reifyArray ast_InterruptGate [$array|

				InterruptDescriptionTable

		     0 divideError
		     1 reserved
		     2 nMI
		     3 breakpoint

		     4 overflow
		     5 boundRangeExceeded
		     6 invalidOpcode
		     7 noMathCoprocessor

		     8 doubleFault
		     9 coprocessorSegmentOverrun
		    10 invalidTSS
		    11 segmentNotPresent

		    12 stackSegmentFault
		    13 generalProtection
		    14 pageFault
		    15 reserved

		    16 floatingPointError
		    17 alignmentCheck
		    18 machineCheck
		    19 sIMDFloatingPoint

		    -- The rest are reserved, until interrupt 32, which is ours

		    32 myInterrupt
|]

