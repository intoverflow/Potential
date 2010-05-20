{-# LANGUAGE
	TemplateHaskell #-}
module Potential.Machine.IDT where

import Potential.Size
import Potential.BuildArray

import Potential.Machine.IntGate

defineArray "IDT" (dataSize (undefined :: InterruptGate a1 a2 a3 a4 a5 a6 a7))
		    [ "intDivideError" , "intReserved"
		    , "intNMI" , "intBreakpoint"

		    , "intOverflow" , "intBoundRangeExceeded"
		    , "intInvalidOpcode" , "intNoMathCoprocessor"

		    , "intDoubleFault" , "intCoprocessorSegmentOverrun"
		    , "intInvalidTSS" , "intSegmentNotPresent"

		    , "intStackSegmentFault" , "intGeneralProtection"
		    , "intPageFault" , "intReserved2"

		    , "intFloatingPointError" , "intAlignmentCheck"
		    , "intMachineCheck" , "intSIMDFloatingPoint"

		    , "intReserved3" , "intReserved4"
		    , "intReserved5" , "intReserved6"
		    , "intReserved7" , "intReserved8"
		    , "intReserved9" , "intReserved10"
		    , "intReserved11" , "intReserved12"
		    , "intReserved13" , "intReserved14"

		    , "intMine"
		    ]

