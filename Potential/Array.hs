{-# LANGUAGE
	NoImplicitPrelude #-}
module Potential.Array
	( setFieldInArray
	, getFieldInArray
	) where

import Prelude( Show(..), (++), ($), (+) )

import Potential.Size
import Potential.MachineState
import Potential.PMonad
import Potential.Assembly
import Potential.Primitives
import Potential.PrimTypes
import Potential.Printing

import Potential.BuildArray
import Potential.BuildDataStructures
import Potential.DataStructures

loadPackageFromArray cell field arrayPtr into =
     do instr $ Ld (Deref2 (displacement field + offset cell) (arg arrayPtr))
		   (arg into)
	array <- get arrayPtr
	cell' <- getCell cell array
	pkg <- getStruct field cell'
	set into pkg

storePackageInArray cell field arrayPtr from =
     do instr $ Sto (arg from)
		    (Deref2 (displacement field + offset cell) (arg arrayPtr))
	from' <- get from
	array <- get arrayPtr
	cell' <- getCell cell array
	f <- setStruct field from' cell'
	array' <- updateCell cell array f
	set arrayPtr array'

setFieldInArray cell field arrayPtr newVal temp =
     do comment $ "updating field \"" ++ fname field ++
		  "\" at " ++ show (Deref3 $ arg arrayPtr) ++
		  " using value in " ++ show (arg newVal) ++
		  ", with " ++ show (arg temp) ++ " as a temp register"
	loadPackageFromArray cell field arrayPtr temp
	shiftTo field newVal
	andForget field temp
	orMerge field newVal temp
	storePackageInArray cell field arrayPtr temp
	comment "update complete"

getFieldInArray cell field arrayPtr retTo =
     do comment $ "getting field \"" ++ fname field ++ "\" from " ++ show (arg arrayPtr)
	loadPackageFromArray cell field arrayPtr retTo
	andIsolate field retTo
	shiftFrom field retTo
	comment $ "field retrieved into " ++ show (arg retTo)


