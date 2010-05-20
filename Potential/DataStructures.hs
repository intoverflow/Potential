{-# LANGUAGE
	NoImplicitPrelude #-}
module Potential.DataStructures 
	( shiftTo, andForget, orMerge
	, andIsolate, shiftFrom
	, setField, getField
	) where

import Prelude( Show(..), (++), ($) )

import Potential.Size
import Potential.PrimTypes
import Potential.MachineState
import Potential.PMonad
import Potential.Assembly
import Potential.Primitives
import Potential.Printing
import Potential.BuildDataStructures


loadPackage field structPtr into =
     do instr $ Ld (Deref2 (displacement field) (arg structPtr)) (arg into)
	struct <- get structPtr
	f <- getStruct field struct
	set into f

storePackage field structPtr from =
     do	instr $ Sto (arg from) (Deref2 (displacement field) (arg structPtr))
	struct <- get structPtr
	pkg'   <- get from
	struct' <- setStruct field pkg' struct
	-- structPtr' <- updatePtr64 struct struct'
	set structPtr struct'

shiftTo field s =
     do instr $ ShL (shiftBy field) (arg s)
	s' <- get s
	shifted <- shiftTyp field s'
	set s shifted

shiftFrom field s =
     do instr $ ShR (shiftBy field) (arg s)
	s' <- get s
	let shifted = shiftDownTyp field s'
	set s shifted

andForget field s =
     do instr $ And (forgetMask field) (arg s)
	s' <- get s
	let forgotten = forgetTyp field s'
	set s forgotten

andIsolate field s =
     do instr $ And (isolateMask field) (arg s)
	s' <- get s
	let isolated = isolateTyp field s'
	set s isolated

orMerge field with toUpdate =
     do instr $ Or (arg with) (arg toUpdate)
	with' <- get with
	toUpdate' <- get toUpdate
	let merged = orTyp field with' toUpdate'
	set toUpdate merged

setField field structPtr newVal temp =
     do comment ("updating field \"" ++ fname field ++
		 "\" at " ++ show (Deref3 $ arg structPtr) ++
		 " using value in " ++ show (arg newVal) ++
		 ", with " ++ show (arg temp) ++ " as a temp register")
	loadPackage field structPtr temp
	shiftTo field newVal
	andForget field temp
	orMerge field newVal temp
	storePackage field structPtr temp
	comment "update complete"


getField field structPtr retTo =
     do comment ("getting field \"" ++ fname field ++
		 "\" from " ++ show (arg structPtr))
	loadPackage field structPtr retTo
	andIsolate field retTo
	shiftFrom field retTo
	comment ("field retrieved into " ++ show (arg retTo))

