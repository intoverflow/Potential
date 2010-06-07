{-# LANGUAGE
	NoMonomorphismRestriction,
	NoImplicitPrelude #-}
module TestMemory where

import Prelude ( ($), undefined, (++), show, fromInteger )

import Potential
import Potential.Machine.IntGate
import Potential.Pointer
import Potential.IxMonad

transmogrify :: InterruptGate a b c d e f g -> InterruptGate a a a a a a a
transmogrify _ = undefined

doAlloc =
     do comment "allocating a new intgate"
	myIntGate <- newPtr64 (undefined :: InterruptGate a b c d e f g)
	return myIntGate

makeIntGate =
     do modified <- nestMemoryRegion (smuggleFrom transmogrify doAlloc)
	return modified

testAlloc' = asCode "testAlloc'" $
     do withMemoryRegion $ makeIntGate >> return ()
	ret

