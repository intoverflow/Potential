{-# LANGUAGE
	TypeFamilies,
	NoImplicitPrelude
	#-}
module Potential.Integer
	( Int64, assertInt64, add ) where

import Prelude( ($) )

import Potential.Size
import Potential.Core

data Int64 = Int64
instance HasSZ Int64 where type SZ Int64 = T64

assertInt64 :: Int64 -> Code c Unmodeled x x ()
assertInt64 _ = unmodeled $ return ()

add r1 r2 =
     do a <- get r1
	b <- get r2
	assertInt64 a
	assertInt64 b
	instr $ Add (arg r1) (arg r2)

