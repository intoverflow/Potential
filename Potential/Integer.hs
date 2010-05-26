{-# LANGUAGE
	TypeFamilies,
	NoImplicitPrelude
	#-}
module Potential.Integer
	( Int64, assertInt64 ) where

import Prelude( ($) )

import Potential.Size
import Potential.Core

data Int64 = Int64
instance HasSZ Int64 where type SZ Int64 = T64

assertInt64 :: Int64 -> PState Instr c Composable s s s' ()
assertInt64 _ = composable $ return ()

