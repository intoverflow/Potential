{-# LANGUAGE
	NoImplicitPrelude
	#-}
module Potential.Mov
	( mov ) where

import Potential.Core

mov src dst =
     do assertRegister src
	assertRegister dst
	instr $ Mov (arg src) (arg dst)
	d <- get src
	set dst d


