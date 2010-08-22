{-# LANGUAGE
	NoImplicitPrelude
	#-}
module Language.Potential.Mov
	( mov ) where

import Language.Potential.Core

mov src dst =
     do assertRegister src
	assertRegister dst
	instr $ Mov (arg src) (arg dst)
	d <- get src
	set dst d


