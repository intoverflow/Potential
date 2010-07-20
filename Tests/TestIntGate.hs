{-# LANGUAGE
        NoImplicitPrelude #-}
module Tests.TestIntGate where

import Potential
import Potential.Machine.IntGate

testGetDPL = asCode "testGetDPL" $
     do pop rax
	-- scall getDPL
	ret

