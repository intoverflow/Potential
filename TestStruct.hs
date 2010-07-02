{-# LANGUAGE
	QuasiQuotes #-}
module TestStruct where

import Potential.DataStructure

fr = [$struct| FlagsRegister where
    cf :: 1     -- carry
    const 1
    pf :: 1     -- parity
    const 0
    af :: 1     -- adjust
    const 0
    zf :: 1     -- zero
    sf :: 1     -- sign
    tf :: 1     -- trap
    ief :: 1	-- interrupt enable
    df :: 1     -- direction
    ovf :: 1	-- overflow
    iopl :: 2   -- current priv level
    const 0     -- nested task, not supported in x86-64 so we make it 0
    const 0
    rf :: 1     -- resume
    const 0     -- virtual 8086, always zero in IA-32e
    ac :: 1     -- alignment check
    vif :: 1    -- virtual interrupt
    vip :: 1    -- virtual interrupt pending
    idf :: 1	-- identification
    reserved 10
    reserved 32
|]

ig = [$struct| InterruptGate where
    offset_lo :: 16
    segsel :: 16
    ist :: 3
    const 00
    const 000
    const 1110          -- identifies InterruptGate type
    const 0
    dpl :: 2
    p :: 1
    offset_mid :: 16
    offset_hi :: 32
    reserved 32
|]


