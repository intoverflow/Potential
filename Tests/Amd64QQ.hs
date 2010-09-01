{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Tests.Amd64QQ where

import Language.Potential
import Prelude (undefined)

-- |A test of the amd64 quasi-quoter
qqTest :: $( [$amd64| rax : Int |] )
qqTest = undefined

