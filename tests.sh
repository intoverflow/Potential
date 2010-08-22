#!/bin/bash

# A script for doing a bunch of compiles

function compile {
  time runhaskell "-package ghc" PC.Main $1
}

compile 'compile --outdir=Tests --outfile=TestArray.S Tests.TestArray'
compile 'compile --outdir=Tests --outfile=TestCode.S Tests.TestCode'
compile 'compile --outdir=Tests --outfile=TestStruct.S Tests.TestStruct'

compile 'compile --outdir=Tests/C/Add --outfile=add2.S Tests.C.Add.Add2'
compile 'compile --outdir=Tests/C/IntGate --outfile=changeDpl2.S Tests.C.IntGate.ChangeDpl2'
compile 'compile --outdir=Tests/C/Factorial --outfile=doFactorial.S Tests.C.Factorial.DoFactorial'

