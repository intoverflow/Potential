#!/bin/bash

# A script for doing a bunch of compiles

function compile {
  time runhaskell "-package ghc" PC.Main $1
}

compile 'compile Tests.TestArray'
compile 'compile Tests.TestCode'
compile 'compile Tests.TestStruct'

compile 'compile --outdir=Ctests/Add --outfile=add2.S Ctests.Add.Add2'
compile 'compile --outdir=Ctests/IntGate --outfile=changeDpl2.S Ctests.IntGate.ChangeDpl2'
compile 'compile --outdir=Ctests/Factorial --outfile="doFactorial.S" Ctests.Factorial.DoFactorial'

