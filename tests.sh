#!/bin/bash

# A script for doing a bunch of compiles

PC=$HOME/bin/pc

time $PC compile --outdir=Tests --outfile=TestArray.S Tests.TestArray
time $PC compile --outdir=Tests --outfile=TestCode.S Tests.TestCode
time $PC compile --outdir=Tests --outfile=TestStruct.S Tests.TestStruct

time $PC compile --outdir=Tests/C/Add --outfile=add2.S Tests.C.Add.Add2
time $PC compile --outdir=Tests/C/IntGate --outfile=changeDpl2.S Tests.C.IntGate.ChangeDpl2
time $PC compile --outdir=Tests/C/Factorial --outfile=doFactorial.S Tests.C.Factorial.DoFactorial

