#!/bin/bash

# compile the source files here
echo ===============================================================================
echo COMPILING THE TEST PROJECT
echo ===============================================================================
echo Compiling miniboxing tester...
rm -rf binaries
mkdir -p binaries/tester-mb-bin
../../mb-scalac -cp jars/*.jar:./vector-mb/ `find tester-mb/src/ -name "*.scala"` -d binaries/tester-mb-bin/
echo Compiling specialization tester...
mkdir binaries/tester-spec-bin
../../mb-scalac -cp jars/*.jar:./vector-spec/ `find tester-spec/src/ -name "*.scala"` -d binaries/tester-spec-bin/

