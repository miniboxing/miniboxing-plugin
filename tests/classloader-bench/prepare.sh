#!/bin/bash

# prepare the specialized version of s.c.i.Vector
echo ===============================================================================
echo COMPILING VECTOR WITH MINIBOXING
echo ===============================================================================
if [ ! -d vector-mb ]
then
  mkdir vector-mb
  ../../mb-scalac -P:minibox:loader -P:minibox:hijack `find ../benchmarks/src/miniboxing/benchmarks/collection/ -name '*.scala'` -d vector-mb
else
  echo Skipping. Remove vector-mb to recompile.
fi

echo
echo ===============================================================================
echo COMPILING VECTOR WITH SPECIALIZATION
echo ===============================================================================
if [ ! -d vector-spec ]
then
  mkdir vector-spec
  ../../mb-scalac `find ../benchmarks/src/miniboxing/benchmarks/collection/ -name '*.scala'` -d vector-spec
else
  echo Skipping. Remove vector-spec to recompile.
fi

echo
echo ===============================================================================
echo COMPILING GENERIC VECTOR
echo ===============================================================================
if [ ! -d vector-nosp ]
then
  mkdir vector-nosp
  ../../mb-scalac -no-specialization `find ../benchmarks/src/miniboxing/benchmarks/collection/ -name '*.scala'` -d vector-nosp
else
  echo Skipping. Remove vector-nosp to recompile.
fi

# prepare the classloader jar
echo
echo ===============================================================================
echo PREPARING MINIBOXING CLASSLOADER AND RUNTIME
echo ===============================================================================
if [ ! -d jars ]
then
  mkdir jars
  cd ../../
  sbt miniboxing-classloader/assembly
  sbt miniboxing-runtime/assembly
  cd -
  cp ../../components/runtime/target/miniboxing-runtime-assembly-0.1-SNAPSHOT.jar jars/
  cp ../../components/classloader/target/miniboxing-classloader-assembly-0.1-SNAPSHOT.jar jars/
fi

# generated code size
echo
echo ===============================================================================
echo CODE SIZE COMPARISON
echo ===============================================================================
echo "Generic:     `du -shb vector-nosp/` (measured in bytes)"
echo "Miniboxed:   `du -shb vector-mb/`"
echo "Specialized: `du -shb vector-spec/`"

