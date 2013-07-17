#!/bin/bash

# prepare the specialized version of s.c.i.Vector
echo ===============================================================================
echo COMPILING WITH MINIBOXING
echo ===============================================================================
if [ ! -d vector-mb ]
then
  mkdir vector-mb
  ../../mb-scalac -P:minibox:loader -P:minibox:hijack `find ../benchmarks/src/miniboxing/benchmarks/collection/ -name '*.scala'` -d vector-mb
fi

echo ===============================================================================
echo COMPILING WITH SPECIALIZATION
echo ===============================================================================
if [ ! -d vector-spec ]
then
  mkdir vector-spec
  ../../mb-scalac `find ../benchmarks/src/miniboxing/benchmarks/collection/ -name '*.scala'` -d vector-spec
fi

# prepare the classloader jar
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
