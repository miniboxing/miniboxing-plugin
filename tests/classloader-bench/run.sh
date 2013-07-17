#!/bin/bash

# compile the source files here
echo ===============================================================================
echo RUNNING THE TEST PROJECT
echo ===============================================================================
echo "Running miniboxing tester... (use $0 -Dclassloader.debug=true for miniboxing"
echo "classloader debug print and $0 -J-verbose to log all classloading activity"
../../mb-scala $@ -cp jars/miniboxing-classloader-assembly-0.1-SNAPSHOT.jar:jars/miniboxing-runtime-assembly-0.1-SNAPSHOT.jar:./vector-mb/:./binaries/tester-mb-bin/ miniboxing.classloader.test.TestMboxing
echo Running specialization tester...
../../mb-scala $@ -cp jars/miniboxing-classloader-assembly-0.1-SNAPSHOT.jar:jars/miniboxing-runtime-assembly-0.1-SNAPSHOT.jar:./vector-spec/:./binaries/tester-spec-bin/ miniboxing.classloader.test.TestSpec

