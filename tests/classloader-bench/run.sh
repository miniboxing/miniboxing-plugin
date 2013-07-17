#!/bin/bash

# compile the source files here
echo ===============================================================================
echo RUNNING THE TEST PROJECT
echo ===============================================================================
echo Running miniboxing tester...
../../mb-scala -cp jars/*.jar:./vector-mb/:./binaries/tester-mb-bin/ miniboxing.classloader.test.TestMboxing
echo Running specialization tester...
../../mb-scala -cp jars/*.jar:./vector-spec/:./binaries/tester-spec-bin/ miniboxing.classloader.test.TestSpec

