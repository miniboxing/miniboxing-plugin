#!/bin/bash

for test in `ls *.scala`
do 
  flags=${test/scala/flags}
  echo "-P:minibox:Ygeneric-constructor-code " `cat $flags` >$flags 
done

