#!/bin/bash

for test in `ls *.scala`
do 
  flags=${test/scala/flags}
  warn=`cat $flags 2>/dev/null | grep "\\-P\\:minibox\\:warn"`
  if [ "$warn" != "" ]
  then
    sed 's/-P:minibox:warn//' -i $flags
    echo $flags: `cat $flags`
  else
    echo "-P:minibox:warn-off" `cat $flags` >$flags 
  fi
done

