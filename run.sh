#!/bin/bash

if [ "$#" -ne "1" ]; then
   for i in *.hs; do
      echo
      echo "$i"
      echo
      runhaskell "$i" < "input_${i%.*}.txt"
   done
else
   echo
   echo "day$1.hs"
   echo
   runhaskell day$1.hs < "input_day$1.txt"
fi
