#!/bin/bash

# Yes I probably should've extracted out the commonality of the scripts but eh

cd $(dirname "$0")
include_dir=$(pwd)

function run_year {
   year=$1
   day=$2
   cd ${year}
   if [ "$2" == "" ]; then
      for i in *.hs; do
         echo
         echo "${year}/$i"
         echo
         ghc -O3 -i"${include_dir}" "$i" -o "${i%.*}" && "${i%.*}" < "input/${i%.*}.txt"
      done
   else
      echo
      echo "${year}/day${day}.hs"
      echo
      ghc -O3 -i"${include_dir}" "day${day}.hs" -o "day${day}" && "./day${day}" < "input/day${day}.txt"

   fi
   cd ..
}

if [ "$#" == "2" ] || [ "$#" == "1" ]; then
   run_year "$@"
elif [ "$#" == "0" ]; then
   for i in 20*; do
      run_year $i
   done
fi
