#!/bin/bash

cd $(dirname "$0")

g++ -static-libgcc -static-libstdc++ -O3 -std=c++17 -s -fno-exceptions -fno-rtti -Wall -Wextra -Wpedantic 2019_day1_min_allocs.cpp -o day1_min_allocs
