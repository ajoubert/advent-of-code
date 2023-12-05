#!/bin/bash

if [ -z "$1" ] || { [ "$1" != "part1" ] && [ "$1" != "part2" ]; }; then
  echo "Usage: $0 <part1|part2> [./inputs/full.txt]"
  exit 1
fi

if [ "$1" = "part1" ]; then
  default_file=./inputs/part1.example.txt
else
  default_file=./inputs/part2.example.txt
fi

file_path=${2:-$default_file}

find . -type f | entr sh -c "./compile.sh; ./run.sh $1 $file_path"
