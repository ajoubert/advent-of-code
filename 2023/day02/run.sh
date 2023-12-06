#!/bin/bash

if ! command -v erl &> /dev/null
then
    echo "erl could not be found"
    echo "Use `nix-shell` to install dependencies with nix"
    exit 1
fi

# Checks if no .beam file is present in current directory
if [ ! -f main.beam ]; then
  echo "Before running this script, please compile the code with:"
  echo "./compile.sh"
  exit 1
fi

if [ -z "$1" ] || { [ "$1" != "part1" ] && [ "$1" != "part2" ]; }; then
  echo "Usage: $0 <part1|part2> [./inputs/full.txt]"
  exit 1
fi

function_name=${1}

file_path=${2:-./inputs/full.txt}

erl -noshell -s main $function_name $file_path -s init stop
