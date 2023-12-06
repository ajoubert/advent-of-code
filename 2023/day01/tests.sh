#!/usr/bin/env bats

if ! command -v erl &> /dev/null
then
    echo "erl could not be found"
    echo "Use `nix-shell` to use nix and install dependencies"
    exit
fi

# Checks if no .beam file is present in current directory
if [ ! -f main.beam ]; then
  echo "Before running this script, please compile the code with:"
  echo "./compile.sh"
  exit 1
fi

@test "Part 1" {
  run ./run.sh part1 inputs/part1.example.txt
  # Actual output contains carriage returns, this extra line removes them
  # Found with `command | oc -c`
  result=$(echo $output | tr -d '\r')
  [ "$status" -eq 0 ]
  [ "$result" = "142" ]
}

@test "Part 2" {
  run ./run.sh part2 inputs/part2.example.txt
  result=$(echo $output | tr -d '\r')
  [ "$status" -eq 0 ]
  [ "$result" = "281" ]
}
