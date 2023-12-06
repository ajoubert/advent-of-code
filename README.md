# Advent of code

> _Experience is the name everyone gives to their mistakes_ -- **Oscar Wilde**

[![](https://badgen.net/badge/instances/2%2F450/blue?label=⭐%20Stars)](https://adventofcode.com/events)
[![](https://badgen.net/badge/icon/Open%20in%20codespaces?icon=github&label)](https://codespaces.new/ajoubert/advent-of-code)

## Description

This is my attempts at the [Advent of Code](https://adventofcode.com/) challenge.
For each year, I will try to use a different language.

## Motivation

Mostly, I think it's **fun**. Programming as a hobby is fun and learning new
things is fun. The advent of code is just a good excuse to bring those two
topics together.

I tend to dislike the competitive approach one can choose to follow during this event, so you will not find in this repository any tooling to automatically submit my results or keep track of the time to completion. In fact, I will likely not even attempt to solve the problems when they are released, but rather whenver I have some free time.

## Dependencies

In order to keep system dependencies to a minimum, this project relies on the [Nix](https://nixos.org/nix/) package manager to provide all required dependencies for each language.

Therefore the only dependency required to run the code is to have Nix installed, insallation instructions can be found [here](https://nixos.org/download).
If you do not want to install Nix, read the `shell.nix` file of each day and install the required dependencies manually.

## Setup

Each year is in a separate folder, with a `README.md` file describing the language chosen and the setup required to run the code.

Within each year, each day is in a separate folder, with a `README.md` file
describing the challenge. Files containing the inputs are present within the 'inputs' folder of each day and are named 'part1.txt' and 'part2.txt'.

Each day also includes a `shell.nix` file which allows to use Nix to include all required dependencies.

## Usage

Before running any code, make sure to run `nix-shell` in the folder of the day you want to execute the code for.

While the languages differ per year, I decided to try and standardize the execution of the solutions. Each day includes a list of bash scripts to be executed.

```bash
$ ./compile.sh # Compiles the code if necessary
$ ./run.sh [part1|part2] <inputfile.txt> # Runs the code
$ ./test.sh # Runs the tests
$ ./watch.sh [part1|part2] <inputfile.txt> # Watches for code changes and re-compiles/executes when necessary
```

## Testing

Each module includes a `test.sh` script, but some languages may provide
a better way to run tests. See the `README.md` file of each year for more
information on the testing strategy for each language.
