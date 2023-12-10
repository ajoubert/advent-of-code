#!/bin/bash

if ! command -v erlc &> /dev/null
then
    echo "erlc could not be found"
    echo "Use `nix-shell` install dependencies with nix"
    exit 1
fi

# Remove all .beam files
rm -f *.beam

# Compile all .erl files into .beam
erlc *.erl
