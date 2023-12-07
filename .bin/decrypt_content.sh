#!/bin/bash

# Find and decrypt README.md.gpg files in yyyy/dayxx/ directories
find ./ -regextype posix-extended -type f -regex ".*/[0-9]{4}/day[0-9]{2}/README.md.gpg" -print0 | while IFS= read -r -d '' file; do
    gpg --yes --batch -o "${file%.gpg}" -d "$file"
done

find ./ -regextype posix-extended -type f -regex ".*/[0-9]{4}/day[0-9]{2}/inputs/full.txt.gpg" -print0 | while IFS= read -r -d '' file; do
    gpg --yes --batch -o "${file%.gpg}" -d "$file"
done

