#!/bin/bash

if [ -z "$DEFAULT_GPG_KEY" ]; then
    echo "Please set DEFAULT_GPG_KEY environment variable"
    echo "If you use my dotfiles, see .config/shell/private.sh"
    exit 1
fi

# Find and encrypt README.md files in yyyy/dayxx/ directories
find ./ -regextype posix-extended -type f -regex ".*/[0-9]{4}/day[0-9]{2}/README.md" -print0 | while IFS= read -r -d '' file; do
    echo "Encrypting $file"
    gpg --yes --batch --recipient "$DEFAULT_GPG_KEY" -e "$file"
done

find ./ -regextype posix-extended -type f -regex ".*/[0-9]{4}/day[0-9]{2}/inputs/full.txt" -print0 | while IFS= read -r -d '' file; do
    echo "Encrypting $file"
    gpg --yes --batch --recipient "$DEFAULT_GPG_KEY" -e "$file"
done

