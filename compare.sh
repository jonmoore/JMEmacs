#!/bin/bash

# Download latest JMEmacs from GitHub and prepare for directory comparison.
# Prints the path to the extracted upstream directory so you can run:
#   ediff-directories, meld, diff -r, etc.

REPO_URL="https://github.com/jonmoore/JMEmacs"
TEMP_DIR=$(mktemp -d)
ZIP_FILE="$TEMP_DIR/JMEmacs-latest.zip"

curl -sL "$REPO_URL/archive/refs/heads/master.zip" -o "$ZIP_FILE"

if [ ! -f "$ZIP_FILE" ]; then
    echo "Error: Failed to download repository" >&2
    rm -rf "$TEMP_DIR"
    exit 1
fi

unzip -q "$ZIP_FILE" -d "$TEMP_DIR"

# Find the extracted directory (GitHub adds branch name to folder)
EXTRACTED_DIR=$(find "$TEMP_DIR" -maxdepth 1 -type d -name "JMEmacs-*" | head -n 1)

if [ -z "$EXTRACTED_DIR" ]; then
    echo "Error: Could not find extracted directory" >&2
    rm -rf "$TEMP_DIR"
    exit 1
fi

echo "$EXTRACTED_DIR"
