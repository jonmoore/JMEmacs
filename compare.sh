#!/bin/bash

# Compare current folder with latest JMEmacs from GitHub

REPO_URL="https://github.com/jonmoore/JMEmacs"
TEMP_DIR=$(mktemp -d)
ZIP_FILE="$TEMP_DIR/JMEmacs-latest.zip"

curl -L "$REPO_URL/archive/refs/heads/master.zip" -o "$ZIP_FILE"

if [ ! -f "$ZIP_FILE" ]; then
    echo "Error: Failed to download repository"
    rm -rf "$TEMP_DIR"
    exit 1
fi

unzip -q "$ZIP_FILE" -d "$TEMP_DIR"

# Find the extracted directory (GitHub adds branch name to folder)
EXTRACTED_DIR=$(find "$TEMP_DIR" -maxdepth 1 -type d -name "JMEmacs-*" | head -n 1)

if [ -z "$EXTRACTED_DIR" ]; then
    echo "Error: Could not find extracted directory"
    rm -rf "$TEMP_DIR"
    exit 1
fi

# Compare directories, excluding common files to ignore
diff -rq --exclude=".git" --exclude="*.elc" --exclude="personal-autoloads.el" \
    "$EXTRACTED_DIR" . | sed "s|$EXTRACTED_DIR|REPO|g"

diff -r --exclude=".git" --exclude="*.elc" --exclude="personal-autoloads.el" \
    "$EXTRACTED_DIR" . | sed "s|$EXTRACTED_DIR|REPO|g"

rm -rf "$TEMP_DIR"
