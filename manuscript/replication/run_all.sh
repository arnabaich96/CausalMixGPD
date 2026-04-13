#!/bin/bash
# Find Rscript — checks PATH then common install locations
if command -v Rscript >/dev/null 2>&1; then
    RSCRIPT=Rscript
elif [ -x "/usr/local/bin/Rscript" ]; then
    RSCRIPT=/usr/local/bin/Rscript
elif [ -x "/opt/homebrew/bin/Rscript" ]; then
    RSCRIPT=/opt/homebrew/bin/Rscript
elif [ -x "/usr/bin/Rscript" ]; then
    RSCRIPT=/usr/bin/Rscript
else
    echo "Error: Rscript not found. Install R from https://cran.r-project.org"
    exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
"$RSCRIPT" "$SCRIPT_DIR/run_all.R"
