#!/bin/bash
# Script to set up renv with R 4.5.2 on Ubuntu WSL
# Run this in your WSL terminal from the project directory

set -e

echo "Setting up renv with R 4.5.2..."

# Check if R is installed
if ! command -v R &> /dev/null; then
    echo "ERROR: R is not installed. Please install R 4.5.2 first."
    echo "Run: bash install_r_4.5.2_wsl.sh"
    exit 1
fi

# Check R version
R_VERSION=$(R --version | head -n 1 | grep -oP 'R version \K[0-9]+\.[0-9]+\.[0-9]+')
echo "Detected R version: $R_VERSION"

if [[ "$R_VERSION" != "4.5.2" ]]; then
    echo "WARNING: R version is $R_VERSION, but lockfile expects 4.5.2"
    echo "You may need to install R 4.5.2. Continue anyway? (y/n)"
    read -r response
    if [[ ! "$response" =~ ^[Yy]$ ]]; then
        exit 1
    fi
fi

# Navigate to project directory (assuming script is in project root)
PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$PROJECT_DIR"

echo "Project directory: $PROJECT_DIR"

# Check if renv is already initialized
if [ -d "renv" ]; then
    echo "renv directory found. Activating renv..."
    
    # Activate renv
    source renv/activate.R 2>/dev/null || {
        echo "Activating renv in R session..."
    }
    
    # Install renv package if not available
    echo "Checking renv package..."
    Rscript -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv', repos = 'https://cloud.r-project.org')"
    
    # Restore packages from lockfile
    echo "Restoring packages from renv.lock..."
    echo "This may take a while..."
    Rscript -e "renv::restore()"
    
    echo ""
    echo "renv setup complete!"
    echo ""
    echo "To use renv in R:"
    echo "  1. Open R in this directory: R"
    echo "  2. renv should auto-activate (check for 'renv' in your prompt)"
    echo "  3. If not, run: source('renv/activate.R')"
    echo ""
    echo "To verify packages are installed:"
    echo "  Rscript -e \"renv::status()\""
    
else
    echo "renv directory not found. Initializing renv..."
    Rscript -e "renv::init()"
    echo "renv initialized. You may need to restore packages:"
    echo "  Rscript -e \"renv::restore()\""
fi
