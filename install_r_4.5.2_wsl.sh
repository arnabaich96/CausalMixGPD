#!/bin/bash
# Script to install R 4.5.2 on Ubuntu WSL
# Run this in your WSL terminal: bash install_r_4.5.2_wsl.sh

set -e

echo "Installing R 4.5.2 on Ubuntu WSL..."

# Update package indices
echo "Step 1: Updating package indices..."
sudo apt update -qq

# Install helper packages
echo "Step 2: Installing helper packages..."
sudo apt install --no-install-recommends -y software-properties-common dirmngr

# Add the CRAN signing key
echo "Step 3: Adding CRAN signing key..."
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc

# Detect Ubuntu version and add appropriate CRAN repository
UBUNTU_VERSION=$(lsb_release -cs)
echo "Detected Ubuntu version: $UBUNTU_VERSION"
echo "Step 4: Adding CRAN repository..."
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu ${UBUNTU_VERSION}-cran40/" -y

# Update again to include CRAN packages
echo "Step 5: Updating package lists with CRAN repository..."
sudo apt update -qq

# Install R base
echo "Step 6: Installing R..."
sudo apt install --no-install-recommends -y r-base

# Install R development tools (for compiling packages)
echo "Step 7: Installing R development tools..."
sudo apt install --no-install-recommends -y r-base-dev

# Verify installation
echo "Step 8: Verifying R installation..."
R --version

echo ""
echo "Installation complete!"
echo "To verify the version, run: R --version"
echo "To start R, run: R"
