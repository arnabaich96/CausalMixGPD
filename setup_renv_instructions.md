# Setting up renv with R 4.5.2 on Ubuntu WSL

## Prerequisites
1. R 4.5.2 must be installed in WSL (see `install_r_4.5.2_wsl.sh`)

## Quick Setup (Automated)

Run this in your WSL terminal from the project directory:

```bash
bash setup_renv_wsl.sh
```

## Manual Setup (Step-by-step)

### 1. Open WSL Terminal
Open your Ubuntu WSL terminal and navigate to the project directory:
```bash
cd "/mnt/d/OneDrive - Florida State University/MyFSU_OneDrive/R-Codes/DPMGPD_package/DPmixGPD"
```

### 2. Verify R Version
```bash
R --version
```
Should show: `R version 4.5.2`

### 3. Install renv Package (if not already installed)
```bash
Rscript -e "install.packages('renv', repos = 'https://cloud.r-project.org')"
```

### 4. Activate renv
The `.Rprofile` file should auto-activate renv when you start R, but you can also activate manually:
```bash
Rscript -e "source('renv/activate.R')"
```

### 5. Restore Packages
Restore all packages from `renv.lock`:
```bash
Rscript -e "renv::restore()"
```

This will install all the packages specified in the lockfile. This may take 10-30 minutes depending on your internet speed.

### 6. Verify Setup
Check renv status:
```bash
Rscript -e "renv::status()"
```

## Using renv

### In RStudio or R Terminal
1. Open R in the project directory
2. renv should auto-activate (you'll see a message)
3. If not, run: `source('renv/activate.R')`

### Common renv Commands
```r
# Check status
renv::status()

# Install a new package
install.packages("package_name")
renv::snapshot()  # Update lockfile

# Update packages
renv::update()

# Restore packages
renv::restore()

# Check which R version is expected
renv::settings$r.version()
```

## Troubleshooting

### Issue: R version mismatch
If you see warnings about R version mismatch:
- Make sure R 4.5.2 is installed: `R --version`
- renv will work with different R versions, but some packages may need recompilation

### Issue: Package installation fails
- Check internet connection
- Try: `renv::restore(rebuild = TRUE)` to rebuild packages
- Check for system dependencies (may need `r-base-dev`)

### Issue: renv not activating
- Check `.Rprofile` exists and contains `source("renv/activate.R")`
- Manually activate: `source("renv/activate.R")`

## Notes
- The `renv.lock` file specifies R 4.5.2
- renv creates a local library in `renv/library/`
- Packages are isolated to this project
- The lockfile ensures reproducible environments
