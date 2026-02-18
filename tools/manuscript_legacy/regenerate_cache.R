# Script to regenerate cached fit objects
# Run this if you want to force recomputation instead of using cached fits

cat("This script will regenerate all cached fit objects.\n")
cat("Set USE_CACHE = FALSE in the knit.R script to use this script.\n\n")

# Note: To regenerate cache, set USE_CACHE = FALSE in the setup chunk, 
# then knit the manuscript with:
# R --slave --file=knit.R
#
# Or manually:
# setwd("vignettes/manuscript")
# source("regenerate_cache.R")

message("To regenerate all caches:")
message("  1. Edit knit.R and set USE_CACHE = FALSE")
message("  2. Run: R --slave --file=knit.R")
message("")
message("To clear cache and start fresh:")
message("  unlink('cache', recursive = TRUE)")
