# Parallel test runner
# Usage: .\run_tests_parallel.ps1 [-MaxJobs 4]

param(
    [int]$MaxJobs = 4
)

$ErrorActionPreference = "Stop"
$VerbosePreference = "Continue"

# Navigate to package root
$packageRoot = Split-Path -Parent $PSScriptRoot
Set-Location $packageRoot

Write-Host "Running tests in parallel with max $MaxJobs jobs"

# R script for running tests
$testScript = @'
args <- commandArgs(trailingOnly = TRUE)
log_path <- args[1]

sink(log_path, split = TRUE)
cat("Starting test suite\n")
cat("Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

tryCatch({
    if (requireNamespace("devtools", quietly = TRUE)) {
        devtools::load_all(quiet = TRUE)
    } else {
        library(DPmixGPD)
    }
    
    result <- devtools::test(reporter = "summary")
    
    cat("\n\nTest suite completed\n")
    cat("Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
    
    # Exit with non-zero if there were failures
    if (any(result$failed > 0)) {
        quit(status = 1)
    }
    quit(status = 0)
}, error = function(e) {
    cat("\n\nTest suite failed with error:", e$message, "\n")
    quit(status = 1)
})
'@

$testScriptPath = Join-Path $packageRoot "run_tests.R"
Set-Content -Path $testScriptPath -Value $testScript

$logsDir = Join-Path $packageRoot "test_logs"
if (-not (Test-Path $logsDir)) {
    New-Item -ItemType Directory -Path $logsDir | Out-Null
}

$logPath = Join-Path $logsDir "tests_$(Get-Date -Format 'yyyyMMdd_HHmmss').log"
$rscriptPath = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"

Write-Host "Running tests..."
Write-Host "Log: $logPath"

$process = Start-Process -FilePath $rscriptPath -ArgumentList @($testScriptPath, $logPath) -Wait -PassThru -NoNewWindow

# Show results
if ($process.ExitCode -eq 0) {
    Write-Host "`n✓ All tests passed" -ForegroundColor Green
} else {
    Write-Host "`n✗ Tests failed" -ForegroundColor Red
    Write-Host "Check log for details: $logPath"
}

# Clean up
Remove-Item $testScriptPath -Force

exit $process.ExitCode
