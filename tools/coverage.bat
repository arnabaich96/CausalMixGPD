@echo off
setlocal

rem Bounded coverage profile for local/CI reliability.
rem Set DPMIXGPD_COVERAGE_SUITE=historical85 manually for the long legacy run.
if "%DPMIXGPD_COVERAGE_SUITE%"=="" set "DPMIXGPD_COVERAGE_SUITE=bounded"
set "DPMIXGPD_COVERAGE_LEVEL=ci"
set "COVERAGE=1"
if "%DPMIXGPD_CI_COVERAGE_ONLY%"=="" set "DPMIXGPD_CI_COVERAGE_ONLY=0"
set "DPMIXGPD_SKIP_COVR_CAUSAL_BRANCHES=1"
set "DPMIXGPD_NO_PAUSE=1"

echo Running %DPMIXGPD_COVERAGE_SUITE% coverage pipeline...
echo Set DPMIXGPD_COVERAGE_SUITE=historical85 for the long legacy profile.

call "%~dp0run_rscript_doubleclick.bat" "%~dp0.Rscripts\coverage.R"
