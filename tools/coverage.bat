@echo off
setlocal

rem Historical high-coverage profile (target: 85%+)
set "DPMIXGPD_COVERAGE_SUITE=historical85"
set "DPMIXGPD_COVERAGE_LEVEL=ci"
set "COVERAGE=1"
set "DPMIXGPD_CI_COVERAGE_ONLY=1"
set "DPMIXGPD_SKIP_COVR_CAUSAL_BRANCHES=1"
set "DPMIXGPD_NO_PAUSE=1"

echo Running historical85 coverage pipeline...
echo This can take a long time on local machines.

call "%~dp0run_rscript_doubleclick.bat" "%~dp0.Rscripts\coverage.R"
