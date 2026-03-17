@echo off
call "%~dp0run_rscript.bat" "%~dp0.Rscripts\coverage.R" --upload %*
