@echo off
setlocal

:: Allow unattended/background runs by setting CMGPD_NO_PAUSE=1
set "DO_PAUSE=1"
if /I "%CMGPD_NO_PAUSE%"=="1" set "DO_PAUSE=0"

:: Use Rscript from PATH if available
where Rscript >nul 2>&1
if %ERRORLEVEL% equ 0 (
    set "RSCRIPT=Rscript"
    goto run
)

:: Search Program Files\R for the newest installed version
for /f "delims=" %%i in ('dir /b /ad /o-n "C:\Program Files\R" 2^>nul') do (
    if exist "C:\Program Files\R\%%i\bin\Rscript.exe" (
        set "RSCRIPT=C:\Program Files\R\%%i\bin\Rscript.exe"
        goto run
    )
)

echo Error: R not found. Install R from https://cran.r-project.org
if "%DO_PAUSE%"=="1" pause
exit /b 1

:run
"%RSCRIPT%" "%~dp0run_all.R"
if %ERRORLEVEL% neq 0 echo.& echo run_all.R failed. See output above.
if "%DO_PAUSE%"=="1" pause
