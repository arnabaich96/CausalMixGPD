@echo off
setlocal enabledelayedexpansion

if "%~1"=="" (
  echo Usage: run_rscript_doubleclick.bat path\to\script.R [args...]
  pause
  exit /b 2
)

for %%I in ("%~1") do set "SCRIPT=%%~fI"
shift

if not "%~1"=="" (
  for %%I in ("%~1") do set "FIRST_ARG=%%~fI"
  if /I "%FIRST_ARG%"=="%SCRIPT%" shift
)

if not exist "%SCRIPT%" (
  echo Script not found:
  echo   %SCRIPT%
  pause
  exit /b 2
)

set "RSCRIPT_EXE="

for /f "tokens=*" %%V in ('dir /b /ad "%ProgramFiles%\R\R-*" 2^>nul ^| %SystemRoot%\system32\sort.exe /r') do (
  if "!RSCRIPT_EXE!"=="" (
    if exist "%ProgramFiles%\R\%%V\bin\Rscript.exe" (
      set "RSCRIPT_EXE=%ProgramFiles%\R\%%V\bin\Rscript.exe"
    ) else if exist "%ProgramFiles%\R\%%V\bin\x64\Rscript.exe" (
      set "RSCRIPT_EXE=%ProgramFiles%\R\%%V\bin\x64\Rscript.exe"
    )
  )
)
if not "!RSCRIPT_EXE!"=="" goto :run

for /f "delims=" %%I in ('where Rscript.exe 2^>nul') do (
  if "!RSCRIPT_EXE!"=="" set "RSCRIPT_EXE=%%I"
)
if not "!RSCRIPT_EXE!"=="" goto :run

echo Could not find Rscript.exe.
echo Please install R and make sure Rscript.exe is on PATH.
pause
exit /b 3

:run
set "R="
set "R_HOME="
set "R_ARCH="
for %%I in ("%RSCRIPT_EXE%") do set "RSCRIPT_DIR=%%~dpI"
for %%I in ("%RSCRIPT_DIR%..") do set "R_INSTALL_ROOT=%%~fI"
if not exist "%R_INSTALL_ROOT%\library\base" (
  for %%I in ("%R_INSTALL_ROOT%\..") do set "R_INSTALL_ROOT=%%~fI"
)
set "PATH=%R_INSTALL_ROOT%\bin;%R_INSTALL_ROOT%\bin\x64;%PATH%"

pushd "%~dp0\.."
echo Running: "%RSCRIPT_EXE%" --no-save --no-restore "%SCRIPT%" %*
echo.
"%RSCRIPT_EXE%" --no-save --no-restore "%SCRIPT%" %*
set "EXIT_CODE=%ERRORLEVEL%"
popd

echo.
if not "%EXIT_CODE%"=="0" (
  echo Script finished with errors. Exit code: %EXIT_CODE%
) else (
  echo Script finished successfully.
)
if not defined DPMIXGPD_NO_PAUSE pause
exit /b %EXIT_CODE%
