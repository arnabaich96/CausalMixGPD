@echo off
setlocal

if "%~1"=="" (
  echo Usage: run_rscript_doubleclick.bat path\to\script.R [args...]
  pause
  exit /b 2
)

set "SCRIPT=%~1"
shift

if not exist "%SCRIPT%" (
  echo Script not found:
  echo   %SCRIPT%
  pause
  exit /b 2
)

set "RSCRIPT_EXE="
where /Q Rscript.exe
if %ERRORLEVEL%==0 (
  for /f "delims=" %%I in ('where Rscript.exe') do (
    set "RSCRIPT_EXE=%%I"
    goto :run
  )
)

for /f "delims=" %%V in ('dir /b /ad "C:\Program Files\R\R-*" 2^>nul ^| sort /r') do (
  if exist "C:\Program Files\R\%%V\bin\Rscript.exe" (
    set "RSCRIPT_EXE=C:\Program Files\R\%%V\bin\Rscript.exe"
    goto :run
  )
)

echo Could not find Rscript.exe.
echo Please install R and make sure Rscript.exe is on PATH.
pause
exit /b 3

:run
pushd "%~dp0\.."
echo Running: "%RSCRIPT_EXE%" "%SCRIPT%" %*
echo.
"%RSCRIPT_EXE%" "%SCRIPT%" %*
set "EXIT_CODE=%ERRORLEVEL%"
popd

echo.
if not "%EXIT_CODE%"=="0" (
  echo Script finished with errors. Exit code: %EXIT_CODE%
) else (
  echo Script finished successfully.
)
pause
exit /b %EXIT_CODE%
