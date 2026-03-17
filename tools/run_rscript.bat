@echo off
setlocal

if "%~1"=="" exit /b 2

set "SCRIPT=%~1"
shift

if not exist "%SCRIPT%" exit /b 2

set "RSCRIPT_EXE="
if exist "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" (
  set "RSCRIPT_EXE=C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
  goto :run
)

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
exit /b 3

:run
pushd "%~dp0\.."
"%RSCRIPT_EXE%" "%SCRIPT%" %*
set "EXIT_CODE=%ERRORLEVEL%"
popd
exit /b %EXIT_CODE%
