$ErrorActionPreference = "Stop"

function Get-RscriptPath {
    $cmd = Get-Command Rscript -ErrorAction SilentlyContinue
    if ($cmd) {
        return $cmd.Source
    }

    $root = "C:\Program Files\R"
    if (Test-Path $root) {
        $candidate = Get-ChildItem $root -Directory |
            Sort-Object Name -Descending |
            ForEach-Object { Join-Path $_.FullName "bin\Rscript.exe" } |
            Where-Object { Test-Path $_ } |
            Select-Object -First 1
        if ($candidate) {
            return $candidate
        }
    }

    throw "Rscript.exe was not found. Install R or add Rscript to PATH."
}

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$rscript = Get-RscriptPath

& $rscript (Join-Path $scriptDir "run_all.R")
exit $LASTEXITCODE
