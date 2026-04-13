param(
  [string]$InputRnw = "manuscript/CausalMixGPD_JSS_article.Rnw",
  [string]$OutputTex = "manuscript/CausalMixGPD_JSS_article.tex"
)

$ErrorActionPreference = "Stop"

function Find-Rscript {
  $command = Get-Command Rscript -ErrorAction SilentlyContinue
  if ($command) {
    return $command.Source
  }

  $roots = @(
    $env:ProgramFiles,
    ${env:ProgramFiles(x86)}
  ) | Where-Object { $_ -and (Test-Path (Join-Path $_ "R")) }

  $candidates = foreach ($root in $roots) {
    Get-ChildItem (Join-Path $root "R") -Recurse -Filter Rscript.exe -ErrorAction SilentlyContinue
  }

  $match = $candidates |
    Sort-Object FullName -Descending |
    Select-Object -First 1

  if (-not $match) {
    throw "Could not locate Rscript.exe. Install R or add Rscript to PATH."
  }

  return $match.FullName
}

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$repoRoot = Resolve-Path (Join-Path $scriptDir "..")
$rscript = Find-Rscript

Push-Location $repoRoot
try {
  & $rscript (Join-Path $scriptDir "render_manuscript.R") $InputRnw $OutputTex
  exit $LASTEXITCODE
} finally {
  Pop-Location
}
