$ErrorActionPreference = "Stop"
Set-Location $PSScriptRoot

if (Get-Command latexmk -ErrorAction SilentlyContinue) {
    latexmk -C CausalMixGPD_CSDA_article.tex | Out-Null
    latexmk -pdf -interaction=nonstopmode -file-line-error -synctex=1 CausalMixGPD_CSDA_article.tex
} else {
    pdflatex -interaction=nonstopmode -file-line-error -synctex=1 CausalMixGPD_CSDA_article.tex
    bibtex CausalMixGPD_CSDA_article
    pdflatex -interaction=nonstopmode -file-line-error -synctex=1 CausalMixGPD_CSDA_article.tex
    pdflatex -interaction=nonstopmode -file-line-error -synctex=1 CausalMixGPD_CSDA_article.tex
}

if (-not (Test-Path "CausalMixGPD_CSDA_article.pdf")) {
    throw "Build failed: CausalMixGPD_CSDA_article.pdf was not created."
}

Write-Host "Built manuscript/CausalMixGPD_CSDA_article.pdf"
