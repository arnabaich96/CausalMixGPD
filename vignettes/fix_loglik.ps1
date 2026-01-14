# Remove logLik calls from all affected vignettes
$vignettes = @(
    "v06-unconditional-DPmixGPD-CRP.Rmd",
    "v07-unconditional-DPmixGPD-SB.Rmd",
    "v08-conditional-DPmix-CRP.Rmd",
    "v09-conditional-DPmix-SB.Rmd",
    "v12-causal-same-backend-CRP.Rmd",
    "v13-causal-same-backend-SB.Rmd",
    "v14-causal-different-backends-CRP.Rmd",
    "v15-causal-different-backends-SB.Rmd"
)

$vignetteDir = "d:\OneDrive - Florida State University\MyFSU_OneDrive\R-Codes\DPMGPD_package\DPmixGPD\vignettes"

foreach ($vig in $vignettes) {
    $path = Join-Path $vignetteDir $vig
    $content = Get-Content $path -Raw
    
    # Remove loglik variable assignments
    $content = $content -replace 'loglik_\w+\s*<-\s*logLik\([^)]+\)\s*\n', ''
    
    # Remove cat statements with LogLik
    $content = $content -replace 'cat\(".*LogLik.*?"\)\s*\n', ''
    
    # Remove data.frame with LogLik columns
    $content = $content -replace 'LogLik\s*=\s*(c\([^)]+\)|logliks[^,\n]+)', 'Comparison = "See summary()"'
    
    # Remove logliks vector declarations
    $content = $content -replace 'logliks[_\w]*\s*<-\s*numeric\([^)]+\)\s*\n', ''
    
    # Save back
    Set-Content -Path $path -Value $content -NoNewline
    
    Write-Host "Fixed: $vig"
}

Write-Host "`nAll vignettes processed!"
