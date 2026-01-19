# PowerShell script to build R package with safe temp dir
New-Item -Path 'D:/Temp' -ItemType Directory -ErrorAction SilentlyContinue | Out-Null
$env:TMP ='D:/Temp'
$env:TEMP ='D:/Temp'
$env:R_TempDir ='D:/Temp'
R CMD build .
