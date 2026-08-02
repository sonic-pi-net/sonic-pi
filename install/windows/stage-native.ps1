<#
  stage-native.ps1 - stage app\server\native into the MSI staging tree from
  the explicit allowlist in native-manifest.txt.

  app\server\native is gitignored (populated by deployment, not git), so it
  cannot be staged from git HEAD like the tracked payload. Instead every
  file must be accounted for by the manifest:

    - a plain glob entry must match >= 1 root file  -> staged (else FAIL)
    - a "dir:" entry is copied recursively, minus ignore: globs (missing -> FAIL)
    - an "ignore:" glob marks expected local debris  -> skipped
    - anything else in the native root               -> FAIL the build
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)][string]$Source,
    [Parameter(Mandatory = $true)][string]$Dest,
    [Parameter(Mandatory = $true)][string]$Manifest
)

$ErrorActionPreference = "Stop"

if (-not (Test-Path $Source -PathType Container)) { Write-Error "Source not found: $Source"; exit 1 }
if (-not (Test-Path $Manifest -PathType Leaf)) { Write-Error "Manifest not found: $Manifest"; exit 1 }
$Source = (Resolve-Path $Source).Path
New-Item -ItemType Directory -Force -Path $Dest | Out-Null
$Dest = (Resolve-Path $Dest).Path

# --- Parse manifest -----------------------------------------------------------
$fileGlobs = @(); $dirEntries = @(); $ignoreGlobs = @()
foreach ($raw in Get-Content $Manifest) {
    $line = ($raw -replace '(^|\s)#.*$', '').Trim()
    if (-not $line) { continue }
    if ($line -match '^dir:\s*(.+)$') { $dirEntries += $Matches[1].Trim() }
    elseif ($line -match '^ignore:\s*(.+)$') { $ignoreGlobs += $Matches[1].Trim() }
    else { $fileGlobs += $line }
}

function Test-GlobMatch([string]$name, [string[]]$globs) {
    foreach ($g in $globs) { if ($name -like $g) { return $true } }
    return $false
}

$errors = @()
$matched = New-Object 'System.Collections.Generic.HashSet[string]' ([System.StringComparer]::OrdinalIgnoreCase)

# --- Root files from globs ----------------------------------------------------
$rootFiles = @(Get-ChildItem -Path $Source -File)
$stagedCount = 0
foreach ($glob in $fileGlobs) {
    $hits = @($rootFiles | Where-Object { $_.Name -like $glob })
    if ($hits.Count -eq 0) { $errors += "manifest entry matched no file: $glob"; continue }
    foreach ($f in $hits) {
        [void]$matched.Add($f.Name)
        Copy-Item -Path $f.FullName -Destination (Join-Path $Dest $f.Name)
        $stagedCount++
    }
}

# --- Directory trees ----------------------------------------------------------
foreach ($d in $dirEntries) {
    $src = Join-Path $Source $d
    if (-not (Test-Path $src -PathType Container)) { $errors += "manifest dir missing: $d"; continue }
    $rcArgs = @($src, (Join-Path $Dest $d), '/E', '/NFL', '/NDL', '/NJH', '/NJS', '/NP')
    if ($ignoreGlobs.Count -gt 0) { $rcArgs += '/XF'; $rcArgs += $ignoreGlobs }
    robocopy @rcArgs | Out-Null
    if ($LASTEXITCODE -ge 8) { $errors += "robocopy failed (exit $LASTEXITCODE) staging dir: $d" }
}

# --- Nothing unaccounted for --------------------------------------------------
$ignored = @(); $unknown = @()
foreach ($f in $rootFiles) {
    if ($matched.Contains($f.Name)) { continue }
    if (Test-GlobMatch $f.Name $ignoreGlobs) { $ignored += $f.Name } else { $unknown += $f.Name }
}
$coveredTops = @($dirEntries | ForEach-Object { ($_ -split '[\\/]')[0] })
foreach ($d in Get-ChildItem -Path $Source -Directory) {
    if ($coveredTops -notcontains $d.Name) { $unknown += "$($d.Name)\ (directory)" }
}

if ($ignored.Count -gt 0) { Write-Host ("ignored (per manifest): " + ($ignored -join ', ')) }
Write-Host "staged $stagedCount root file(s) + $($dirEntries.Count) dir tree(s)"

if ($errors.Count -gt 0 -or $unknown.Count -gt 0) {
    $errors | ForEach-Object { Write-Host "ERROR: $_" }
    if ($unknown.Count -gt 0) {
        Write-Host "ERROR: unexpected item(s) in ${Source}:"
        $unknown | ForEach-Object { Write-Host "    $_" }
        Write-Host "Add to native-manifest.txt to ship, add an ignore: entry to keep local, or delete."
    }
    exit 1
}
exit 0
