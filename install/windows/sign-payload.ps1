<#
  sign-payload.ps1 - Authenticode-sign Sonic Pi payload binaries (and the MSI).

  WHY
  ---
  Windows Smart App Control (SAC) evaluates every PE image that *runs or is
  loaded* - not just the installer package. Signing the MSI alone is therefore
  not enough: each staged .exe/.dll/.scx/.so must be signed *before* the MSI is
  built (so the installer embeds already-signed files), and the MSI itself
  signed afterwards.

  NO CREDENTIALS IN-TREE
  ----------------------
  The signing identity is selected from the Windows certificate store *by
  subject name*, supplied via the SP_SIGN_CERT_NAME environment variable. The
  private key never leaves the store / hardware token and is never referenced
  here - there is no PFX path, password, or certificate name committed to the
  repo. The presence of SP_SIGN_CERT_NAME is also the on/off switch:

      SP_SIGN_CERT_NAME unset            -> signing is SKIPPED (exit 0).
                                            CI and quick local builds just
                                            don't set it and get an unsigned
                                            build with a clear warning.
      SP_SIGN_CERT_NAME set, but signtool
        or the cert is unavailable, or a
        file fails to sign              -> ERROR (exit 1). If you asked for
                                            signing, a release must not quietly
                                            ship unsigned.

  Optional overrides (both have safe defaults):
      SP_SIGN_TSA   RFC3161 timestamp server (default: Certum's public TSA).

  USAGE
  -----
      powershell -NoProfile -ExecutionPolicy Bypass -File sign-payload.ps1 `
          -Path app -Description "Sonic Pi v5.0.0-beta3"
      powershell -NoProfile -ExecutionPolicy Bypass -File sign-payload.ps1 `
          -Path Sonic-Pi-for-Win-x64-v5.0.0-beta-3.msi -Description "Sonic Pi v5.0.0-beta3"

  -Path entries that are directories are scanned recursively for -Extensions;
  entries that are files are signed directly (used for the .msi).
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)][string[]]$Path,
    [string]$Description = "Sonic Pi",
    [string[]]$Extensions = @("exe", "dll", "scx", "so"),
    [int]$BatchSize = 40,
    [int]$MaxRetries = 4
)

$ErrorActionPreference = "Stop"

# --- On/off switch + identity come from the environment, never the repo -------
$certName = $env:SP_SIGN_CERT_NAME
$tsaUrl = if ($env:SP_SIGN_TSA) { $env:SP_SIGN_TSA } else { "http://time.certum.pl" }

if ([string]::IsNullOrWhiteSpace($certName)) {
    Write-Warning "SP_SIGN_CERT_NAME not set - skipping code signing (UNSIGNED build)."
    Write-Warning "Set it to your cert's subject name to enable signing, e.g.:"
    Write-Warning '  $env:SP_SIGN_CERT_NAME = "Open Source Developer ..."'
    exit 0
}

# --- Locate signtool.exe (Windows SDK; rarely on PATH) ------------------------
function Find-SignTool {
    $cmd = Get-Command signtool.exe -ErrorAction SilentlyContinue
    if ($cmd) { return $cmd.Source }
    $roots = @(
        "${env:ProgramFiles(x86)}\Windows Kits\10\bin",
        "${env:ProgramFiles}\Windows Kits\10\bin"
    )
    $best = $null
    foreach ($r in $roots) {
        if (Test-Path $r) {
            $hit = Get-ChildItem -Path $r -Recurse -Filter signtool.exe -ErrorAction SilentlyContinue |
                Where-Object { $_.FullName -match '\\x64\\signtool\.exe$' } |
                Sort-Object FullName -Descending | Select-Object -First 1
            if ($hit) { $best = $hit.FullName; break }
        }
    }
    return $best
}

$signtool = Find-SignTool
if (-not $signtool) {
    Write-Error "signtool.exe not found (install the Windows SDK), but SP_SIGN_CERT_NAME is set. Refusing to ship unsigned."
    exit 1
}

# --- Confirm a *valid* cert with that subject is available --------------------
# signtool /n does a substring match. Stores can hold expired certs with an
# overlapping subject (e.g. renewed "Open Source Developer ..." certs), so
# verify there is a match that is in-date and has a usable private key, and
# fail loudly on an expired-only match rather than silently picking it.
$now = Get-Date
$matched = foreach ($store in @("Cert:\CurrentUser\My", "Cert:\LocalMachine\My")) {
    try { Get-ChildItem $store -ErrorAction SilentlyContinue | Where-Object { $_.Subject -like "*$certName*" } } catch {}
}
$matched = @($matched)
$valid = @($matched | Where-Object { $_.HasPrivateKey -and $_.NotBefore -le $now -and $_.NotAfter -gt $now })
if ($matched.Count -eq 0) {
    Write-Error "No certificate matching subject '*$certName*' in CurrentUser\My or LocalMachine\My. Refusing to ship unsigned."
    exit 1
}
if ($valid.Count -eq 0) {
    Write-Error "Cert(s) matching '*$certName*' were found but are expired or have no private key. Refusing to ship unsigned:"
    $matched | ForEach-Object { Write-Host ("    {0}  (expires {1}, privkey {2})" -f $_.Subject, $_.NotAfter, $_.HasPrivateKey) }
    exit 1
}
if ($valid.Count -gt 1) {
    Write-Warning "Multiple valid certs match '*$certName*' - signtool picks one. Narrow SP_SIGN_CERT_NAME if needed:"
    $valid | ForEach-Object { Write-Host ("    {0}  (expires {1})" -f $_.Subject, $_.NotAfter) }
}

Write-Host "signtool : $signtool"
Write-Host "identity : (subject contains) $certName"
Write-Host "timestamp: $tsaUrl"

# --- Collect target files -----------------------------------------------------
$files = New-Object System.Collections.Generic.List[string]
$incl = $Extensions | ForEach-Object { "*.$_" }
foreach ($p in $Path) {
    if (Test-Path $p -PathType Leaf) {
        $files.Add((Resolve-Path $p).Path)
    }
    elseif (Test-Path $p -PathType Container) {
        Get-ChildItem -Path $p -Recurse -File -Include $incl -ErrorAction SilentlyContinue |
            ForEach-Object { $files.Add($_.FullName) }
    }
    else {
        Write-Warning "Path not found, skipping: $p"
    }
}
$files = @($files | Sort-Object -Unique)
$total = $files.Count
if ($total -eq 0) { Write-Host "Nothing to sign."; exit 0 }
Write-Host "Signing $total file(s) in batches of $BatchSize..."

# --- Batch-sign with timestamping + retry/backoff on TSA hiccups --------------
$signArgs = @("sign", "/n", $certName, "/fd", "SHA256", "/tr", $tsaUrl, "/td", "SHA256", "/d", $Description, "/q")
$failed = New-Object System.Collections.Generic.List[string]
$done = 0
for ($i = 0; $i -lt $total; $i += $BatchSize) {
    $batch = $files[$i..([Math]::Min($i + $BatchSize - 1, $total - 1))]
    $ok = $false
    for ($attempt = 1; $attempt -le $MaxRetries -and -not $ok; $attempt++) {
        & $signtool @signArgs @batch
        if ($LASTEXITCODE -eq 0) {
            $ok = $true
        }
        elseif ($attempt -lt $MaxRetries) {
            $wait = [Math]::Min(30, 5 * $attempt)
            Write-Warning "signtool exit $LASTEXITCODE (batch @ $i, attempt $attempt/$MaxRetries) - likely TSA throttling; retrying in ${wait}s."
            Start-Sleep -Seconds $wait
        }
    }
    if ($ok) {
        $done += $batch.Count
        Write-Host ("  signed {0}/{1}" -f $done, $total)
    }
    else {
        $batch | ForEach-Object { $failed.Add($_) }
    }
}

if ($failed.Count -gt 0) {
    Write-Host ""
    Write-Error "Code signing FAILED for $($failed.Count) file(s):"
    $failed | ForEach-Object { Write-Host "    $_" }
    exit 1
}

Write-Host "All $total file(s) signed and timestamped."
exit 0
