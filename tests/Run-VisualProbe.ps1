[CmdletBinding()]
param(
    [ValidateSet('pt', 'en')][string[]]$Language = @('pt', 'en'),
    [ValidateSet(96, 144, 192)][int[]]$Dpi = @(96, 144, 192),
    [switch]$SkipBuild,
    [switch]$IconFamilies,
    [ValidateSet('connected', 'clareza', 'mosaico', 'folio')][string]$InitialIconFamily = 'connected',
    [string]$OutputDirectory
)

$ErrorActionPreference = 'Stop'
$repositoryPath = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..'))
$probeWorkPath = Join-Path $repositoryPath '.codex-visual-work'
$probeBinaryPath = Join-Path $probeWorkPath 'probe-bin\ui_visual_probe.exe'
if (-not $OutputDirectory) {
    $OutputDirectory = Join-Path $probeWorkPath ('capture-' + (Get-Date -Format 'yyyyMMdd-HHmmss'))
}
$OutputDirectory = [IO.Path]::GetFullPath($OutputDirectory)
if (Test-Path -LiteralPath $OutputDirectory) {
    throw "Use a new output directory; existing visual reports are preserved: $OutputDirectory"
}
New-Item -ItemType Directory -Path $OutputDirectory -Force | Out-Null

if (-not $SkipBuild) {
    $buildLogPath = Join-Path $OutputDirectory 'build.log'
    & 'C:\Apps\lazarus\lazbuild.exe' --lazarusdir='C:\Apps\lazarus' --os=win64 --cpu=x86_64 --build-all (Join-Path $PSScriptRoot 'ui_visual_probe.lpi') *> $buildLogPath
    if ($LASTEXITCODE -ne 0) {
        Get-Content -LiteralPath $buildLogPath -Tail 35
        throw "Visual probe build failed ($LASTEXITCODE)."
    }
}
if (-not (Test-Path -LiteralPath $probeBinaryPath)) {
    throw 'Build the visual probe first or omit -SkipBuild.'
}

$captureResults = @()
foreach ($probeLanguage in $Language) {
    foreach ($probeDpi in $Dpi) {
        $runPath = Join-Path $OutputDirectory "$probeLanguage-$probeDpi"
        $runtimePath = Join-Path $runPath 'runtime'
        $capturePath = Join-Path $runPath 'captures'
        $languagePath = Join-Path $runtimePath 'languages'
        New-Item -ItemType Directory -Path $runtimePath, $capturePath, $languagePath -Force | Out-Null
        Copy-Item -LiteralPath $probeBinaryPath -Destination $runtimePath
        foreach ($library in @('pcre64.dll', 'sqlite3.dll')) {
            Copy-Item -LiteralPath (Join-Path $repositoryPath $library) -Destination $runtimePath
        }
        foreach ($catalogLanguage in @('pt', 'en')) {
            Copy-Item -LiteralPath (Join-Path $repositoryPath "languages\iBiblia.$catalogLanguage.po") -Destination (Join-Path $languagePath "ui_visual_probe.$catalogLanguage.po")
        }
        $stdoutPath = Join-Path $runPath 'stdout.log'
        $stderrPath = Join-Path $runPath 'stderr.log'
        Write-Output "Capturing $probeLanguage at $probeDpi PPI..."
        $processArgs = @('--language', $probeLanguage, '--dpi', "$probeDpi", '--output', ('"' + $capturePath + '"'))
        $processArgs += @('--initial-icon-family', $InitialIconFamily)
        if ($IconFamilies) {
            $processArgs += @('--icon-families', '1')
        }
        $process = Start-Process -FilePath (Join-Path $runtimePath 'ui_visual_probe.exe') -ArgumentList $processArgs -WorkingDirectory $runtimePath -WindowStyle Hidden -PassThru -RedirectStandardOutput $stdoutPath -RedirectStandardError $stderrPath
        $deadline = [DateTime]::UtcNow.AddMinutes(5)
        while (-not $process.WaitForExit(1000)) {
            if ([DateTime]::UtcNow -gt $deadline) {
                $process.Kill()
                throw "Visual probe timed out in $runPath. Inspect the last START line in stdout.log."
            }
        }
        $process.WaitForExit()
        $captureResults += [pscustomobject]@{
            Language = $probeLanguage
            Dpi = $probeDpi
            ExitCode = $process.ExitCode
            Captures = @(Get-ChildItem -LiteralPath $capturePath -Filter '*.png').Count
            ReviewItems = @(Select-String -LiteralPath $stdoutPath -Pattern '^REVIEW_').Count
            Directory = $runPath
        }
        if ($process.ExitCode -ne 0) {
            Get-Content -LiteralPath $stdoutPath -Tail 12
            Get-Content -LiteralPath $stderrPath -Tail 12
            throw "Visual probe failed in $runPath."
        }
    }
}
$captureResults | Export-Csv -LiteralPath (Join-Path $OutputDirectory 'summary.csv') -NoTypeInformation
$captureResults | Format-Table -AutoSize
Write-Output "Visual evidence: $OutputDirectory"
