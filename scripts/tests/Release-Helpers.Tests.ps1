# Offline regression checks for release trust boundaries. No Pester dependency.
[CmdletBinding()]
param()
Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
. (Join-Path $PSScriptRoot '..\Release-Helpers.ps1')

$temporaryParent = [IO.Path]::GetFullPath([IO.Path]::GetTempPath()).TrimEnd('\','/')
$temporaryName = 'ibiblia-release-helper-tests-' + [Guid]::NewGuid().ToString('N')
$temporaryRoot = Join-Path $temporaryParent $temporaryName
New-Item -ItemType Directory -Path $temporaryRoot | Out-Null
$script:checks = 0
function Assert-Check {
    param([bool]$Condition, [string]$Message)
    if (-not $Condition) { throw $Message }
    $script:checks++
}
function Assert-Throws {
    param([scriptblock]$Action, [string]$Message)
    $threw = $false
    try { & $Action } catch { $threw = $true }
    Assert-Check $threw $Message
}
try {
    $file = Join-Path $temporaryRoot 'payload.txt'
    [IO.File]::WriteAllText($file, 'release fixture')
    $hash = Get-ReleaseSha256 $file
    Assert-ReleaseHash $file $hash
    Assert-Check ($hash.Length -eq 64) 'SHA256 must be a complete hex digest.'
    Assert-Throws { Assert-ReleaseHash $file ('0' * 64) } 'A modified dependency must fail hash validation.'
    Assert-Throws { Assert-ReleaseHash $file 'not-a-sha256' } 'A malformed dependency pin must fail validation.'

    foreach ($unsafe in @('../secrets.ini','safe/../../secrets.ini','C:\secrets.ini','/secrets.ini','safe//file','.','safe/../file')) {
        Assert-Throws { Join-ReleasePath $temporaryRoot $unsafe } "Unsafe archive path was accepted: $unsafe"
    }
    Assert-Check ((Get-ReleaseRelativePath 'languages\iBiblia.pt.po') -ceq 'languages/iBiblia.pt.po') 'Payload separators must be normalized.'

    $payload = Join-Path $temporaryRoot 'payload'
    New-Item -ItemType Directory -Path $payload | Out-Null
    Copy-ReleaseFile $temporaryRoot 'payload.txt' $payload 'payload.txt'
    Assert-ReleaseInventory $payload @('payload.txt')
    Assert-Throws { Copy-ReleaseFile $temporaryRoot 'payload.txt' $payload 'payload.txt' } 'Payload collisions must fail.'
    [IO.File]::WriteAllText((Join-Path $payload 'iBiblia.ini'), 'fixture settings')
    Assert-Throws { Assert-ReleaseInventory $payload @('payload.txt') } 'An accidentally included settings file must fail inventory validation.'

    $pe = Join-Path $temporaryRoot 'x64.dll'
    $stream = [IO.File]::Create($pe)
    $writer = New-Object IO.BinaryWriter($stream)
    try {
        $writer.Write((New-Object byte[] 128))
        $stream.Position = 0
        $writer.Write([uint16]0x5a4d)
        $stream.Position = 0x3c
        $writer.Write([int32]64)
        $stream.Position = 64
        $writer.Write([uint32]0x00004550)
        $writer.Write([uint16]0x8664)
    } finally { $writer.Dispose(); $stream.Dispose() }
    Assert-ReleaseX64PE $pe
    $stream = [IO.File]::OpenWrite($pe)
    $writer = New-Object IO.BinaryWriter($stream)
    try { $stream.Position = 68; $writer.Write([uint16]0x14c) } finally { $writer.Dispose(); $stream.Dispose() }
    Assert-Throws { Assert-ReleaseX64PE $pe } 'A Windows x86 DLL must fail the x64 gate.'
    Assert-Throws { Assert-ReleaseX64PE $file } 'A text file must fail the PE gate.'

    Add-Type -AssemblyName System.IO.Compression
    Add-Type -AssemblyName System.IO.Compression.FileSystem
    $archive = Join-Path $temporaryRoot 'traversal.zip'
    $zip = [IO.Compression.ZipFile]::Open($archive, [IO.Compression.ZipArchiveMode]::Create)
    try {
        $entry = $zip.CreateEntry('../escaped.ini')
        $entryStream = $entry.Open()
        try { $entryStream.WriteByte(65) } finally { $entryStream.Dispose() }
    } finally { $zip.Dispose() }
    Assert-Throws { Expand-ReleaseZip $archive (Join-Path $temporaryRoot 'extracted') } 'Archive traversal must fail.'
    Assert-Check (-not (Test-Path -LiteralPath (Join-Path $temporaryRoot 'escaped.ini'))) 'Archive traversal wrote outside its destination.'

    $successArchive = Join-Path $temporaryRoot 'valid.zip'
    [IO.Compression.ZipFile]::CreateFromDirectory($payload, $successArchive)
    Expand-ReleaseZip $successArchive (Join-Path $temporaryRoot 'valid-extracted')
    Assert-ReleaseInventory (Join-Path $temporaryRoot 'valid-extracted') @('payload.txt','iBiblia.ini')
    Assert-ReleaseHash (Join-Path $temporaryRoot 'valid-extracted\payload.txt') $hash

    $summaryLog = Join-Path $temporaryRoot 'fpcunit-summary.log'
    [IO.File]::WriteAllText($summaryLog, "Number of run tests: 42" + [Environment]::NewLine + "Number of errors: 0" + [Environment]::NewLine + "Number of failures: 0")
    $summary = Get-ReleaseTestSummary $summaryLog
    Assert-Check ($summary.tests -eq 42) 'FPCUnit manifest must record the actual test count.'
    [IO.File]::WriteAllText($summaryLog, 'no suite was executed')
    Assert-Throws { Get-ReleaseTestSummary $summaryLog } 'A successful process without tests must not qualify.'
    [IO.File]::WriteAllText($summaryLog, "Number of run tests: 42" + [Environment]::NewLine + "Number of errors: 0" + [Environment]::NewLine + "Number of failures: 1")
    Assert-Throws { Get-ReleaseTestSummary $summaryLog } 'FPCUnit assertion failures must block packaging.'
    $completedZip = Join-Path $temporaryRoot 'completed.zip'
    $hostExecutable = (Get-Process -Id $PID).Path
    Assert-Throws {
        Invoke-ReleaseCommand $hostExecutable @('-NoProfile','-Command','exit 7') (Join-Path $temporaryRoot 'failed-test.log')
        [IO.File]::WriteAllText($completedZip, 'must not publish')
    } 'A failing test process must stop the release pipeline.'
    Assert-Check (-not (Test-Path -LiteralPath $completedZip)) 'A failed test published a completed artifact.'
    Write-Host "PASS: $script:checks release-helper checks."
} finally {
    $resolvedTemporary = [IO.Path]::GetFullPath($temporaryRoot)
    $expectedTemporary = [IO.Path]::GetFullPath((Join-Path $temporaryParent $temporaryName))
    if ($resolvedTemporary -ceq $expectedTemporary -and
        $resolvedTemporary.StartsWith($temporaryParent + [IO.Path]::DirectorySeparatorChar, [StringComparison]::OrdinalIgnoreCase)) {
        Remove-Item -LiteralPath $resolvedTemporary -Recurse -Force
    } else { throw 'Refusing to clean an unexpected helper-test path.' }
}
