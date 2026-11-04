<#
.SYNOPSIS
Build and validate a Windows x64 portable release from clean, committed source.
.EXAMPLE
powershell.exe -NoProfile -File .\scripts\Build-Release.ps1 -Version 1.1.0
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory=$true)][ValidatePattern('^\d+\.\d+\.\d+$')][string]$Version,
    [string]$LazarusDir = 'C:\Apps\lazarus',
    [string]$OutputDirectory = 'dist',
    [string]$PythonPath = 'python.exe'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
. (Join-Path $PSScriptRoot 'Release-Helpers.ps1')

$repository = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..'))
$git = (Get-Command git.exe -ErrorAction Stop).Source
$python = (Get-Command $PythonPath -ErrorAction Stop).Source
$lazbuild = Join-Path $LazarusDir 'lazbuild.exe'
if (-not (Test-Path -LiteralPath $lazbuild -PathType Leaf)) { throw "lazbuild.exe not found in $LazarusDir" }
$fpcCandidates = @(Get-ChildItem -LiteralPath (Join-Path $LazarusDir 'fpc') -Directory |
    Sort-Object { [version]$_.Name })
if ($fpcCandidates.Count -eq 0) { throw 'No bundled FPC compiler found.' }
$fpc = Join-Path $fpcCandidates[-1].FullName 'bin\x86_64-win64\fpc.exe'
if (-not (Test-Path -LiteralPath $fpc -PathType Leaf)) { throw "FPC x64 compiler not found: $fpc" }

Push-Location $repository
$work = $null
try {
    $status = @(& $git status --porcelain --untracked-files=normal)
    if ($LASTEXITCODE -ne 0) { throw 'Could not read Git working-tree state.' }
    if ($status.Count -gt 0) {
        throw 'Release builds require clean committed source, including all release scripts, resources and version metadata. Commit the intended changes first.'
    }
    $commit = [string](& $git rev-parse HEAD)
    if ($LASTEXITCODE -ne 0 -or $commit -notmatch '^[0-9a-f]{40}$') { throw 'Could not resolve the release commit.' }
    if (-not [IO.Path]::IsPathRooted($OutputDirectory)) {
        $OutputDirectory = Join-Path $repository $OutputDirectory
    }
    $OutputDirectory = [IO.Path]::GetFullPath($OutputDirectory)
    $artifactName = "iBiblia-$Version-windows-x64"
    $finalZip = Join-Path $OutputDirectory ($artifactName + '.zip')
    $finalHash = Join-Path $OutputDirectory 'SHA256SUMS.txt'
    $finalManifest = Join-Path $OutputDirectory ($artifactName + '.manifest.json')
    foreach ($path in @($finalZip, $finalHash, $finalManifest)) {
        if (Test-Path -LiteralPath $path) { throw "Release output already exists; use a fresh output directory: $path" }
    }

    $work = Join-Path $repository ('outputs\release-work\' + [Guid]::NewGuid().ToString('N'))
    $source = Join-Path $work 'source'
    $payload = Join-Path $work 'portable'
    $verified = Join-Path $work 'extracted'
    $logs = Join-Path $work 'logs'
    New-Item -ItemType Directory -Path $work,$logs,$payload -Force | Out-Null
    $log = Join-Path $logs 'commands.log'

    # Archive only committed tracked source. Ignored EXEs, INIs and user data are never copied.
    $sourceZip = Join-Path $work 'source.zip'
    Invoke-ReleaseCommand $git @('archive', '--format=zip', "--output=$sourceZip", $commit) $log
    Expand-ReleaseZip $sourceZip $source

    [xml]$projectXml = Get-Content -LiteralPath (Join-Path $source 'iBiblia.lpi') -Raw
    $versionInfo = $projectXml.SelectSingleNode('/CONFIG/ProjectOptions/VersionInfo')
    $parts = @()
    foreach ($name in @('MajorVersionNr','MinorVersionNr','RevisionNr')) {
        $node = $versionInfo.SelectSingleNode($name)
        if ($null -eq $node) { $parts += '0' } else { $parts += $node.GetAttribute('Value') }
    }
    if (($parts -join '.') -cne $Version) { throw "Requested version $Version does not match committed project version $($parts -join '.')." }
    $autoIncrement = $versionInfo.SelectSingleNode('AutoIncrementBuild')
    if ($null -ne $autoIncrement -and $autoIncrement.GetAttribute('Value') -eq 'True') {
        throw 'Disable AutoIncrementBuild in committed release metadata before building.'
    }

    $lock = Get-Content -LiteralPath (Join-Path $source 'release\dependencies.lock.json') -Raw | ConvertFrom-Json
    if ($lock.schemaVersion -ne 1) { throw 'Unsupported release dependency-lock schema.' }
    $vendoredFpcBindings = Assert-ReleaseVendorProvenance $source
    $cache = Join-Path $repository 'outputs\release-cache'
    $dependencyRecords = @()
    foreach ($dependency in $lock.dependencies) {
        if ($dependency.kind -eq 'repository') {
            $repositoryFile = Join-ReleasePath $source $dependency.sourcePath
            Assert-ReleaseHash $repositoryFile $dependency.sha256
            $dependencyRoot = $source
        } else {
            $download = Get-ReleaseCachedDownload $dependency $cache
            if ($dependency.kind -eq 'zip') {
                $dependencyRoot = Join-Path $work ('dependencies\' + $dependency.id)
                Expand-ReleaseZip $download $dependencyRoot
            } elseif ($dependency.kind -eq 'file') {
                $dependencyRoot = Join-Path $work ('dependencies\' + $dependency.id)
                New-Item -ItemType Directory -Path $dependencyRoot -Force | Out-Null
                Copy-Item -LiteralPath $download -Destination (Join-ReleasePath $dependencyRoot $dependency.archiveName)
            } else { throw "Unknown dependency delivery type: $($dependency.kind)" }
        }
        foreach ($file in $dependency.files) {
            $runtimeFile = Join-ReleasePath $source $file.destination
            $from = Join-ReleasePath $dependencyRoot $file.source
            Assert-ReleaseHash $from $file.sha256
            # The isolated source snapshot is the sole build/test runtime.
            if (-not $from.Equals($runtimeFile, [StringComparison]::OrdinalIgnoreCase)) {
                New-Item -ItemType Directory -Path ([IO.Path]::GetDirectoryName($runtimeFile)) -Force | Out-Null
                Copy-Item -LiteralPath $from -Destination $runtimeFile -Force
            }
        }
        $dependencyRecords += [ordered]@{
            id = $dependency.id; version = $dependency.version
            source = $dependency.source; sha256 = $dependency.sha256
        }
    }

    $lazarusVersion = (& $lazbuild --version 2>&1 | Out-String).Trim()
    if ($LASTEXITCODE -ne 0) { throw 'Could not read Lazarus version.' }
    $compilerVersion = (& $fpc -iV 2>&1 | Out-String).Trim()
    if ($LASTEXITCODE -ne 0) { throw 'Could not read FPC version.' }
    $toolchain = [ordered]@{
        lazbuildVersion = $lazarusVersion
        lazbuildSha256 = Get-ReleaseSha256 $lazbuild
        fpcVersion = $compilerVersion
        fpcSha256 = Get-ReleaseSha256 $fpc
        target = 'x86_64-win64'
        mode = 'Release'
        optimization = 'O3'
        arguments = @('--os=win64','--cpu=x86_64','--ws=win32','--build-all','--no-write-project')
    }

    Push-Location $source
    try {
        $common = @("--lazarusdir=$LazarusDir",'--os=win64','--cpu=x86_64','--ws=win32','--build-all','--no-write-project')
        Invoke-ReleaseCommand $lazbuild ($common + @('--build-mode=Release','iBiblia.lpi')) (Join-Path $logs 'application-build.log')
        Invoke-ReleaseCommand $lazbuild ($common + @('tests\fpcunitibiblia_console.lpi')) (Join-Path $logs 'tests-build.log')

        # Each executable gets the exact same pinned libraries and executable-relative resources.
        foreach ($file in $lock.testRuntimeFiles) {
            Copy-Item -LiteralPath (Join-ReleasePath $source $file) -Destination (Join-Path $source ('tests\' + [IO.Path]::GetFileName($file))) -Force
        }
        Invoke-ReleaseCommand (Join-Path $source 'tests\fpcunitibiblia_console.exe') @('--all') (Join-Path $logs 'tests.log')
        $testSummary = Get-ReleaseTestSummary (Join-Path $logs 'tests.log')

        # TLS probe uses no API key. Both trust success and certificate rejection are gates.
        Invoke-ReleaseCommand $lazbuild ($common + @('tests\tls_runtime_probe.lpi')) (Join-Path $logs 'tls-build.log')
        $probe = Join-Path $source 'tests\tls_runtime_probe.exe'
        Invoke-ReleaseCommand $probe @('--runtime-dir',$source,'--url','https://example.com/') (Join-Path $logs 'tls-trusted.log')
        Invoke-ReleaseCommand $python @('tests\run_tls_runtime_probe.py','--probe',$probe,'--runtime-dir',$source,'--openssl',(Join-Path $source '.release-tools\openssl.exe')) (Join-Path $logs 'tls-local.log')
    } finally { Pop-Location }

    $expectedVersion = $Version + '.0'
    $exe = Join-Path $source 'iBiblia.exe'
    $metadata = [Diagnostics.FileVersionInfo]::GetVersionInfo($exe)
    if ($metadata.FileVersion -ne $expectedVersion -or $metadata.ProductVersion -ne $expectedVersion) {
        throw "EXE FileVersion and ProductVersion must both be $expectedVersion; got $($metadata.FileVersion) / $($metadata.ProductVersion)."
    }

    $expectedPaths = @()
    foreach ($file in $lock.payload) {
        Copy-ReleaseFile $source $file.source $payload $file.destination
        $expectedPaths += [string]$file.destination
    }
    Assert-ReleaseInventory $payload $expectedPaths
    foreach ($file in @(Get-ChildItem -LiteralPath $payload -Recurse -File)) {
        if ($file.Extension -in @('.exe','.dll')) { Assert-ReleaseX64PE $file.FullName }
    }
    $inventory = @(Get-ReleaseInventory $payload)
    $innerManifest = [ordered]@{
        schemaVersion = 1; product = 'iBiblia'; version = $Version
        platform = 'windows-x64'; sourceCommit = $commit
        builtAtUtc = [DateTime]::UtcNow.ToString('o')
        toolchain = $toolchain; dependencies = $dependencyRecords
        vendoredFpcBindings = $vendoredFpcBindings
        dependencyLockSha256 = Get-ReleaseSha256 (Join-Path $source 'release\dependencies.lock.json')
        validation = [ordered]@{
            applicationBuild = 'passed'; fullFpcUnitSuite = $testSummary
            tlsPublicTrustedCertificate = 'passed'; tlsLocalTrustedCertificate = 'passed'; tlsUntrustedCertificateRejected = 'passed'; tlsWrongHostnameRejected = 'passed'
        }
        files = $inventory
        manifestNote = 'This inventory excludes release-manifest.json itself; the external artifact manifest hashes the completed ZIP and all extracted files.'
    }
    Write-ReleaseJson $innerManifest (Join-Path $payload 'release-manifest.json')
    $expectedPaths += 'release-manifest.json'

    $candidateZip = Join-Path $work ($artifactName + '.zip')
    Add-Type -AssemblyName System.IO.Compression.FileSystem
    [IO.Compression.ZipFile]::CreateFromDirectory($payload, $candidateZip, [IO.Compression.CompressionLevel]::Optimal, $false)
    Expand-ReleaseZip $candidateZip $verified
    Assert-ReleaseInventory $verified $expectedPaths
    foreach ($file in @(Get-ChildItem -LiteralPath $verified -Recurse -File)) {
        if ($file.Extension -in @('.exe','.dll')) { Assert-ReleaseX64PE $file.FullName }
    }
    $extractedInventory = @(Get-ReleaseInventory $verified)
    foreach ($file in $extractedInventory) {
        Assert-ReleaseHash (Join-ReleasePath $payload $file.path) $file.sha256
    }
    $archiveHash = Get-ReleaseSha256 $candidateZip
    $outerManifest = [ordered]@{
        schemaVersion = 1; product = 'iBiblia'; version = $Version
        sourceCommit = $commit; toolchain = $toolchain
        artifact = [ordered]@{ file = [IO.Path]::GetFileName($finalZip); bytes = (Get-Item -LiteralPath $candidateZip).Length; sha256 = $archiveHash }
        files = $extractedInventory
    }
    $candidateManifest = Join-Path $work ($artifactName + '.manifest.json')
    Write-ReleaseJson $outerManifest $candidateManifest
    $candidateHash = Join-Path $work 'SHA256SUMS.txt'
    [IO.File]::WriteAllText($candidateHash, ($archiveHash + '  ' + [IO.Path]::GetFileName($finalZip) + [Environment]::NewLine), (New-Object Text.UTF8Encoding($false)))

    # Publish only after builds, tests, TLS checks, extraction and inventory all pass.
    New-Item -ItemType Directory -Path $OutputDirectory -Force | Out-Null
    Move-Item -LiteralPath $candidateManifest -Destination $finalManifest
    Move-Item -LiteralPath $candidateHash -Destination $finalHash
    Move-Item -LiteralPath $candidateZip -Destination $finalZip
    Write-Host "Validated portable release: $finalZip"
    Write-Host "SHA256: $archiveHash"
    Write-Host "Build and test evidence: $logs"
} catch {
    if ($null -ne $work) { Write-Warning "Release failed; no completed ZIP was published. Diagnostic files are preserved in $work" }
    throw
} finally { Pop-Location }
