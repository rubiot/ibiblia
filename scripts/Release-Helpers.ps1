# Shared by Build-Release.ps1 and its offline regression checks. PowerShell 5.1+.
Set-StrictMode -Version Latest

function Get-ReleaseSha256 {
    param([Parameter(Mandatory=$true)][string]$Path)
    return (Get-FileHash -LiteralPath $Path -Algorithm SHA256).Hash.ToLowerInvariant()
}

function Assert-ReleaseHash {
    param([string]$Path, [string]$Expected)
    if ($Expected -notmatch '^[0-9a-fA-F]{64}$') { throw "Invalid SHA256 pin for $Path" }
    $actual = Get-ReleaseSha256 $Path
    if ($actual -cne $Expected.ToLowerInvariant()) {
        throw "SHA256 mismatch for $Path. Expected $Expected; got $actual."
    }
}

function Get-ReleaseRelativePath {
    param([string]$Path)
    if ([string]::IsNullOrWhiteSpace($Path)) { throw 'Empty payload path.' }
    $normalized = $Path.Replace('\','/')
    if ([IO.Path]::IsPathRooted($Path) -or $normalized.StartsWith('/') -or
        $normalized.Contains(':') -or $normalized -match '(^|/)\.\.?(/|$)' -or
        $normalized -match '//') { throw "Unsafe payload path: $Path" }
    return $normalized
}

function Join-ReleasePath {
    param([string]$Root, [string]$RelativePath)
    $relative = Get-ReleaseRelativePath $RelativePath
    $rootPath = [IO.Path]::GetFullPath($Root).TrimEnd('\','/') + [IO.Path]::DirectorySeparatorChar
    $result = [IO.Path]::GetFullPath((Join-Path $Root $relative))
    if (-not $result.StartsWith($rootPath, [StringComparison]::OrdinalIgnoreCase)) {
        throw "Path leaves release directory: $RelativePath"
    }
    return $result
}

function Assert-ReleaseX64PE {
    param([string]$Path)
    $stream = [IO.File]::OpenRead($Path)
    $reader = New-Object IO.BinaryReader($stream)
    try {
        if ($stream.Length -lt 64 -or $reader.ReadUInt16() -ne 0x5a4d) {
            throw "Not a Windows PE file: $Path"
        }
        $stream.Position = 0x3c
        $offset = $reader.ReadInt32()
        if ($offset -lt 64 -or $offset -gt ($stream.Length - 26)) {
            throw "Invalid PE header offset: $Path"
        }
        $stream.Position = $offset
        if ($reader.ReadUInt32() -ne 0x00004550 -or $reader.ReadUInt16() -ne 0x8664) {
            throw "Expected an AMD64 Windows PE file: $Path"
        }
    } finally {
        $reader.Dispose()
        $stream.Dispose()
    }
}

function Expand-ReleaseZip {
    param([string]$Archive, [string]$Destination)
    Add-Type -AssemblyName System.IO.Compression
    Add-Type -AssemblyName System.IO.Compression.FileSystem
    if (Test-Path -LiteralPath $Destination) {
        if (@(Get-ChildItem -LiteralPath $Destination -Force).Count -ne 0) {
            throw "Archive destination must be empty: $Destination"
        }
    } else { New-Item -ItemType Directory -Path $Destination | Out-Null }
    $zip = [IO.Compression.ZipFile]::OpenRead($Archive)
    try {
        foreach ($entry in $zip.Entries) {
            $relative = $entry.FullName.Replace('\','/')
            if ($relative.EndsWith('/')) { $relative = $relative.TrimEnd('/') }
            if ($relative.Length -eq 0) { continue }
            $target = Join-ReleasePath $Destination $relative
            $unixKind = ($entry.ExternalAttributes -shr 16) -band 0xf000
            if ($unixKind -eq 0xa000) { throw "Archive symbolic links are not allowed: $relative" }
            if ($entry.FullName.EndsWith('/')) {
                New-Item -ItemType Directory -Path $target -Force | Out-Null
                continue
            }
            New-Item -ItemType Directory -Path ([IO.Path]::GetDirectoryName($target)) -Force | Out-Null
            $inputStream = $entry.Open()
            $outputStream = $null
            try {
                $outputStream = [IO.File]::Open($target, [IO.FileMode]::CreateNew, [IO.FileAccess]::Write)
                $inputStream.CopyTo($outputStream)
            } finally {
                if ($null -ne $outputStream) { $outputStream.Dispose() }
                $inputStream.Dispose()
            }
        }
    } finally { $zip.Dispose() }
}

function Copy-ReleaseFile {
    param([string]$SourceRoot, [string]$SourcePath, [string]$DestinationRoot, [string]$DestinationPath)
    $source = Join-ReleasePath $SourceRoot $SourcePath
    $destination = Join-ReleasePath $DestinationRoot $DestinationPath
    if (-not (Test-Path -LiteralPath $source -PathType Leaf)) { throw "Required release file missing: $source" }
    if (Test-Path -LiteralPath $destination) { throw "Duplicate release destination: $DestinationPath" }
    New-Item -ItemType Directory -Path ([IO.Path]::GetDirectoryName($destination)) -Force | Out-Null
    Copy-Item -LiteralPath $source -Destination $destination
}

function Get-ReleaseInventory {
    param([string]$Root)
    $prefix = [IO.Path]::GetFullPath($Root).TrimEnd('\','/') + [IO.Path]::DirectorySeparatorChar
    foreach ($file in @(Get-ChildItem -LiteralPath $Root -Recurse -File | Sort-Object FullName)) {
        $relative = $file.FullName.Substring($prefix.Length).Replace('\','/')
        [pscustomobject][ordered]@{
            path = $relative
            bytes = $file.Length
            sha256 = Get-ReleaseSha256 $file.FullName
        }
    }
}

function Assert-ReleaseInventory {
    param([string]$Root, [string[]]$ExpectedPaths)
    $actual = @(Get-ReleaseInventory $Root)
    $expected = @($ExpectedPaths | ForEach-Object { Get-ReleaseRelativePath $_ })
    if (@($expected | Sort-Object -Unique).Count -ne $expected.Count) { throw 'Duplicate payload allowlist entry.' }
    if ($actual.Count -ne $expected.Count) {
        throw "Release inventory has $($actual.Count) files; expected $($expected.Count)."
    }
    foreach ($item in $actual) {
        if (-not ($expected -ccontains $item.path)) { throw "Unexpected release file: $($item.path)" }
    }
}

function Invoke-ReleaseCommand {
    param([string]$Executable, [string[]]$Arguments, [string]$LogPath)
    $previousErrorAction = $ErrorActionPreference
    try {
        # Native stderr is logged as data under Windows PowerShell 5.1.
        $ErrorActionPreference = 'Continue'
        & $Executable @Arguments 2>&1 | Tee-Object -FilePath $LogPath -Append | Out-Host
        $exitCode = $LASTEXITCODE
    } finally { $ErrorActionPreference = $previousErrorAction }
    if ($exitCode -ne 0) { throw "$Executable failed with exit code $exitCode. See $LogPath" }
}

function Get-ReleaseCachedDownload {
    param([object]$Dependency, [string]$CacheDirectory)
    if ($Dependency.url -notmatch '^https://') { throw "Dependency requires HTTPS: $($Dependency.id)" }
    New-Item -ItemType Directory -Path $CacheDirectory -Force | Out-Null
    $cacheName = $Dependency.id + '-' + $Dependency.sha256.ToLowerInvariant() + '-' + $Dependency.archiveName
    $cacheFile = Join-ReleasePath $CacheDirectory $cacheName
    if (Test-Path -LiteralPath $cacheFile) {
        Assert-ReleaseHash $cacheFile $Dependency.sha256
        return $cacheFile
    }
    $temporary = $cacheFile + '.downloading-' + [Guid]::NewGuid().ToString('N')
    try {
        [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
        Invoke-WebRequest -UseBasicParsing -Uri $Dependency.url -OutFile $temporary
        Assert-ReleaseHash $temporary $Dependency.sha256
        Move-Item -LiteralPath $temporary -Destination $cacheFile
    } finally {
        if (Test-Path -LiteralPath $temporary) { Remove-Item -LiteralPath $temporary }
    }
    return $cacheFile
}

function Write-ReleaseJson {
    param([object]$Value, [string]$Path)
    $json = $Value | ConvertTo-Json -Depth 15
    [IO.File]::WriteAllText($Path, $json + [Environment]::NewLine, (New-Object Text.UTF8Encoding($false)))
}


function Get-ReleaseTestSummary {
    param([string]$LogPath)
    $text = Get-Content -LiteralPath $LogPath -Raw
    $run = [regex]::Match($text, '(?m)^Number of run tests:\s*(\d+)\s*$')
    $errors = [regex]::Match($text, '(?m)^Number of errors:\s*(\d+)\s*$')
    $failures = [regex]::Match($text, '(?m)^Number of failures:\s*(\d+)\s*$')
    if (-not $run.Success -or -not $errors.Success -or -not $failures.Success -or
        [int]$run.Groups[1].Value -lt 1 -or [int]$errors.Groups[1].Value -ne 0 -or
        [int]$failures.Groups[1].Value -ne 0) {
        throw "The full FPCUnit summary is missing or unsuccessful: $LogPath"
    }
    return [ordered]@{
        tests = [int]$run.Groups[1].Value
        errors = [int]$errors.Groups[1].Value
        failures = [int]$failures.Groups[1].Value
    }
}

function Assert-ReleaseVendorProvenance {
    param([string]$SourceRoot)
    $vendorRoot = Join-ReleasePath $SourceRoot 'vendor/fpc-openssl'
    $provenancePath = Join-ReleasePath $vendorRoot 'provenance.json'
    $provenance = Get-Content -LiteralPath $provenancePath -Raw | ConvertFrom-Json
    if ($provenance.revision -notmatch '^[0-9a-f]{40}$') {
        throw 'The vendored FPC bindings require an exact upstream revision.'
    }
    $verifiedFiles = @()
    foreach ($record in $provenance.files) {
        $expected = $record.sha256
        if ($record.name -ceq $provenance.compatibility_patch.file) {
            $expected = $provenance.compatibility_patch.sha256
        }
        Assert-ReleaseHash (Join-ReleasePath $vendorRoot $record.name) $expected
        $verifiedFiles += [ordered]@{path=$record.name;sha256=$expected}
    }
    foreach ($record in $provenance.license_files) {
        Assert-ReleaseHash (Join-ReleasePath $vendorRoot $record.name) $record.sha256
        $verifiedFiles += [ordered]@{path=$record.name;sha256=$record.sha256}
    }
    return [ordered]@{
        repository = $provenance.repository
        revision = $provenance.revision
        provenanceSha256 = Get-ReleaseSha256 $provenancePath
        files = $verifiedFiles
    }
}
