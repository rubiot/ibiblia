$ErrorActionPreference = 'Stop'
Add-Type -AssemblyName System.Drawing
$culture = [Globalization.CultureInfo]::InvariantCulture
$family = [Drawing.FontFamily]::new('Segoe UI')
$glyphs = [ordered]@{}
function Format-Point($point) {
    return $point.X.ToString('0.###', $culture) + ' ' + $point.Y.ToString('0.###', $culture)
}
foreach ($item in @(@('vav', 0x05D5), @('yod', 0x05D9), @('beta', 0x03B2))) {
    $outline = [Drawing.Drawing2D.GraphicsPath]::new()
    try {
        $outline.AddString([string][char]$item[1], $family, [int][Drawing.FontStyle]::Bold, 64, [Drawing.PointF]::new(0, 0), [Drawing.StringFormat]::GenericTypographic)
        $bounds = $outline.GetBounds()
        $points = $outline.PathPoints
        $types = $outline.PathTypes
        $segments = [Collections.Generic.List[string]]::new()
        for ($n = 0; $n -lt $points.Length; $n++) {
            $kind = $types[$n] -band 7
            if ($kind -eq 0) { $segments.Add('M' + (Format-Point $points[$n])) }
            elseif ($kind -eq 1) { $segments.Add('L' + (Format-Point $points[$n])) }
            elseif ($kind -eq 3) {
                $segments.Add('C' + (Format-Point $points[$n]) + ' ' + (Format-Point $points[$n + 1]) + ' ' + (Format-Point $points[$n + 2]))
                $n += 2
            }
            if ($types[$n] -band 128) { $segments.Add('Z') }
        }
        $glyphs[$item[0]] = [ordered]@{ codepoint = ('U+{0:X4}' -f $item[1]); x = $bounds.X; y = $bounds.Y; width = $bounds.Width; height = $bounds.Height; d = ($segments -join '') }
    }
    finally { $outline.Dispose() }
}
$family.Dispose()
$destination = Join-Path $PSScriptRoot '../designs/glyphs.json'
[IO.Directory]::CreateDirectory([IO.Path]::GetDirectoryName($destination)) | Out-Null
[IO.File]::WriteAllText($destination, ($glyphs | ConvertTo-Json -Depth 5), [Text.UTF8Encoding]::new($false))
