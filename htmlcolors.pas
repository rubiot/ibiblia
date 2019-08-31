unit HTMLColors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  RColor = record
    Name: String;
    Value: Integer;
  end;

function HTML2Color(const HTML: String): Integer;
function TranslateColorName(Name: String): Integer;

const
  // the following list shows 140 basic html color names and their corresponding
  // HTML hex-values
  ColorTable: array[0..140] of RColor = (
    (Name: 'aliceblue';            Value: $F0F8FF),
    (Name: 'antiquewhite';         Value: $FAEBD7),
    (Name: 'aqua';                 Value: $00FFFF),
    (Name: 'aquamarine';           Value: $7FFFD4),
    (Name: 'azure';                Value: $F0FFFF),
    (Name: 'beige';                Value: $F5F5DC),
    (Name: 'bisque';               Value: $FFE4C4),
    (Name: 'black';                Value: $000000),
    (Name: 'blanchedalmond';       Value: $FFFFCD),
    (Name: 'blue';                 Value: $0000FF),
    (Name: 'blueviolet';           Value: $8A2BE2),
    (Name: 'brown';                Value: $A52A2A),
    (Name: 'burlywood';            Value: $DEB887),
    (Name: 'cadetblue';            Value: $5F9EA0),
    (Name: 'chartreuse';           Value: $7FFF00),
    (Name: 'chocolate';            Value: $D2691E),
    (Name: 'coral';                Value: $FF7F50),
    (Name: 'cornflowerblue';       Value: $6495ED),
    (Name: 'cornsilk';             Value: $FFF8DC),
    (Name: 'crimson';              Value: $DC143C),
    (Name: 'cyan';                 Value: $00FFFF),
    (Name: 'darkblue';             Value: $00008B),
    (Name: 'darkcyan';             Value: $008B8B),
    (Name: 'darkgoldenrod';        Value: $B8860B),
    (Name: 'darkgray';             Value: $A9A9A9),
    (Name: 'darkgreen';            Value: $006400),
    (Name: 'darkkhaki';            Value: $BDB76B),
    (Name: 'darkmagenta';          Value: $8B008B),
    (Name: 'darkolivegreen';       Value: $556B2F),
    (Name: 'darkorange';           Value: $FF8C00),
    (Name: 'darkorchid';           Value: $9932CC),
    (Name: 'darkred';              Value: $8B0000),
    (Name: 'darksalmon';           Value: $E9967A),
    (Name: 'darkseagreen';         Value: $8FBC8F),
    (Name: 'darkslateblue';        Value: $483D8B),
    (Name: 'darkslategray';        Value: $2F4F4F),
    (Name: 'darkturquoise';        Value: $00CED1),
    (Name: 'darkviolet';           Value: $9400D3),
    (Name: 'deeppink';             Value: $FF1493),
    (Name: 'deepskyblue';          Value: $00BFFF),
    (Name: 'dimgray';              Value: $696969),
    (Name: 'dodgerblue';           Value: $1E90FF),
    (Name: 'firebrick';            Value: $B22222),
    (Name: 'floralwhite';          Value: $FFFAF0),
    (Name: 'forestgreen';          Value: $228B22),
    (Name: 'fuchsia';              Value: $FF00FF),
    (Name: 'gainsboro';            Value: $DCDCDC),
    (Name: 'ghostwhite';           Value: $F8F8FF),
    (Name: 'gold';                 Value: $FFD700),
    (Name: 'goldenrod';            Value: $DAA520),
    (Name: 'gray';                 Value: $808080),
    (Name: 'green';                Value: $008000),
    (Name: 'greenyellow';          Value: $ADFF2F),
    (Name: 'honeydew';             Value: $F0FFF0),
    (Name: 'hotpink';              Value: $FF69B4),
    (Name: 'indianred';            Value: $CD5C5C),
    (Name: 'indigo';               Value: $4B0082),
    (Name: 'ivory';                Value: $FFF0F0),
    (Name: 'khaki';                Value: $F0E68C),
    (Name: 'lavender';             Value: $E6E6FA),
    (Name: 'lavenderblush';        Value: $FFF0F5),
    (Name: 'lawngreen';            Value: $7CFC00),
    (Name: 'lemonchiffon';         Value: $FFFACD),
    (Name: 'lightblue';            Value: $ADD8E6),
    (Name: 'lightcoral';           Value: $F08080),
    (Name: 'lightcyan';            Value: $E0FFFF),
    (Name: 'lightgoldenrodyellow'; Value: $FAFAD2),
    (Name: 'lightgreen';           Value: $90EE90),
    (Name: 'lightgrey';            Value: $D3D3D3),
    (Name: 'lightpink';            Value: $FFB6C1),
    (Name: 'lightsalmon';          Value: $FFA07A),
    (Name: 'lightseagreen';        Value: $20B2AA),
    (Name: 'lightskyblue';         Value: $87CEFA),
    (Name: 'lightslategray';       Value: $778899),
    (Name: 'lightsteelblue';       Value: $B0C4DE),
    (Name: 'lightyellow';          Value: $FFFFE0),
    (Name: 'lime';                 Value: $00FF00),
    (Name: 'limegreen';            Value: $32CD32),
    (Name: 'linen';                Value: $FAF0E6),
    (Name: 'magenta';              Value: $FF00FF),
    (Name: 'maroon';               Value: $800000),
    (Name: 'mediumaquamarine';     Value: $66CDAA),
    (Name: 'mediumblue';           Value: $0000CD),
    (Name: 'mediumorchid';         Value: $BA55D3),
    (Name: 'mediumpurple';         Value: $9370DB),
    (Name: 'mediumseagreen';       Value: $3CB371),
    (Name: 'mediumpurple';         Value: $9370DB),
    (Name: 'mediumslateblue';      Value: $7B68EE),
    (Name: 'mediumspringgreen';    Value: $00FA9A),
    (Name: 'mediumturquoise';      Value: $48D1CC),
    (Name: 'mediumvioletred';      Value: $C71585),
    (Name: 'midnightblue';         Value: $191970),
    (Name: 'mintcream';            Value: $F5FFFA),
    (Name: 'mistyrose';            Value: $FFE4E1),
    (Name: 'moccasin';             Value: $FFE4B5),
    (Name: 'navajowhite';          Value: $FFDEAD),
    (Name: 'navy';                 Value: $000080),
    (Name: 'oldlace';              Value: $FDF5E6),
    (Name: 'olive';                Value: $808000),
    (Name: 'olivedrab';            Value: $6B8E23),
    (Name: 'orange';               Value: $FFA500),
    (Name: 'orangered';            Value: $FF4500),
    (Name: 'orchid';               Value: $DA70D6),
    (Name: 'palegoldenrod';        Value: $EEE8AA),
    (Name: 'palegreen';            Value: $98FB98),
    (Name: 'paleturquoise';        Value: $AFEEEE),
    (Name: 'palevioletred';        Value: $DB7093),
    (Name: 'papayawhip';           Value: $FFEFD5),
    (Name: 'peachpuff';            Value: $FFDBBD),
    (Name: 'peru';                 Value: $CD853F),
    (Name: 'pink';                 Value: $FFC0CB),
    (Name: 'plum';                 Value: $DDA0DD),
    (Name: 'powderblue';           Value: $B0E0E6),
    (Name: 'purple';               Value: $800080),
    (Name: 'red';                  Value: $FF0000),
    (Name: 'rosybrown';            Value: $BC8F8F),
    (Name: 'royalblue';            Value: $4169E1),
    (Name: 'saddlebrown';          Value: $8B4513),
    (Name: 'salmon';               Value: $FA8072),
    (Name: 'sandybrown';           Value: $F4A460),
    (Name: 'seagreen';             Value: $2E8B57),
    (Name: 'seashell';             Value: $FFF5EE),
    (Name: 'sienna';               Value: $A0522D),
    (Name: 'silver';               Value: $C0C0C0),
    (Name: 'skyblue';              Value: $87CEEB),
    (Name: 'slateblue';            Value: $6A5ACD),
    (Name: 'slategray';            Value: $708090),
    (Name: 'snow';                 Value: $FFFAFA),
    (Name: 'springgreen';          Value: $00FF7F),
    (Name: 'steelblue';            Value: $4682B4),
    (Name: 'tan';                  Value: $D2B48C),
    (Name: 'teal';                 Value: $008080),
    (Name: 'thistle';              Value: $D8BFD8),
    (Name: 'tomato';               Value: $FD6347),
    (Name: 'turquoise';            Value: $40E0D0),
    (Name: 'violet';               Value: $EE82EE),
    (Name: 'wheat';                Value: $F5DEB3),
    (Name: 'white';                Value: $FFFFFF),
    (Name: 'whitesmoke';           Value: $F5F5F5),
    (Name: 'yellow';               Value: $FFFF00),
    (Name: 'yellowgreen';          Value: $9ACD32)
  );

implementation

function TranslateColorName(Name: String): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(ColorTable) to High(ColorTable) do
    // find matching color name
    if ColorTable[I].Name = Name.ToLower then
    begin
      // return RGB colors
      Result :=
        Byte(ColorTable[I].Value shr 16) +
        Byte(ColorTable[I].Value shr 8) shl 8 +
        Byte(ColorTable[I].Value) shl 16;
      Break;
    end;
end;

function HTML2Color(const HTML: String): Integer;
var
  Offset: Integer;
begin
  try
    // check for leading '#'
    if Copy(HTML, 1, 1) = '#' then
      Offset := 1
    else
      Offset := 0;
    // convert hexa-decimal values to RGB
    Result :=
      Integer(StrToInt('$' + Copy(HTML, Offset + 1, 2))) +
      Integer(StrToInt('$' + Copy(HTML, Offset + 3, 2))) shl 8 +
      Integer(StrToInt('$' + Copy(HTML, Offset + 5, 2))) shl 16;
  except
    // try for color names
    Result := TranslateColorName(LowerCase(HTML));
  end;
end;

end.

