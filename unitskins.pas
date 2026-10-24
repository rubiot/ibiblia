unit unitskins;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Graphics, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls, Menus, Buttons, Dialogs
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF}
  ;

type
  TSkinId = (
    skClassic,          // Classic (Default light)
    skTheWordDark,      // theWord 7 Dark (Charcoal)
    skNight,            // OLED / Pitch Black
    skSepia,            // theWord Parchment / Sepia (Eye-strain relief)
    skSlate,            // theWord Steel Blue / Cool Slate
    skSolarizedDark,    // Solarized Dark
    skHighContrastDark  // High Contrast Stark Black & White
  );

  TSkinColors = record
    Id: TSkinId;
    Name: string;
    DisplayName: string;
    IsDark: Boolean;
    // Reading area / Editor / TreeView
    WindowBg: TColor;
    WindowText: TColor;
    WindowTextDim: TColor;
    // Shell, Panels, Toolbars, Statusbar, Tabs
    PanelBg: TColor;
    PanelText: TColor;
    PanelBorder: TColor;
    // Words hover & selection
    PointedBg: TColor;
    PointedText: TColor;
    HighlightBg: TColor;
    HighlightText: TColor;
    // TreeView / Navigation
    TreeBg: TColor;
    TreeText: TColor;
    // Dictionary Popup / Note Hints
    NoteBg: TColor;
    NoteText: TColor;
    // Accents
    HeaderColor: TColor;
    VerseNumColor: TColor;
    VerseHighlightBg: TColor;
  end;

function MakeRGB(R, G, B: Byte): TColor; inline;
function GetSkin(AId: TSkinId): TSkinColors;
function SkinNameToId(const AName: string): TSkinId;
procedure SetCurrentSkin(AId: TSkinId);
procedure SetCurrentSkinByName(const AName: string);
procedure ApplySkinToForm(AForm: TCustomForm; const ASkin: TSkinColors);
procedure ApplySkinToControlTree(AParent: TWinControl; const ASkin: TSkinColors);
procedure HookTabControl(AControl: TWinControl);
procedure UnhookTabControl(AControl: TWinControl);
procedure HookButtonControl(AControl: TWinControl);
procedure UnhookButtonControl(AControl: TWinControl);
procedure HookCheckBoxControl(AControl: TWinControl);
procedure UnhookCheckBoxControl(AControl: TWinControl);
procedure HookGroupBoxControl(AControl: TWinControl);
procedure UnhookGroupBoxControl(AControl: TWinControl);
procedure HookScrollBoxControl(AControl: TWinControl);
procedure UnhookScrollBoxControl(AControl: TWinControl);
procedure HookToolBarControl(AControl: TWinControl);
procedure UnhookToolBarControl(AControl: TWinControl);
procedure HookTreeViewControl(AControl: TWinControl);
procedure UnhookTreeViewControl(AControl: TWinControl);
function HandleToolBarCustomDraw(lParam: LPARAM; const ASkin: TSkinColors; var AHandled: Boolean): LRESULT;

// Themed Dialogs
function ThemedMessageDlg(const aCaption, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint = 0): Integer;
function ThemedMessageDlg(Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint = 0): Integer;
procedure ThemedShowMessage(const Msg: string);

// Win32 OS-level Dark Mode & Theming
procedure ApplyDarkModeToWindow(AForm: TCustomForm; AIsDark: Boolean);
procedure ApplyDarkModeToControl(AControl: TWinControl; AIsDark: Boolean);
procedure SetControlThemeMode(AControl: TWinControl; AIsDark: Boolean; const DarkTheme: string = '');
procedure ApplyDarkModeToMenu(AMenu: TMenu; FormHandle: THandle; AIsDark: Boolean; ABackCol: TColor);
function GetCachedPanelBrush(AColor: TColor): THandle;
function GetCachedWindowBrush(AColor: TColor): THandle;

{$IFDEF WINDOWS}
type
  TSubclassProc = function(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM;
    uIdSubclass: PtrUInt; dwRefData: PtrUInt): LRESULT; stdcall;

  TSetWindowSubclass = function(hWnd: HWND; pfnSubclass: TSubclassProc;
    uIdSubclass: PtrUInt; dwRefData: PtrUInt): WINBOOL; stdcall;
  TRemoveWindowSubclass = function(hWnd: HWND; pfnSubclass: TSubclassProc;
    uIdSubclass: PtrUInt): WINBOOL; stdcall;
  TDefSubclassProc = function(hWnd: HWND; uMsg: UINT; wParam: WPARAM;
    lParam: LPARAM): LRESULT; stdcall;

var
  fnSetWindowSubclass: TSetWindowSubclass;
  fnRemoveWindowSubclass: TRemoveWindowSubclass;
  fnDefSubclassProc: TDefSubclassProc;
{$ENDIF}

var
  CurrentSkin: TSkinColors;

implementation

{$IFDEF WINDOWS}
type
  TPreferredAppMode = (
    pamDefault = 0,
    pamAllowDark = 1,
    pamForceDark = 2,
    pamForceLight = 3,
    pamMax = 4
  );

  TDwmSetWindowAttribute = function(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HRESULT; stdcall;
  TSetWindowTheme = function(hwnd: HWND; pszSubAppName: LPCWSTR; pszSubIdList: LPCWSTR): HRESULT; stdcall;
  TAllowDarkModeForWindow = function(hWnd: HWND; allow: BOOL): BOOL; stdcall;
  TSetPreferredAppMode = function(appMode: TPreferredAppMode): TPreferredAppMode; stdcall;
  TFlushMenuThemes = procedure; stdcall;

var
  hUxTheme: THandle = 0;
  hDwmApi: THandle = 0;
  hComCtl: THandle = 0;
  fnDwmSetWindowAttribute: TDwmSetWindowAttribute = nil;
  fnSetWindowTheme: TSetWindowTheme = nil;
  fnAllowDarkModeForWindow: TAllowDarkModeForWindow = nil;
  fnSetPreferredAppMode: TSetPreferredAppMode = nil;
  fnFlushMenuThemes: TFlushMenuThemes = nil;
  UxThemeInitialized: Boolean = False;

procedure InitUxTheme;
begin
  if UxThemeInitialized then exit;
  UxThemeInitialized := True;

  hDwmApi := LoadLibrary('dwmapi.dll');
  if hDwmApi <> 0 then
    Pointer(fnDwmSetWindowAttribute) := GetProcAddress(hDwmApi, 'DwmSetWindowAttribute');

  hUxTheme := LoadLibrary('uxtheme.dll');
  if hUxTheme <> 0 then
  begin
    Pointer(fnSetWindowTheme) := GetProcAddress(hUxTheme, 'SetWindowTheme');
    Pointer(fnAllowDarkModeForWindow) := GetProcAddress(hUxTheme, PChar(PtrUInt(133)));
    Pointer(fnFlushMenuThemes) := GetProcAddress(hUxTheme, PChar(PtrUInt(136)));
    Pointer(fnSetPreferredAppMode) := GetProcAddress(hUxTheme, PChar(PtrUInt(135)));
  end;

  hComCtl := LoadLibrary('comctl32.dll');
  if hComCtl <> 0 then
  begin
    Pointer(fnSetWindowSubclass) := GetProcAddress(hComCtl, 'SetWindowSubclass');
    Pointer(fnRemoveWindowSubclass) := GetProcAddress(hComCtl, 'RemoveWindowSubclass');
    Pointer(fnDefSubclassProc) := GetProcAddress(hComCtl, 'DefSubclassProc');
  end;
end;
{$ENDIF}

function MakeRGB(R, G, B: Byte): TColor; inline;
begin
  Result := TColor(R or (G shl 8) or (B shl 16));
end;

function GetSkin(AId: TSkinId): TSkinColors;
begin
  Result.Name := '';
  Result.DisplayName := '';
  case AId of
    skClassic:
    begin
      Result.Id := skClassic;
      Result.Name := 'Classic';
      Result.DisplayName := 'Classic (Light)';
      Result.IsDark := False;
      Result.WindowBg := clWindow;
      Result.WindowText := clWindowText;
      Result.WindowTextDim := MakeRGB(130, 130, 130);
      Result.PanelBg := clBtnFace;
      Result.PanelText := clWindowText;
      Result.PanelBorder := MakeRGB(200, 200, 200);
      Result.PointedBg := clHighlight;
      Result.PointedText := clHighlightText;
      Result.HighlightBg := clYellow;
      Result.HighlightText := clBlack;
      Result.TreeBg := clWindow;
      Result.TreeText := clWindowText;
      Result.NoteBg := MakeRGB(204, 251, 251);
      Result.NoteText := clBlack;
      Result.HeaderColor := MakeRGB(190, 124, 65);
      Result.VerseNumColor := MakeRGB(44, 186, 125);
      Result.VerseHighlightBg := clSilver;
    end;

    skTheWordDark:
    begin
      Result.Id := skTheWordDark;
      Result.Name := 'TheWordDark';
      Result.DisplayName := 'theWord Dark (Charcoal)';
      Result.IsDark := True;
      Result.WindowBg := MakeRGB($1E, $1E, $1E);
      Result.WindowText := MakeRGB($E2, $E2, $E2);
      Result.WindowTextDim := MakeRGB($A6, $A6, $A6);
      Result.PanelBg := MakeRGB($25, $25, $26);
      Result.PanelText := MakeRGB($D8, $D8, $D8);
      Result.PanelBorder := MakeRGB($3E, $3E, $42);
      Result.PointedBg := MakeRGB($0E, $63, $9C);
      Result.PointedText := MakeRGB($FF, $FF, $FF);
      Result.HighlightBg := MakeRGB($65, $5C, $0F);
      Result.HighlightText := MakeRGB($FF, $FF, $FF);
      Result.TreeBg := MakeRGB($1E, $1E, $1E);
      Result.TreeText := MakeRGB($F0, $F0, $F0);
      Result.NoteBg := MakeRGB($2A, $2A, $2D);
      Result.NoteText := MakeRGB($E2, $E2, $E2);
      Result.HeaderColor := MakeRGB($4E, $C9, $B0);
      Result.VerseNumColor := MakeRGB($FF, $78, $78);
      Result.VerseHighlightBg := MakeRGB($3A, $3D, $41);
    end;

    skNight:
    begin
      Result.Id := skNight;
      Result.Name := 'Night';
      Result.DisplayName := 'Night (Pitch Black)';
      Result.IsDark := True;
      Result.WindowBg := MakeRGB($12, $12, $12);
      Result.WindowText := MakeRGB($F0, $F0, $F0);
      Result.WindowTextDim := MakeRGB($A0, $A0, $A0);
      Result.PanelBg := MakeRGB($1A, $1A, $1A);
      Result.PanelText := MakeRGB($E8, $E8, $E8);
      Result.PanelBorder := MakeRGB($2C, $2C, $2C);
      Result.PointedBg := MakeRGB($1A, $73, $E8);
      Result.PointedText := MakeRGB($FF, $FF, $FF);
      Result.HighlightBg := MakeRGB($D4, $A0, $17);
      Result.HighlightText := MakeRGB($00, $00, $00);
      Result.TreeBg := MakeRGB($12, $12, $12);
      Result.TreeText := MakeRGB($F0, $F0, $F0);
      Result.NoteBg := MakeRGB($1A, $1A, $1A);
      Result.NoteText := MakeRGB($F0, $F0, $F0);
      Result.HeaderColor := MakeRGB($81, $C9, $95);
      Result.VerseNumColor := MakeRGB($FF, $78, $78);
      Result.VerseHighlightBg := MakeRGB($2E, $2E, $2E);
    end;

    skSepia:
    begin
      Result.Id := skSepia;
      Result.Name := 'Sepia';
      Result.DisplayName := 'Sepia (Parchment)';
      Result.IsDark := False;
      Result.WindowBg := MakeRGB($F4, $EE, $DB);
      Result.WindowText := MakeRGB($2D, $24, $1E);
      Result.WindowTextDim := MakeRGB($85, $77, $6B);
      Result.PanelBg := MakeRGB($E2, $D7, $C3);
      Result.PanelText := MakeRGB($2D, $24, $1E);
      Result.PanelBorder := MakeRGB($C9, $BC, $9C);
      Result.PointedBg := MakeRGB($B8, $D2, $E6);
      Result.PointedText := MakeRGB($00, $00, $00);
      Result.HighlightBg := MakeRGB($D9, $C2, $89);
      Result.HighlightText := MakeRGB($00, $00, $00);
      Result.TreeBg := MakeRGB($F4, $EE, $DB);
      Result.TreeText := MakeRGB($2D, $24, $1E);
      Result.NoteBg := MakeRGB($F7, $F3, $E8);
      Result.NoteText := MakeRGB($2D, $24, $1E);
      Result.HeaderColor := MakeRGB($2E, $6B, $4E);
      Result.VerseNumColor := MakeRGB($8A, $4E, $2C);
      Result.VerseHighlightBg := MakeRGB($E0, $D4, $C0);
    end;

    skSlate:
    begin
      Result.Id := skSlate;
      Result.Name := 'Slate';
      Result.DisplayName := 'Slate (Steel Blue)';
      Result.IsDark := True;
      Result.WindowBg := MakeRGB($1C, $23, $2B);
      Result.WindowText := MakeRGB($E0, $E8, $F0);
      Result.WindowTextDim := MakeRGB($A0, $B0, $C0);
      Result.PanelBg := MakeRGB($22, $2B, $35);
      Result.PanelText := MakeRGB($E0, $E8, $F0);
      Result.PanelBorder := MakeRGB($2F, $3D, $4D);
      Result.PointedBg := MakeRGB($2A, $60, $99);
      Result.PointedText := MakeRGB($FF, $FF, $FF);
      Result.HighlightBg := MakeRGB($35, $52, $70);
      Result.HighlightText := MakeRGB($FF, $FF, $FF);
      Result.TreeBg := MakeRGB($1C, $23, $2B);
      Result.TreeText := MakeRGB($F0, $F4, $F8);
      Result.NoteBg := MakeRGB($22, $2B, $35);
      Result.NoteText := MakeRGB($E0, $E8, $F0);
      Result.HeaderColor := MakeRGB($56, $B6, $C2);
      Result.VerseNumColor := MakeRGB($FF, $78, $78);
      Result.VerseHighlightBg := MakeRGB($2D, $3B, $4A);
    end;

    skSolarizedDark:
    begin
      Result.Id := skSolarizedDark;
      Result.Name := 'SolarizedDark';
      Result.DisplayName := 'Solarized Dark';
      Result.IsDark := True;
      Result.WindowBg := MakeRGB($00, $2B, $36);
      Result.WindowText := MakeRGB($93, $A1, $A1);
      Result.WindowTextDim := MakeRGB($78, $8D, $94);
      Result.PanelBg := MakeRGB($07, $36, $42);
      Result.PanelText := MakeRGB($93, $A1, $A1);
      Result.PanelBorder := MakeRGB($09, $4B, $5C);
      Result.PointedBg := MakeRGB($26, $8B, $D2);
      Result.PointedText := MakeRGB($FD, $F6, $E3);
      Result.HighlightBg := MakeRGB($B5, $89, $00);
      Result.HighlightText := MakeRGB($00, $2B, $36);
      Result.TreeBg := MakeRGB($00, $2B, $36);
      Result.TreeText := MakeRGB($FD, $F6, $E3);
      Result.NoteBg := MakeRGB($07, $36, $42);
      Result.NoteText := MakeRGB($93, $A1, $A1);
      Result.HeaderColor := MakeRGB($2A, $A1, $98);
      Result.VerseNumColor := MakeRGB($FF, $78, $78);
      Result.VerseHighlightBg := MakeRGB($07, $36, $42);
    end;

    skHighContrastDark:
    begin
      Result.Id := skHighContrastDark;
      Result.Name := 'HighContrastDark';
      Result.DisplayName := 'High Contrast Dark';
      Result.IsDark := True;
      Result.WindowBg := MakeRGB(0, 0, 0);
      Result.WindowText := MakeRGB(255, 255, 255);
      Result.WindowTextDim := MakeRGB(192, 192, 192);
      Result.PanelBg := MakeRGB(16, 16, 16);
      Result.PanelText := MakeRGB(255, 255, 255);
      Result.PanelBorder := MakeRGB(80, 80, 80);
      Result.PointedBg := MakeRGB(0, 120, 215);
      Result.PointedText := MakeRGB(255, 255, 255);
      Result.HighlightBg := MakeRGB(255, 255, 0);
      Result.HighlightText := MakeRGB(0, 0, 0);
      Result.TreeBg := MakeRGB(0, 0, 0);
      Result.TreeText := MakeRGB(255, 255, 255);
      Result.NoteBg := MakeRGB(24, 24, 24);
      Result.NoteText := MakeRGB(255, 255, 255);
      Result.HeaderColor := MakeRGB(0, 255, 255);
      Result.VerseNumColor := MakeRGB(255, 128, 0);
      Result.VerseHighlightBg := MakeRGB(64, 64, 64);
    end;
    else
      Result := GetSkin(skClassic);
  end;
end;

function SkinNameToId(const AName: string): TSkinId;
var
  lower: string;
begin
  lower := LowerCase(Trim(AName));
  if (lower = 'theworddark') or (lower = 'dark') or (lower = 'darkmode') or (lower = 'charcoal') then
    Result := skTheWordDark
  else if (lower = 'night') or (lower = 'black') or (lower = 'oled') then
    Result := skNight
  else if (lower = 'sepia') or (lower = 'parchment') or (lower = 'warm') then
    Result := skSepia
  else if (lower = 'slate') or (lower = 'steelblue') or (lower = 'blue') then
    Result := skSlate
  else if (lower = 'solarizeddark') or (lower = 'solarized') then
    Result := skSolarizedDark
  else if (lower = 'highcontrastdark') or (lower = 'highcontrast') then
    Result := skHighContrastDark
  else
    Result := skClassic;
end;

procedure SetCurrentSkin(AId: TSkinId);
begin
  CurrentSkin := GetSkin(AId);
end;

procedure SetCurrentSkinByName(const AName: string);
begin
  SetCurrentSkin(SkinNameToId(AName));
end;

procedure SetControlThemeMode(AControl: TWinControl; AIsDark: Boolean; const DarkTheme: string = '');
{$IFDEF WINDOWS}
var
  wTheme: WideString;
  wSpace: WideString;
{$ENDIF}
begin
  if not assigned(AControl) or not AControl.HandleAllocated then exit;

  {$IFDEF WINDOWS}
  InitUxTheme;
  if Assigned(fnAllowDarkModeForWindow) then
    fnAllowDarkModeForWindow(AControl.Handle, AIsDark);

  if Assigned(fnSetWindowTheme) then
  begin
    if AIsDark then
    begin
      if DarkTheme = '' then
      begin
        wSpace := ' ';
        fnSetWindowTheme(AControl.Handle, PWideChar(wSpace), PWideChar(wSpace));
      end
      else if DarkTheme = 'DarkMode_CFD' then
      begin
        wTheme := 'DarkMode_CFD';
        fnSetWindowTheme(AControl.Handle, PWideChar(wTheme), nil);
      end
      else
      begin
        wTheme := 'DarkMode_Explorer';
        fnSetWindowTheme(AControl.Handle, PWideChar(wTheme), nil);
      end;
    end
    else
    begin
      fnSetWindowTheme(AControl.Handle, nil, nil);
    end;
  end;
  AControl.Invalidate;
  {$ENDIF}
end;

procedure ApplyDarkModeToControl(AControl: TWinControl; AIsDark: Boolean);
begin
  if not assigned(AControl) then exit;
  if (AControl is TComboBox) or (AControl is TEdit) or (AControl is TMemo) then
    SetControlThemeMode(AControl, AIsDark, 'DarkMode_CFD')
  else if (AControl is TCustomTabControl) then
    SetControlThemeMode(AControl, AIsDark, '')
  else
    SetControlThemeMode(AControl, AIsDark, 'DarkMode_Explorer');
end;

procedure ApplyDarkModeToMenu(AMenu: TMenu; FormHandle: THandle; AIsDark: Boolean; ABackCol: TColor);
{$IFDEF WINDOWS}
var
  MenuInfo: TMenuInfo;
{$ENDIF}
begin
  if not assigned(AMenu) or (AMenu.Handle = 0) then exit;

  {$IFDEF WINDOWS}
  MenuInfo := Default(TMenuInfo);
  MenuInfo.cbSize := SizeOf(TMenuInfo);
  MenuInfo.fMask := MIM_BACKGROUND or MIM_APPLYTOSUBMENUS or $00000010 {MIM_STYLE};
  MenuInfo.dwStyle := $00000400 {MNS_CHECKORBMP};
  if AIsDark then
    MenuInfo.hbrBack := CreateSolidBrush(ColorToRGB(ABackCol))
  else
    MenuInfo.hbrBack := 0;

  SetMenuInfo(AMenu.Handle, @MenuInfo);
  if FormHandle <> 0 then
    DrawMenuBar(FormHandle);
  {$ENDIF}
end;

procedure ApplyDarkModeToWindow(AForm: TCustomForm; AIsDark: Boolean);
{$IFDEF WINDOWS}
const
  DWMWA_USE_IMMERSIVE_DARK_MODE_BEFORE_20H1 = 19;
  DWMWA_USE_IMMERSIVE_DARK_MODE = 20;
  DWMWA_CAPTION_COLOR = 35;
  DWMWA_TEXT_COLOR = 36;
var
  dwVal: DWORD;
  clrCap, clrTxt: COLORREF;
{$ENDIF}
begin
  if not assigned(AForm) then exit;

  {$IFDEF WINDOWS}
  InitUxTheme;
  if AIsDark then
    dwVal := 1
  else
    dwVal := 0;

  if Assigned(fnSetPreferredAppMode) then
  begin
    if AIsDark then
      fnSetPreferredAppMode(pamForceDark)
    else
      fnSetPreferredAppMode(pamDefault);
  end;

  if Assigned(fnFlushMenuThemes) then
    fnFlushMenuThemes();

  if Assigned(fnAllowDarkModeForWindow) and AForm.HandleAllocated then
    fnAllowDarkModeForWindow(AForm.Handle, AIsDark);

  if Assigned(fnDwmSetWindowAttribute) and AForm.HandleAllocated then
  begin
    fnDwmSetWindowAttribute(AForm.Handle, DWMWA_USE_IMMERSIVE_DARK_MODE, @dwVal, SizeOf(dwVal));
    fnDwmSetWindowAttribute(AForm.Handle, DWMWA_USE_IMMERSIVE_DARK_MODE_BEFORE_20H1, @dwVal, SizeOf(dwVal));

    if AIsDark then
    begin
      clrCap := ColorToRGB(CurrentSkin.PanelBg);
      clrTxt := ColorToRGB(CurrentSkin.PanelText);
      fnDwmSetWindowAttribute(AForm.Handle, DWMWA_CAPTION_COLOR, @clrCap, SizeOf(clrCap));
      fnDwmSetWindowAttribute(AForm.Handle, DWMWA_TEXT_COLOR, @clrTxt, SizeOf(clrTxt));
    end;
  end;

  if Assigned(fnSetWindowTheme) and AForm.HandleAllocated then
  begin
    if AIsDark then
      fnSetWindowTheme(AForm.Handle, PWideChar(WideString('DarkMode_Explorer')), nil)
    else
      fnSetWindowTheme(AForm.Handle, nil, nil);
  end;

  if (AForm is TForm) and Assigned(TForm(AForm).Menu) and AForm.HandleAllocated then
    ApplyDarkModeToMenu(TForm(AForm).Menu, AForm.Handle, AIsDark, CurrentSkin.PanelBg);
  {$ENDIF}
end;

var
  hbrCachedPanelBg: THandle = 0;
  cachedPanelBgColor: TColor = clNone;
  hbrCachedWindowBg: THandle = 0;
  cachedWindowBgColor: TColor = clNone;

function GetCachedPanelBrush(AColor: TColor): THandle;
begin
  if (hbrCachedPanelBg <> 0) and (cachedPanelBgColor = AColor) then
    Result := hbrCachedPanelBg
  else
  begin
    if hbrCachedPanelBg <> 0 then
      DeleteObject(hbrCachedPanelBg);
    cachedPanelBgColor := AColor;
    hbrCachedPanelBg := CreateSolidBrush(ColorToRGB(AColor));
    Result := hbrCachedPanelBg;
  end;
end;

function GetCachedWindowBrush(AColor: TColor): THandle;
begin
  if (hbrCachedWindowBg <> 0) and (cachedWindowBgColor = AColor) then
    Result := hbrCachedWindowBg
  else
  begin
    if hbrCachedWindowBg <> 0 then
      DeleteObject(hbrCachedWindowBg);
    cachedWindowBgColor := AColor;
    hbrCachedWindowBg := CreateSolidBrush(ColorToRGB(AColor));
    Result := hbrCachedWindowBg;
  end;
end;

{$IFDEF WINDOWS}
const
  TCM_FIRST = $1300;
  TCM_GETITEMCOUNT = TCM_FIRST + 4;
  TCM_GETITEMRECT  = TCM_FIRST + 10;
  TCM_GETCURSEL    = TCM_FIRST + 11;
  TCM_GETITEMW     = TCM_FIRST + 60;
  TCIF_TEXT        = $0001;

type
  TTCItemW = record
    mask: UINT;
    dwState: DWORD;
    dwStateMask: DWORD;
    pszText: LPWSTR;
    cchTextMax: Integer;
    iImage: Integer;
    lParam: LPARAM;
  end;

function UniversalTabSubclassProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM;
  uIdSubclass: PtrUInt; dwRefData: PtrUInt): LRESULT; stdcall;
var
  hdc, memDC: THandle;
  memBmp, oldBmp: THandle;
  rcClient, rcTab, rcText, rcLine, rcTopAccent, rcBottom: RECT;
  i, count, curSel, tabHeaderBottom: Integer;
  tabItem: TTCItemW;
  txtBuf: array[0..255] of WideChar;
  hbr, hbrActive, hbrBorder, hbrHighlight: THandle;
  hFont, oldFont: THandle;
  oldBk: Integer;
  wTab: WideString;
  txtLen: Integer;
begin
  if (uMsg = WM_PAINT) and CurrentSkin.IsDark then
  begin
    // 1. Let default window procedure paint
    Result := fnDefSubclassProc(hWnd, uMsg, wParam, lParam);

    // 2. Overpaint with double-buffered dark theme graphics
    hdc := GetDC(hWnd);
    if hdc <> 0 then
    begin
      FillChar(rcClient, SizeOf(rcClient), 0);
      GetClientRect(hWnd, @rcClient);

      count := SendMessage(hWnd, TCM_GETITEMCOUNT, 0, 0);
      curSel := SendMessage(hWnd, TCM_GETCURSEL, 0, 0);

      if (dwRefData <> 0) and (TObject(dwRefData) is TTabControl) then
      begin
        if count <= 0 then
          count := TTabControl(dwRefData).Tabs.Count;
        if curSel < 0 then
          curSel := TTabControl(dwRefData).TabIndex;
      end
      else if (dwRefData <> 0) and (TObject(dwRefData) is TPageControl) then
      begin
        if count <= 0 then
          count := TPageControl(dwRefData).PageCount;
        if curSel < 0 then
          curSel := TPageControl(dwRefData).ActivePageIndex;
      end;

      tabHeaderBottom := 26;
      for i := 0 to count - 1 do
      begin
        FillChar(rcTab, SizeOf(rcTab), 0);
        if SendMessage(hWnd, TCM_GETITEMRECT, i, PtrInt(@rcTab)) <> 0 then
        begin
          if rcTab.Bottom > tabHeaderBottom then
            tabHeaderBottom := rcTab.Bottom;
        end;
      end;

      memDC := CreateCompatibleDC(hdc);
      memBmp := CreateCompatibleBitmap(hdc, rcClient.Right - rcClient.Left, tabHeaderBottom + 4);
      oldBmp := SelectObject(memDC, memBmp);

      // Fill entire tab header background with PanelBg
      hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.PanelBg));
      FillRect(memDC, rcClient, hbr);
      DeleteObject(hbr);

      // Draw subtle border line below tab strip
      rcLine.Left := rcClient.Left;
      rcLine.Right := rcClient.Right;
      rcLine.Top := tabHeaderBottom + 1;
      rcLine.Bottom := tabHeaderBottom + 2;
      hbrBorder := CreateSolidBrush(ColorToRGB(CurrentSkin.PanelBorder));
      FillRect(memDC, rcLine, hbrBorder);

      hFont := THandle(SendMessage(hWnd, WM_GETFONT, 0, 0));
      if (hFont = 0) and (dwRefData <> 0) and (TObject(dwRefData) is TControl) then
        hFont := THandle(TControl(dwRefData).Font.Handle);
      if hFont = 0 then
        hFont := GetStockObject(DEFAULT_GUI_FONT);

      if hFont <> 0 then
        oldFont := SelectObject(memDC, hFont)
      else
        oldFont := 0;

      oldBk := SetBkMode(memDC, TRANSPARENT);
      hbrActive := CreateSolidBrush(ColorToRGB(CurrentSkin.PointedBg));
      hbrHighlight := CreateSolidBrush(ColorToRGB(CurrentSkin.HighlightBg));

      // Render each tab button
      for i := 0 to count - 1 do
      begin
        FillChar(rcTab, SizeOf(rcTab), 0);
        if SendMessage(hWnd, TCM_GETITEMRECT, i, PtrInt(@rcTab)) = 0 then
        begin
          rcTab.Left := 2 + i * 80;
          rcTab.Right := rcTab.Left + 78;
          rcTab.Top := 2;
          rcTab.Bottom := tabHeaderBottom;
        end;

        FillChar(txtBuf, SizeOf(txtBuf), 0);
        txtLen := 0;

        if (dwRefData <> 0) and (TObject(dwRefData) is TTabControl) then
        begin
          if (i >= 0) and (i < TTabControl(dwRefData).Tabs.Count) then
          begin
            wTab := WideString(TTabControl(dwRefData).Tabs[i]);
            txtLen := Min(255, Length(wTab));
            if txtLen > 0 then
            begin
              Move(wTab[1], txtBuf[0], txtLen * SizeOf(WideChar));
              txtBuf[txtLen] := #0;
            end;
          end;
        end
        else if (dwRefData <> 0) and (TObject(dwRefData) is TPageControl) then
        begin
          if (i >= 0) and (i < TPageControl(dwRefData).PageCount) then
          begin
            wTab := WideString(TPageControl(dwRefData).Pages[i].Caption);
            txtLen := Min(255, Length(wTab));
            if txtLen > 0 then
            begin
              Move(wTab[1], txtBuf[0], txtLen * SizeOf(WideChar));
              txtBuf[txtLen] := #0;
            end;
          end;
        end;

        if txtLen = 0 then
        begin
          FillChar(tabItem, SizeOf(tabItem), 0);
          tabItem.mask := TCIF_TEXT;
          tabItem.pszText := @txtBuf[0];
          tabItem.cchTextMax := 255;
          SendMessage(hWnd, TCM_GETITEMW, i, PtrInt(@tabItem));
          txtLen := lstrlenW(@txtBuf[0]);
        end;

        if i = curSel then
        begin
          // Active tab: PointedBg with bright white text
          FillRect(memDC, rcTab, hbrActive);

          // Top accent line
          rcTopAccent := rcTab;
          rcTopAccent.Bottom := rcTopAccent.Top + 2;
          FillRect(memDC, rcTopAccent, hbrHighlight);

          // Merge active tab bottom into client area
          rcBottom := rcTab;
          rcBottom.Top := rcTab.Bottom - 1;
          rcBottom.Bottom := rcTab.Bottom + 2;
          FillRect(memDC, rcBottom, hbrActive);

          SetTextColor(memDC, ColorToRGB(CurrentSkin.PointedText));
        end
        else
        begin
          // Inactive tab: PanelBg with PanelText
          hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.PanelBg));
          FillRect(memDC, rcTab, hbr);
          DeleteObject(hbr);

          SetTextColor(memDC, ColorToRGB(CurrentSkin.PanelText));
        end;

        FrameRect(memDC, rcTab, hbrBorder);

        rcText := rcTab;
        DrawTextW(memDC, @txtBuf[0], txtLen, rcText,
          DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
      end;

      DeleteObject(hbrActive);
      DeleteObject(hbrHighlight);
      DeleteObject(hbrBorder);

      SetBkMode(memDC, oldBk);
      if oldFont <> 0 then
        SelectObject(memDC, oldFont);

      BitBlt(hdc, rcClient.Left, rcClient.Top,
             rcClient.Right - rcClient.Left, tabHeaderBottom + 4,
             memDC, 0, 0, SRCCOPY);

      SelectObject(memDC, oldBmp);
      DeleteObject(memBmp);
      DeleteDC(memDC);

      ReleaseDC(hWnd, hdc);
    end;
    Exit;
  end;

  if (uMsg = WM_ERASEBKGND) and CurrentSkin.IsDark then
  begin
    FillChar(rcClient, SizeOf(rcClient), 0);
    GetClientRect(hWnd, @rcClient);
    hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.PanelBg));
    FillRect(THandle(wParam), rcClient, hbr);
    DeleteObject(hbr);
    Result := 1;
    Exit;
  end;

  if ((uMsg = WM_CTLCOLORSTATIC) or (uMsg = WM_CTLCOLORBTN)) and CurrentSkin.IsDark then
  begin
    SetBkMode(THandle(wParam), TRANSPARENT);
    SetTextColor(THandle(wParam), ColorToRGB(CurrentSkin.PanelText));
    Result := LRESULT(GetCachedPanelBrush(CurrentSkin.PanelBg));
    Exit;
  end;

  if (uMsg = WM_CTLCOLOREDIT) and CurrentSkin.IsDark then
  begin
    SetBkColor(THandle(wParam), ColorToRGB(CurrentSkin.WindowBg));
    SetTextColor(THandle(wParam), ColorToRGB(CurrentSkin.WindowText));
    Result := LRESULT(GetCachedWindowBrush(CurrentSkin.WindowBg));
    Exit;
  end;



  if ((uMsg = WM_LBUTTONDOWN) or (uMsg = WM_LBUTTONUP) or (uMsg = WM_MOUSEMOVE)) and CurrentSkin.IsDark then
  begin
    Result := fnDefSubclassProc(hWnd, uMsg, wParam, lParam);
    InvalidateRect(hWnd, nil, FALSE);
    Exit;
  end;

  Result := fnDefSubclassProc(hWnd, uMsg, wParam, lParam);
end;

procedure HookTabControl(AControl: TWinControl);
begin
  if not Assigned(fnSetWindowSubclass) then exit;
  if Assigned(AControl) then
  begin
    AControl.HandleNeeded;
    if AControl.Handle <> 0 then
    begin
      fnSetWindowSubclass(AControl.Handle, @UniversalTabSubclassProc, 4001, PtrUInt(AControl));
      AControl.Invalidate;
    end;
  end;
end;

procedure UnhookTabControl(AControl: TWinControl);
begin
  if not Assigned(fnRemoveWindowSubclass) then exit;
  if Assigned(AControl) and (AControl.Handle <> 0) then
    fnRemoveWindowSubclass(AControl.Handle, @UniversalTabSubclassProc, 4001);
end;

function FormSubclassProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM;
  uIdSubclass: PtrUInt; dwRefData: PtrUInt): LRESULT; stdcall;
var
  rc: RECT;
  hbr: THandle;
begin
  if (uMsg = WM_ERASEBKGND) and CurrentSkin.IsDark then
  begin
    FillChar(rc, SizeOf(rc), 0);
    GetClientRect(hWnd, @rc);
    hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.PanelBg));
    FillRect(THandle(wParam), rc, hbr);
    DeleteObject(hbr);
    Result := 1;
    Exit;
  end;

  if ((uMsg = WM_CTLCOLORSTATIC) or (uMsg = WM_CTLCOLORBTN)) and CurrentSkin.IsDark then
  begin
    SetBkMode(THandle(wParam), TRANSPARENT);
    SetTextColor(THandle(wParam), ColorToRGB(CurrentSkin.PanelText));
    Result := LRESULT(GetCachedPanelBrush(CurrentSkin.PanelBg));
    Exit;
  end;

  if (uMsg = WM_CTLCOLOREDIT) and CurrentSkin.IsDark then
  begin
    SetBkColor(THandle(wParam), ColorToRGB(CurrentSkin.WindowBg));
    SetTextColor(THandle(wParam), ColorToRGB(CurrentSkin.WindowText));
    Result := LRESULT(GetCachedWindowBrush(CurrentSkin.WindowBg));
    Exit;
  end;

  Result := fnDefSubclassProc(hWnd, uMsg, wParam, lParam);
end;

procedure HookFormControl(AControl: TWinControl);
begin
  {$IFDEF WINDOWS}
  if not Assigned(fnSetWindowSubclass) then exit;
  if Assigned(AControl) and (AControl.Handle <> 0) then
  begin
    fnSetWindowSubclass(AControl.Handle, @FormSubclassProc, 3001, PtrUInt(AControl));
    AControl.Invalidate;
  end;
  {$ENDIF}
end;

procedure UnhookFormControl(AControl: TWinControl);
begin
  {$IFDEF WINDOWS}
  if not Assigned(fnRemoveWindowSubclass) then exit;
  if Assigned(AControl) and (AControl.Handle <> 0) then
    fnRemoveWindowSubclass(AControl.Handle, @FormSubclassProc, 3001);
  {$ENDIF}
end;

function ButtonSubclassProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM;
  uIdSubclass: PtrUInt; dwRefData: PtrUInt): LRESULT; stdcall;
var
  ps: PAINTSTRUCT;
  hdc: THandle;
  rc, rcText: RECT;
  btnText: array[0..255] of WideChar;
  btnStr: WideString;
  txtLen: Integer;
  hbr: THandle;
  hFont, oldFont: THandle;
  oldBk: Integer;
  oldCol: COLORREF;
  isHot, isPushed, isDis: Boolean;
  btnState: LRESULT;
  pt: TPoint;
begin
  if (uMsg = WM_PAINT) and CurrentSkin.IsDark then
  begin
    hdc := BeginPaint(hWnd, @ps);
    if hdc <> 0 then
    begin
      FillChar(rc, SizeOf(rc), 0);
      GetClientRect(hWnd, @rc);

      isDis := not IsWindowEnabled(hWnd);

      btnState := SendMessage(hWnd, BM_GETSTATE, 0, 0);
      isPushed := (btnState and BST_PUSHED) <> 0;

      GetCursorPos(@pt);
      ScreenToClient(hWnd, @pt);
      isHot := (pt.x >= rc.Left) and (pt.x <= rc.Right) and (pt.y >= rc.Top) and (pt.y <= rc.Bottom) and not isDis;

      if isPushed then
        hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.HighlightBg))
      else if isHot then
        hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.PointedBg))
      else
        hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.PanelBg));

      FillRect(hdc, rc, hbr);
      DeleteObject(hbr);

      if isHot or isPushed then
        hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.HighlightBg))
      else
        hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.PanelBorder));
      FrameRect(hdc, rc, hbr);
      DeleteObject(hbr);

      FillChar(btnText, SizeOf(btnText), 0);
      txtLen := GetWindowTextW(hWnd, @btnText[0], 255);
      if (txtLen = 0) and (dwRefData <> 0) and (TObject(dwRefData) is TControl) then
      begin
        txtLen := Min(255, Length(TControl(dwRefData).Caption));
        if txtLen > 0 then
        begin
          btnStr := WideString(TControl(dwRefData).Caption);
          txtLen := Min(255, Length(btnStr));
          Move(btnStr[1], btnText[0], txtLen * SizeOf(WideChar));
          btnText[txtLen] := #0;
        end;
      end;

      if txtLen > 0 then
      begin
        hFont := THandle(SendMessage(hWnd, WM_GETFONT, 0, 0));
        if (hFont = 0) and (dwRefData <> 0) and (TObject(dwRefData) is TControl) then
          hFont := THandle(TControl(dwRefData).Font.Handle);

        if hFont <> 0 then
          oldFont := SelectObject(hdc, hFont)
        else
          oldFont := 0;

        oldBk := SetBkMode(hdc, TRANSPARENT);
        if isDis then
          oldCol := SetTextColor(hdc, ColorToRGB(CurrentSkin.WindowTextDim))
        else if isHot or isPushed then
          oldCol := SetTextColor(hdc, ColorToRGB(CurrentSkin.PointedText))
        else
          oldCol := SetTextColor(hdc, ColorToRGB(CurrentSkin.PanelText));

        rcText := rc;
        DrawTextW(hdc, @btnText[0], txtLen, rcText,
          DT_CENTER or DT_VCENTER or DT_SINGLELINE);

        SetTextColor(hdc, oldCol);
        SetBkMode(hdc, oldBk);
        if oldFont <> 0 then
          SelectObject(hdc, oldFont);
      end;

      EndPaint(hWnd, @ps);
    end;
    Result := 0;
    Exit;
  end;

  if (uMsg = WM_ERASEBKGND) and CurrentSkin.IsDark then
  begin
    FillChar(rc, SizeOf(rc), 0);
    GetClientRect(hWnd, @rc);
    hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.PanelBg));
    FillRect(THandle(wParam), rc, hbr);
    DeleteObject(hbr);
    Result := 1;
    Exit;
  end;

  if ((uMsg = WM_MOUSEMOVE) or (uMsg = WM_LBUTTONDOWN) or (uMsg = WM_LBUTTONUP)) and CurrentSkin.IsDark then
  begin
    Result := fnDefSubclassProc(hWnd, uMsg, wParam, lParam);
    InvalidateRect(hWnd, nil, FALSE);
    Exit;
  end;

  Result := fnDefSubclassProc(hWnd, uMsg, wParam, lParam);
end;

procedure HookButtonControl(AControl: TWinControl);
begin
  if not Assigned(fnSetWindowSubclass) then exit;
  if Assigned(AControl) and (AControl.Handle <> 0) then
  begin
    fnSetWindowSubclass(AControl.Handle, @ButtonSubclassProc, 5001, PtrUInt(AControl));
    AControl.Invalidate;
  end;
end;

procedure UnhookButtonControl(AControl: TWinControl);
begin
  if not Assigned(fnRemoveWindowSubclass) then exit;
  if Assigned(AControl) and (AControl.Handle <> 0) then
    fnRemoveWindowSubclass(AControl.Handle, @ButtonSubclassProc, 5001);
end;

function CheckBoxSubclassProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM;
  uIdSubclass: PtrUInt; dwRefData: PtrUInt): LRESULT; stdcall;
var
  ps: PAINTSTRUCT;
  hdc: THandle;
  rc, rcText, boxRc, rcFocus, rcIndet: RECT;
  chkText: array[0..255] of WideChar;
  chkStr: WideString;
  txtLen: Integer;
  hbr, hPen, oldPen, hStockBr, oldBr: THandle;
  hFont, oldFont: THandle;
  oldBk: Integer;
  oldCol: COLORREF;
  isHot, isPushed, isDis, isChecked, isIndet: Boolean;
  chkState: LRESULT;
  pt: TPoint;
  boxSize, boxTop, boxLeft: Integer;
begin
  if (uMsg = WM_PAINT) and CurrentSkin.IsDark then
  begin
    hdc := BeginPaint(hWnd, @ps);
    if hdc <> 0 then
    begin
      FillChar(rc, SizeOf(rc), 0);
      GetClientRect(hWnd, @rc);

      isDis := not IsWindowEnabled(hWnd);

      // Check state: BM_GETCHECK ($00F0) returns BST_CHECKED (1) or BST_UNCHECKED (0)
      chkState := SendMessage(hWnd, BM_GETCHECK, 0, 0);
      isChecked := (chkState = BST_CHECKED);
      isIndet := (chkState = BST_INDETERMINATE);

      chkState := SendMessage(hWnd, BM_GETSTATE, 0, 0);
      isPushed := (chkState and BST_PUSHED) <> 0;

      GetCursorPos(@pt);
      ScreenToClient(hWnd, @pt);
      isHot := (pt.x >= rc.Left) and (pt.x <= rc.Right) and (pt.y >= rc.Top) and (pt.y <= rc.Bottom) and not isDis;

      // 1. Fill entire control background with CurrentSkin.PanelBg (Dark Theme Background)
      hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.PanelBg));
      FillRect(hdc, rc, hbr);
      DeleteObject(hbr);

      // 2. Checkbox Square Box
      boxSize := 13;
      boxLeft := rc.Left + 1;
      boxTop := rc.Top + (rc.Bottom - rc.Top - boxSize) div 2;
      boxRc.Left := boxLeft;
      boxRc.Top := boxTop;
      boxRc.Right := boxLeft + boxSize;
      boxRc.Bottom := boxTop + boxSize;

      // Box interior
      if isDis then
        hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.PanelBg))
      else if isPushed then
        hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.HighlightBg))
      else if isChecked then
        hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.PanelBg))
      else if isHot then
        hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.PointedBg))
      else
        hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.PanelBg));
      FillRect(hdc, boxRc, hbr);
      DeleteObject(hbr);

      // Box border
      if isDis then
        hPen := CreatePen(PS_SOLID, 1, ColorToRGB(CurrentSkin.WindowTextDim))
      else if isChecked then
        hPen := CreatePen(PS_SOLID, 1, RGB(96, 205, 255))
      else if isHot then
        hPen := CreatePen(PS_SOLID, 1, ColorToRGB(CurrentSkin.PanelText))
      else
        hPen := CreatePen(PS_SOLID, 1, ColorToRGB(CurrentSkin.PanelBorder));
      oldPen := SelectObject(hdc, hPen);
      hStockBr := GetStockObject(HOLLOW_BRUSH);
      oldBr := SelectObject(hdc, hStockBr);
      Rectangle(hdc, boxRc.Left, boxRc.Top, boxRc.Right, boxRc.Bottom);
      SelectObject(hdc, oldBr);
      SelectObject(hdc, oldPen);
      DeleteObject(hPen);

      // Draw Checkmark if Checked
      if isChecked then
      begin
        if isDis then
          hPen := CreatePen(PS_SOLID, 2, ColorToRGB(CurrentSkin.WindowTextDim))
        else
          hPen := CreatePen(PS_SOLID, 2, RGB(96, 205, 255)); // Bright cyan checkmark
        oldPen := SelectObject(hdc, hPen);
        MoveToEx(hdc, boxLeft + 2, boxTop + 6, nil);
        LineTo(hdc, boxLeft + 5, boxTop + 9);
        LineTo(hdc, boxLeft + 10, boxTop + 3);
        SelectObject(hdc, oldPen);
        DeleteObject(hPen);
      end
      else if isIndet then
      begin
        rcIndet.Left := boxLeft + 3;
        rcIndet.Top := boxTop + 3;
        rcIndet.Right := boxLeft + boxSize - 3;
        rcIndet.Bottom := boxTop + boxSize - 3;
        hbr := CreateSolidBrush(RGB(96, 205, 255));
        FillRect(hdc, rcIndet, hbr);
        DeleteObject(hbr);
      end;

      // 3. Caption Text
      FillChar(chkText, SizeOf(chkText), 0);
      txtLen := GetWindowTextW(hWnd, @chkText[0], 255);
      if (txtLen = 0) and (dwRefData <> 0) and (TObject(dwRefData) is TControl) then
      begin
        txtLen := Min(255, Length(TControl(dwRefData).Caption));
        if txtLen > 0 then
        begin
          chkStr := WideString(TControl(dwRefData).Caption);
          txtLen := Min(255, Length(chkStr));
          Move(chkStr[1], chkText[0], txtLen * SizeOf(WideChar));
          chkText[txtLen] := #0;
        end;
      end;

      if txtLen > 0 then
      begin
        hFont := 0;
        if (dwRefData <> 0) and (TObject(dwRefData) is TControl) then
          hFont := THandle(TControl(dwRefData).Font.Handle);
        if hFont = 0 then
          hFont := THandle(SendMessage(hWnd, WM_GETFONT, 0, 0));

        if hFont <> 0 then
          oldFont := SelectObject(hdc, hFont)
        else
          oldFont := 0;

        oldBk := SetBkMode(hdc, TRANSPARENT);
        if isDis then
          oldCol := SetTextColor(hdc, ColorToRGB(CurrentSkin.WindowTextDim))
        else
          oldCol := SetTextColor(hdc, ColorToRGB(CurrentSkin.PanelText));

        rcText := rc;
        rcText.Left := boxRc.Right + 5;
        DrawTextW(hdc, @chkText[0], txtLen, rcText,
          DT_LEFT or DT_VCENTER or DT_SINGLELINE);

        if (GetFocus() = hWnd) and not isDis then
        begin
          rcFocus := rcText;
          DrawTextW(hdc, @chkText[0], txtLen, rcFocus,
            DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_CALCRECT);
          InflateRect(rcFocus, 1, 1);
          DrawFocusRect(hdc, rcFocus);
        end;

        SetTextColor(hdc, oldCol);
        SetBkMode(hdc, oldBk);
        if oldFont <> 0 then
          SelectObject(hdc, oldFont);
      end;

      EndPaint(hWnd, @ps);
    end;
    Result := 0;
    Exit;
  end;

  if (uMsg = WM_ERASEBKGND) and CurrentSkin.IsDark then
  begin
    FillChar(rc, SizeOf(rc), 0);
    GetClientRect(hWnd, @rc);
    hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.PanelBg));
    FillRect(THandle(wParam), rc, hbr);
    DeleteObject(hbr);
    Result := 1;
    Exit;
  end;

  if ((uMsg = WM_MOUSEMOVE) or (uMsg = WM_LBUTTONDOWN) or (uMsg = WM_LBUTTONUP) or
      (uMsg = WM_SETFOCUS) or (uMsg = WM_KILLFOCUS) or (uMsg = BM_SETCHECK) or
      (uMsg = WM_ENABLE) or (uMsg = WM_KEYUP)) and CurrentSkin.IsDark then
  begin
    Result := fnDefSubclassProc(hWnd, uMsg, wParam, lParam);
    InvalidateRect(hWnd, nil, FALSE);
    Exit;
  end;

  Result := fnDefSubclassProc(hWnd, uMsg, wParam, lParam);
end;

procedure HookCheckBoxControl(AControl: TWinControl);
begin
  if not Assigned(fnSetWindowSubclass) then exit;
  if Assigned(AControl) and (AControl.Handle <> 0) then
  begin
    fnSetWindowSubclass(AControl.Handle, @CheckBoxSubclassProc, 6001, PtrUInt(AControl));
    AControl.Invalidate;
  end;
end;

procedure UnhookCheckBoxControl(AControl: TWinControl);
begin
  if not Assigned(fnRemoveWindowSubclass) then exit;
  if Assigned(AControl) and (AControl.Handle <> 0) then
    fnRemoveWindowSubclass(AControl.Handle, @CheckBoxSubclassProc, 6001);
end;

function GroupBoxSubclassProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM;
  uIdSubclass: PtrUInt; dwRefData: PtrUInt): LRESULT; stdcall;
var
  rc, rcText, rcCalc, rcFill: RECT;
  grpText: array[0..255] of WideChar;
  grpStr: WideString;
  txtLen: Integer;
  hdc: THandle;
  hbr: THandle;
  hFont, oldFont: THandle;
  oldBk: Integer;
  oldCol: COLORREF;
  isDis: Boolean;
  textW, textH: Integer;
begin
  if (uMsg = WM_PAINT) and CurrentSkin.IsDark then
  begin
    // 1. Let default window procedure paint the groupbox border line and gap
    Result := fnDefSubclassProc(hWnd, uMsg, wParam, lParam);

    // 2. Overpaint the caption text in light CurrentSkin.PanelText
    hdc := GetDC(hWnd);
    if hdc <> 0 then
    begin
      FillChar(rc, SizeOf(rc), 0);
      GetClientRect(hWnd, @rc);

      isDis := not IsWindowEnabled(hWnd);

      FillChar(grpText, SizeOf(grpText), 0);
      txtLen := GetWindowTextW(hWnd, @grpText[0], 255);
      if (txtLen = 0) and (dwRefData <> 0) and (TObject(dwRefData) is TControl) then
      begin
        txtLen := Min(255, Length(TControl(dwRefData).Caption));
        if txtLen > 0 then
        begin
          grpStr := WideString(TControl(dwRefData).Caption);
          txtLen := Min(255, Length(grpStr));
          Move(grpStr[1], grpText[0], txtLen * SizeOf(WideChar));
          grpText[txtLen] := #0;
        end;
      end;

      if txtLen > 0 then
      begin
        hFont := 0;
        if (dwRefData <> 0) and (TObject(dwRefData) is TControl) then
          hFont := THandle(TControl(dwRefData).Font.Handle);
        if hFont = 0 then
          hFont := THandle(SendMessage(hWnd, WM_GETFONT, 0, 0));

        if hFont <> 0 then
          oldFont := SelectObject(hdc, hFont)
        else
          oldFont := 0;

        // Calculate text extent
        rcCalc.Left := 9;
        rcCalc.Top := 0;
        rcCalc.Right := rc.Right;
        rcCalc.Bottom := 100;
        DrawTextW(hdc, @grpText[0], txtLen, rcCalc,
          DT_LEFT or DT_TOP or DT_SINGLELINE or DT_CALCRECT);
        textW := rcCalc.Right - rcCalc.Left;
        textH := rcCalc.Bottom - rcCalc.Top;

        // Erase any dark text drawn by Windows with CurrentSkin.PanelBg
        rcFill.Left := 8;
        rcFill.Top := 0;
        rcFill.Right := 9 + textW + 2;
        rcFill.Bottom := textH;
        hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.PanelBg));
        FillRect(hdc, rcFill, hbr);
        DeleteObject(hbr);

        // Draw caption in light CurrentSkin.PanelText
        oldBk := SetBkMode(hdc, TRANSPARENT);
        if isDis then
          oldCol := SetTextColor(hdc, ColorToRGB(CurrentSkin.WindowTextDim))
        else
          oldCol := SetTextColor(hdc, ColorToRGB(CurrentSkin.PanelText));

        rcText.Left := 9;
        rcText.Top := 0;
        rcText.Right := 9 + textW;
        rcText.Bottom := textH;
        DrawTextW(hdc, @grpText[0], txtLen, rcText,
          DT_LEFT or DT_TOP or DT_SINGLELINE);

        SetTextColor(hdc, oldCol);
        SetBkMode(hdc, oldBk);
        if oldFont <> 0 then
          SelectObject(hdc, oldFont);
      end;

      ReleaseDC(hWnd, hdc);
    end;
    Exit;
  end;

  if (uMsg = WM_ERASEBKGND) and CurrentSkin.IsDark then
  begin
    FillChar(rc, SizeOf(rc), 0);
    GetClientRect(hWnd, @rc);
    hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.PanelBg));
    FillRect(THandle(wParam), rc, hbr);
    DeleteObject(hbr);
    Result := 1;
    Exit;
  end;

  if (uMsg = WM_ENABLE) and CurrentSkin.IsDark then
  begin
    Result := fnDefSubclassProc(hWnd, uMsg, wParam, lParam);
    InvalidateRect(hWnd, nil, FALSE);
    Exit;
  end;

  Result := fnDefSubclassProc(hWnd, uMsg, wParam, lParam);
end;

procedure HookGroupBoxControl(AControl: TWinControl);
begin
  if not Assigned(fnSetWindowSubclass) then exit;
  if Assigned(AControl) and (AControl.Handle <> 0) then
  begin
    fnSetWindowSubclass(AControl.Handle, @GroupBoxSubclassProc, 7001, PtrUInt(AControl));
    AControl.Invalidate;
  end;
end;

procedure UnhookGroupBoxControl(AControl: TWinControl);
begin
  if not Assigned(fnRemoveWindowSubclass) then exit;
  if Assigned(AControl) and (AControl.Handle <> 0) then
    fnRemoveWindowSubclass(AControl.Handle, @GroupBoxSubclassProc, 7001);
end;


function ScrollBoxSubclassProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM;
  uIdSubclass: PtrUInt; dwRefData: PtrUInt): LRESULT; stdcall;
var
  rc: RECT;
  hbr: THandle;
begin
  if (uMsg = WM_ERASEBKGND) and CurrentSkin.IsDark then
  begin
    FillChar(rc, SizeOf(rc), 0);
    GetClientRect(hWnd, @rc);
    hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.WindowBg));
    FillRect(THandle(wParam), rc, hbr);
    DeleteObject(hbr);
    Result := 1;
    Exit;
  end;

  Result := fnDefSubclassProc(hWnd, uMsg, wParam, lParam);
end;

procedure HookScrollBoxControl(AControl: TWinControl);
begin
  if not Assigned(fnSetWindowSubclass) then exit;
  if Assigned(AControl) and (AControl.Handle <> 0) then
  begin
    fnSetWindowSubclass(AControl.Handle, @ScrollBoxSubclassProc, 7001, PtrUInt(AControl));
    AControl.Invalidate;
  end;
end;

procedure UnhookScrollBoxControl(AControl: TWinControl);
begin
  if not Assigned(fnRemoveWindowSubclass) then exit;
  if Assigned(AControl) and (AControl.Handle <> 0) then
    fnRemoveWindowSubclass(AControl.Handle, @ScrollBoxSubclassProc, 7001);
end;

function ToolBarSubclassProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM;
  uIdSubclass: PtrUInt; dwRefData: PtrUInt): LRESULT; stdcall;
var
  rc: RECT;
  hbr: THandle;
begin
  if (uMsg = WM_ERASEBKGND) and CurrentSkin.IsDark then
  begin
    FillChar(rc, SizeOf(rc), 0);
    GetClientRect(hWnd, @rc);
    hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.PanelBg));
    FillRect(THandle(wParam), rc, hbr);
    DeleteObject(hbr);
    Result := 1;
    Exit;
  end;

  Result := fnDefSubclassProc(hWnd, uMsg, wParam, lParam);
end;

procedure HookToolBarControl(AControl: TWinControl);
begin
  if not Assigned(fnSetWindowSubclass) then exit;
  if Assigned(AControl) and (AControl.Handle <> 0) then
  begin
    fnSetWindowSubclass(AControl.Handle, @ToolBarSubclassProc, 8001, PtrUInt(AControl));
    AControl.Invalidate;
  end;
end;

procedure UnhookToolBarControl(AControl: TWinControl);
begin
  if not Assigned(fnRemoveWindowSubclass) then exit;
  if Assigned(AControl) and (AControl.Handle <> 0) then
    fnRemoveWindowSubclass(AControl.Handle, @ToolBarSubclassProc, 8001);
end;

function TreeViewSubclassProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM;
  uIdSubclass: PtrUInt; dwRefData: PtrUInt): LRESULT; stdcall;
var
  rc: RECT;
  hbr: THandle;
begin
  if (uMsg = WM_ERASEBKGND) and CurrentSkin.IsDark then
  begin
    FillChar(rc, SizeOf(rc), 0);
    GetClientRect(hWnd, @rc);
    hbr := CreateSolidBrush(ColorToRGB(CurrentSkin.TreeBg));
    FillRect(THandle(wParam), rc, hbr);
    DeleteObject(hbr);
    Result := 1;
    Exit;
  end;

  Result := fnDefSubclassProc(hWnd, uMsg, wParam, lParam);
end;

procedure HookTreeViewControl(AControl: TWinControl);
begin
  if not Assigned(fnSetWindowSubclass) then exit;
  if Assigned(AControl) and (AControl.Handle <> 0) then
  begin
    fnSetWindowSubclass(AControl.Handle, @TreeViewSubclassProc, 9001, PtrUInt(AControl));
    SendMessage(AControl.Handle, $111D, 0, ColorToRGB(CurrentSkin.TreeBg));
    SendMessage(AControl.Handle, $111E, 0, ColorToRGB(CurrentSkin.TreeText));
    SendMessage(AControl.Handle, $1128, 0, ColorToRGB(CurrentSkin.PanelBorder));
    AControl.Invalidate;
  end;
end;

procedure UnhookTreeViewControl(AControl: TWinControl);
begin
  if not Assigned(fnRemoveWindowSubclass) then exit;
  if Assigned(AControl) and (AControl.Handle <> 0) then
    fnRemoveWindowSubclass(AControl.Handle, @TreeViewSubclassProc, 9001);
end;

const
  CDDS_PREPAINT_CODE = $00000001;
  CDDS_ITEM_CODE = $00010000;
  CDDS_ITEMPREPAINT_CODE = CDDS_ITEM_CODE or CDDS_PREPAINT_CODE;
  CDRF_DODEFAULT_CODE = $00000000;
  CDRF_NOTIFYITEMDRAW_CODE = $00000020;
  CDIS_SELECTED_CODE = $0001;
  CDIS_HOT_CODE = $0040;
  CDIS_CHECKED_CODE = $0008;
  TBCDRF_NOEDGES_CODE = $00010000;

type
  TNMCustomDrawToolbar = record
    hdr: NMHDR;
    dwDrawStage: DWORD;
    hdc: THandle;
    rc: RECT;
    dwItemSpec: PtrUInt;
    uItemState: UINT;
    lItemlParam: LPARAM;
  end;
  PNMCustomDrawToolbar = ^TNMCustomDrawToolbar;

function HandleToolBarCustomDraw(lParam: LPARAM; const ASkin: TSkinColors; var AHandled: Boolean): LRESULT;
var
  pnm: PNMCustomDrawToolbar;
  hbr: THandle;
begin
  Result := CDRF_DODEFAULT_CODE;
  AHandled := False;
  if not ASkin.IsDark then exit;

  pnm := PNMCustomDrawToolbar(lParam);
  if pnm = nil then exit;

  if pnm^.dwDrawStage = CDDS_PREPAINT_CODE then
  begin
    Result := CDRF_NOTIFYITEMDRAW_CODE;
    AHandled := True;
    Exit;
  end;

  if pnm^.dwDrawStage = CDDS_ITEMPREPAINT_CODE then
  begin
    if (pnm^.uItemState and CDIS_SELECTED_CODE) <> 0 then
      hbr := CreateSolidBrush(ColorToRGB(ASkin.HighlightBg))
    else if (pnm^.uItemState and (CDIS_HOT_CODE or CDIS_CHECKED_CODE)) <> 0 then
      hbr := CreateSolidBrush(ColorToRGB(ASkin.PointedBg))
    else
      hbr := CreateSolidBrush(ColorToRGB(ASkin.PanelBg));

    FillRect(pnm^.hdc, pnm^.rc, hbr);
    DeleteObject(hbr);

    if (pnm^.uItemState and (CDIS_HOT_CODE or CDIS_SELECTED_CODE or CDIS_CHECKED_CODE)) <> 0 then
    begin
      hbr := CreateSolidBrush(ColorToRGB(ASkin.PanelBorder));
      FrameRect(pnm^.hdc, pnm^.rc, hbr);
      DeleteObject(hbr);
    end;

    Result := TBCDRF_NOEDGES_CODE;
    AHandled := True;
    Exit;
  end;
end;
{$ELSE}
procedure HookTabControl(AControl: TWinControl); begin end;
procedure UnhookTabControl(AControl: TWinControl); begin end;
procedure HookButtonControl(AControl: TWinControl); begin end;
procedure UnhookButtonControl(AControl: TWinControl); begin end;
procedure HookCheckBoxControl(AControl: TWinControl); begin end;
procedure UnhookCheckBoxControl(AControl: TWinControl); begin end;
procedure HookScrollBoxControl(AControl: TWinControl); begin end;
procedure UnhookScrollBoxControl(AControl: TWinControl); begin end;
procedure HookToolBarControl(AControl: TWinControl); begin end;
procedure UnhookToolBarControl(AControl: TWinControl); begin end;
procedure HookTreeViewControl(AControl: TWinControl); begin end;
procedure UnhookTreeViewControl(AControl: TWinControl); begin end;
function HandleToolBarCustomDraw(lParam: LPARAM; const ASkin: TSkinColors; var AHandled: Boolean): LRESULT; begin Result := 0; AHandled := False; end;
function GetCachedPanelBrush(AColor: TColor): THandle; begin Result := 0; end;
function GetCachedWindowBrush(AColor: TColor): THandle; begin Result := 0; end;
{$ENDIF}

procedure ApplySkinToControlTree(AParent: TWinControl; const ASkin: TSkinColors);
var
  i: integer;
  ctrl: TControl;
begin
  if not Assigned(AParent) then exit;
  for i := 0 to AParent.ControlCount - 1 do
  begin
    ctrl := AParent.Controls[i];
    if ctrl is TPanel then
    begin
      TPanel(ctrl).Color := ASkin.PanelBg;
      TPanel(ctrl).Font.Color := ASkin.PanelText;
    end
    else if ctrl is TCustomMemo then
    begin
      TCustomMemo(ctrl).Color := ASkin.WindowBg;
      TCustomMemo(ctrl).Font.Color := ASkin.WindowText;
      if ctrl is TWinControl then
        ApplyDarkModeToControl(TWinControl(ctrl), ASkin.IsDark);
    end
    else if ctrl is TCustomEdit then
    begin
      TCustomEdit(ctrl).Color := ASkin.WindowBg;
      TCustomEdit(ctrl).Font.Color := ASkin.WindowText;
      if ctrl is TLabeledEdit then
        TLabeledEdit(ctrl).EditLabel.Font.Color := ASkin.PanelText;
      if ctrl is TWinControl then
        ApplyDarkModeToControl(TWinControl(ctrl), ASkin.IsDark);
    end
    else if ctrl is TCustomListBox then
    begin
      TCustomListBox(ctrl).Color := ASkin.WindowBg;
      TCustomListBox(ctrl).Font.Color := ASkin.WindowText;
      if ctrl is TWinControl then
        ApplyDarkModeToControl(TWinControl(ctrl), ASkin.IsDark);
    end
    else if ctrl is TCustomLabel then
    begin
      TCustomLabel(ctrl).Font.Color := ASkin.PanelText;
    end
    else if ctrl is TCustomCheckBox then
    begin
      TCustomCheckBox(ctrl).Color := ASkin.PanelBg;
      TCustomCheckBox(ctrl).Font.Color := ASkin.PanelText;
      if ctrl is TWinControl then
      begin
        ApplyDarkModeToControl(TWinControl(ctrl), ASkin.IsDark);
        {$IFDEF WINDOWS}
        HookCheckBoxControl(TWinControl(ctrl));
        {$ENDIF}
      end;
    end
    else if ctrl is TRadioButton then
    begin
      TRadioButton(ctrl).Font.Color := ASkin.PanelText;
      if ctrl is TWinControl then
        ApplyDarkModeToControl(TWinControl(ctrl), ASkin.IsDark);
    end
    else if ctrl is TCustomGroupBox then
    begin
      TCustomGroupBox(ctrl).Color := ASkin.PanelBg;
      TCustomGroupBox(ctrl).Font.Color := ASkin.PanelText;
      if ctrl is TWinControl then
      begin
        ApplyDarkModeToControl(TWinControl(ctrl), ASkin.IsDark);
        {$IFDEF WINDOWS}
        HookGroupBoxControl(TWinControl(ctrl));
        {$ENDIF}
      end;
    end
    else if ctrl is TTreeView then
    begin
      TTreeView(ctrl).Color := ASkin.TreeBg;
      TTreeView(ctrl).Font.Color := ASkin.TreeText;
      TTreeView(ctrl).ExpandSignColor := ASkin.TreeText;
      TTreeView(ctrl).TreeLineColor := ASkin.PanelBorder;
      TTreeView(ctrl).ExpandSignType := tvestArrowFill;
      if ctrl is TWinControl then
        ApplyDarkModeToControl(TWinControl(ctrl), ASkin.IsDark);
      TTreeView(ctrl).Invalidate;
    end
    else if (ctrl is TCustomPage) or (ctrl.ClassName = 'TTabSheet') then
    begin
      TWinControl(ctrl).Color := ASkin.PanelBg;
      TWinControl(ctrl).Font.Color := ASkin.PanelText;
      if ctrl is TWinControl then
        ApplyDarkModeToControl(TWinControl(ctrl), ASkin.IsDark);
    end
    else if ctrl is TScrollBox then
    begin
      TScrollBox(ctrl).Color := ASkin.WindowBg;
      if ctrl is TWinControl then
        ApplyDarkModeToControl(TWinControl(ctrl), ASkin.IsDark);
    end
    else if (ctrl is TCustomButton) or (ctrl is TBitBtn) or (ctrl is TSpeedButton) then
    begin
      if ctrl is TWinControl then
      begin
        ApplyDarkModeToControl(TWinControl(ctrl), ASkin.IsDark);
        {$IFDEF WINDOWS}
        HookButtonControl(TWinControl(ctrl));
        {$ENDIF}
      end;
    end
    else if ctrl is TCustomTabControl then
    begin
      TCustomTabControl(ctrl).Color := ASkin.PanelBg;
      TCustomTabControl(ctrl).Font.Color := ASkin.PanelText;
      ApplyDarkModeToControl(TWinControl(ctrl), ASkin.IsDark);
      {$IFDEF WINDOWS}
      HookTabControl(TWinControl(ctrl));
      {$ENDIF}
    end;

    if ctrl is TWinControl then
      ApplySkinToControlTree(TWinControl(ctrl), ASkin);
  end;
end;

procedure ApplySkinToForm(AForm: TCustomForm; const ASkin: TSkinColors);
var
  i: integer;
  ctrl: TControl;
begin
  if not assigned(AForm) then exit;
  AForm.Color := ASkin.PanelBg;
  AForm.Font.Color := ASkin.PanelText;

  {$IFDEF WINDOWS}
  if AForm.HandleAllocated then
    HookFormControl(AForm);
  {$ENDIF}

  // 1. Recursive control tree traversal
  if AForm is TWinControl then
    ApplySkinToControlTree(TWinControl(AForm), ASkin);

  // 2. Also cover non-child components
  for i := 0 to AForm.ComponentCount - 1 do
  begin
    if AForm.Components[i] is TControl then
    begin
      ctrl := TControl(AForm.Components[i]);
      if (ctrl is TCustomButton) or (ctrl is TBitBtn) or (ctrl is TSpeedButton) then
      begin
        if ctrl is TWinControl then
        begin
          ApplyDarkModeToControl(TWinControl(ctrl), ASkin.IsDark);
          {$IFDEF WINDOWS}
          HookButtonControl(TWinControl(ctrl));
          {$ENDIF}
        end;
      end
      else if ctrl is TCustomTabControl then
      begin
        TCustomTabControl(ctrl).Color := ASkin.PanelBg;
        TCustomTabControl(ctrl).Font.Color := ASkin.PanelText;
        ApplyDarkModeToControl(TWinControl(ctrl), ASkin.IsDark);
        {$IFDEF WINDOWS}
        HookTabControl(TWinControl(ctrl));
        {$ENDIF}
      end;
    end;
  end;

  ApplyDarkModeToWindow(AForm, ASkin.IsDark);
end;

function ThemedMessageDlg(const aCaption, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint = 0): Integer;
var
  dlg: TForm;
  i: Integer;
  ctrl: TControl;
begin
  dlg := Dialogs.CreateMessageDialog(Msg, DlgType, Buttons);
  try
    dlg.Caption := aCaption;
    dlg.HelpContext := HelpCtx;
    dlg.Position := poScreenCenter;

    if CurrentSkin.IsDark then
    begin
      dlg.Color := CurrentSkin.PanelBg;
      for i := 0 to dlg.ControlCount - 1 do
      begin
        ctrl := dlg.Controls[i];
        if ctrl is TLabel then
        begin
          TLabel(ctrl).Font.Color := CurrentSkin.PanelText;
        end
        else if ctrl is TButton then
        begin
          TButton(ctrl).Font.Color := CurrentSkin.PanelText;
        end;
      end;
    end;

    dlg.HandleNeeded;
    for i := 0 to dlg.ControlCount - 1 do
    begin
      if dlg.Controls[i] is TWinControl then
        TWinControl(dlg.Controls[i]).HandleNeeded;
    end;

    ApplySkinToForm(dlg, CurrentSkin);

    {$IFDEF WINDOWS}
    ApplyDarkModeToWindow(dlg, CurrentSkin.IsDark);
    for i := 0 to dlg.ControlCount - 1 do
    begin
      if dlg.Controls[i] is TButton then
      begin
        HookButtonControl(TWinControl(dlg.Controls[i]));
        dlg.Controls[i].Invalidate;
      end;
    end;
    {$ENDIF}

    Result := dlg.ShowModal;
  finally
    dlg.Free;
  end;
end;

function ThemedMessageDlg(Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint = 0): Integer;
begin
  Result := ThemedMessageDlg('iBiblia', Msg, DlgType, Buttons, HelpCtx);
end;

procedure ThemedShowMessage(const Msg: string);
begin
  ThemedMessageDlg('iBiblia', Msg, mtInformation, [mbOK], 0);
end;

initialization
  SetCurrentSkin(skClassic);
  {$IFDEF WINDOWS}
  InitUxTheme;
  if Assigned(fnSetPreferredAppMode) then
    fnSetPreferredAppMode(pamAllowDark);
  {$ENDIF}

finalization
  {$IFDEF WINDOWS}
  if hbrCachedPanelBg <> 0 then DeleteObject(hbrCachedPanelBg);
  if hbrCachedWindowBg <> 0 then DeleteObject(hbrCachedWindowBg);
  if hUxTheme <> 0 then FreeLibrary(hUxTheme);
  if hDwmApi <> 0 then FreeLibrary(hDwmApi);
  if hComCtl <> 0 then FreeLibrary(hComCtl);
  {$ENDIF}

end.
