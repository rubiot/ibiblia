unit FormFindInProject;

(* Find & Replace in Project: a modeless Notepad++-style search and bulk-replace
  window for Dark-Skinned iBiblia. Supports Normal, Extended (\n, \r, \t, \x..),
  and PCRE Regular Expression modes (with . matches newline, capture groups
  $1/\1/${name}, and \u/\l/\U/\L/\E case transforms), Match whole word,
  Match case, Wrap around, Count, Find All, Replace, and Replace All while
  preserving existing word associations via TProject.ApplyVerseTexts. *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Math, Forms, Controls, Graphics, StdCtrls,
  ExtCtrls, ComCtrls, Dialogs, LCLType, LazUTF8, PCRE,
  ui_style, Project, unitpleasewait;

type
  TFindNavigateEvent = procedure(const ReferenceID: string) of object;

  TFindSearchMode = (fsmNormal, fsmExtended, fsmRegex);

  TFindMatchEntry = record
    ReferenceID: string;
    FormattedRef: string;
    OriginalText: string;
    ReplacedText: string;
    MatchCount: Integer;
  end;

  { TFrmFindInProject }

  TFrmFindInProject = class(TForm)
  private
    FProject: TProject;
    FMatches: array of TFindMatchEntry;
    FCurrentIndex: Integer;
    FOnNavigate: TFindNavigateEvent;

    FTabControl: TTabControl;
    FControlsPanel: TPanel;
    FFindLabel: TLabel;
    FFindCombo: TComboBox;
    FReplaceLabel: TLabel;
    FReplaceCombo: TComboBox;
    FScopeLabel: TLabel;
    FScopeCombo: TComboBox;

    FOptionsGroup: TGroupBox;
    FWholeWordCheck: TCheckBox;
    FMatchCaseCheck: TCheckBox;
    FWrapAroundCheck: TCheckBox;
    FKeepAssocCheck: TCheckBox;
    FRawTagsCheck: TCheckBox;

    FModeGroup: TGroupBox;
    FModeNormalRadio: TRadioButton;
    FModeExtendedRadio: TRadioButton;
    FModeRegexRadio: TRadioButton;
    FDotMatchesNewlineCheck: TCheckBox;

    FFindNextButton: TButton;
    FFindPrevButton: TButton;
    FCountButton: TButton;
    FFindAllButton: TButton;
    FReplaceButton: TButton;
    FReplaceAllButton: TButton;
    FCloseButton: TButton;

    FStatusPanel: TPanel;
    FCountLabel: TLabel;
    FResultsList: TListBox;

    procedure BuildControls;
    procedure RefreshCaptions;
    procedure UpdateModeUI;
    procedure UpdateTabUI;
    procedure AddToHistory(Combo: TComboBox; const Value: string);
    function GetSearchMode: TFindSearchMode;
    function GetSelectedTextType(out TextType: TTipoTextoBiblico): Boolean;
    function GetScopeDisplayName: string;
    function FormatRefID(const RefID: string): string;
    function BuildSearchRegex(const Pattern: string; out ErrorMsg: string): IRegex;
    function MatchAndReplaceText(const Input, RawReplacement: string;
      const Rx: IRegex; DoReplace: Boolean; out OutputText: string): Integer;
    function ScanProject(ComputeReplacement: Boolean;
      out TotalOccurrences: Integer; out ErrorMsg: string): Boolean;
    procedure PopulateResultsList(ShowReplacePreview: Boolean);
    procedure ClearResults;
    procedure GoToResult(Index: Integer);
    procedure DoFindAll(NavigateFirst: Boolean);
    procedure DoCount;
    procedure DoFindStep(ForwardDir: Boolean);
    procedure DoReplaceCurrent;
    procedure DoReplaceAll;

    procedure TabControlChange(Sender: TObject);
    procedure ModeRadioClick(Sender: TObject);
    procedure ScopeComboChange(Sender: TObject);
    procedure RawTagsCheckClick(Sender: TObject);
    procedure FindNextButtonClick(Sender: TObject);
    procedure FindPrevButtonClick(Sender: TObject);
    procedure CountButtonClick(Sender: TObject);
    procedure FindAllButtonClick(Sender: TObject);
    procedure ReplaceButtonClick(Sender: TObject);
    procedure ReplaceAllButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure ResultsListSelectionChange(Sender: TObject; User: Boolean);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetProject(AProject: TProject);
    procedure SelectModeTab(ReplaceTab: Boolean);
    property OnNavigate: TFindNavigateEvent read FOnNavigate write FOnNavigate;
  end;

{ Shows the shared Find / Replace in Project window for AProject. }
procedure ShowFindInProject(AOwner: TComponent; AProject: TProject;
  ANavigate: TFindNavigateEvent; AStartOnReplace: Boolean = False);
{ Call when the open project changes or closes (nil). }
procedure FindInProjectProjectChanged(AProject: TProject);

implementation

resourcestring
  SFindTitle = 'Find & Replace in Project';
  STabFind = 'Find';
  STabReplace = 'Replace';
  SFindWhat = 'Find &what:';
  SReplaceWith = 'Re&place with:';
  SFindLookIn = '&Look in:';
  SOptionsTitle = 'Options';
  SMatchWholeWord = 'Match &whole word only';
  SMatchCase = 'Match &case';
  SWrapAround = 'Wrap aro&und';
  SKeepAssociations = '&Keep word associations intact';
  SShowRawTags = 'Show raw &tags in preview';
  SSearchModeTitle = 'Search Mode';
  SModeNormal = '&Normal';
  SModeExtended = 'E&xtended (\n, \r, \t, \0, \x...)';
  SModeRegex = 'Regular &expression';
  SDotMatchesNewline = '. matches new&line';
  SBtnFindNext = '&Find Next';
  SBtnFindPrev = 'Find &Prev';
  SBtnCount = 'Coun&t';
  SBtnFindAll = 'Find &All';
  SBtnReplace = '&Replace';
  SBtnReplaceAll = 'Replace A&ll';
  SBtnClose = '&Close';
  SFindPrompt = 'Enter text, tags, or regex and press Enter (Ctrl+F = Find, Ctrl+H = Replace).';
  SFindSearching = 'Searching project...';
  SFindCountFmt = '%d occurrence(s) in %d verse(s)';
  SFindNoMatches = '0 occurrences found.';
  SReplaceNotEditable = 'Replace works on Destination text, Source text, Reference text 1, or Reference text 2. Please select one of those fields in "Look in".';
  SReplaceAllConfirmFmt = 'Replace %d occurrence(s) across %d verse(s) in %s?' + LineEnding + LineEnding + '%s';
  SReplaceAllAssocKeep = 'Existing word associations will be preserved wherever words remain intact, and changes will be saved to the project.';
  SReplaceAllAssocClear = 'Warning: "Keep word associations intact" is unchecked. Associations in modified verses may be cleared.';
  SReplaceAllTitle = 'Confirm Replace All';
  SReplaceProgressTitle = 'Find & Replace';
  SReplaceProgressMsg = 'Applying replacements and preserving associations...';
  SReplaceDoneFmt = 'Replaced %d occurrence(s) across %d verse(s) and saved the project.';
  SReplaceErrorFmt = 'Replace completed with error: %s';
  SRegexErrorFmt = 'Invalid regular expression: %s';
  SFindScopeDestination = 'Destination text';
  SFindScopeSource = 'Source text';
  SFindScopeReference1 = 'Reference text 1';
  SFindScopeReference2 = 'Reference text 2';
  SFindScopeStrongs = 'Strong''s / morphology tags';
  SFindScopeComments = 'Comments';
  SFindScopeAll = 'All fields';

var
  FindInstance: TFrmFindInProject = nil;

function HexValue(C: Char): Integer;
begin
  case C of
    '0'..'9': Result := Ord(C) - Ord('0');
    'a'..'f': Result := 10 + Ord(C) - Ord('a');
    'A'..'F': Result := 10 + Ord(C) - Ord('A');
  else
    Result := -1;
  end;
end;

function ParseHexDigits(const S: string; StartIdx, MaxDigits: Integer;
  out Value, Consumed: Integer): Boolean;
var
  I, D: Integer;
begin
  Value := 0;
  Consumed := 0;
  I := StartIdx;
  while (I <= Length(S)) and (Consumed < MaxDigits) do
  begin
    D := HexValue(S[I]);
    if D < 0 then
      Break;
    Value := (Value shl 4) or D;
    Inc(Consumed);
    Inc(I);
  end;
  Result := Consumed > 0;
end;

{ Notepad++ Extended mode unescaper: \n, \r, \t, \0, \\, \xHH, \uHHHH }
function UnescapeExtended(const S: string): string;
var
  I, Val, Consumed: Integer;
  C: Char;
begin
  Result := '';
  I := 1;
  while I <= Length(S) do
  begin
    if (S[I] = '\') and (I < Length(S)) then
    begin
      C := S[I + 1];
      case C of
        'n': begin Result := Result + #10; Inc(I, 2); end;
        'r': begin Result := Result + #13; Inc(I, 2); end;
        't': begin Result := Result + #9; Inc(I, 2); end;
        '0': begin Result := Result + #0; Inc(I, 2); end;
        '\': begin Result := Result + '\'; Inc(I, 2); end;
        'x':
          begin
            if ParseHexDigits(S, I + 2, 2, Val, Consumed) then
            begin
              Result := Result + Chr(Val and $FF);
              Inc(I, 2 + Consumed);
            end
            else
            begin
              Result := Result + '\x';
              Inc(I, 2);
            end;
          end;
        'u':
          begin
            if ParseHexDigits(S, I + 2, 4, Val, Consumed) and (Consumed = 4) then
            begin
              Result := Result + UnicodeToUTF8(Cardinal(Val));
              Inc(I, 6);
            end
            else
            begin
              Result := Result + '\u';
              Inc(I, 2);
            end;
          end;
      else
        begin
          Result := Result + '\' + C;
          Inc(I, 2);
        end;
      end;
    end
    else
    begin
      Result := Result + S[I];
      Inc(I);
    end;
  end;
end;

{ Escapes literal text so PCRE matches it verbatim. }
function EscapeRegexLiteral(const S: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    if C in ['\', '.', '^', '$', '*', '+', '?', '(', ')', '[', ']', '{', '}', '|'] then
      Result := Result + '\' + C
    else
      Result := Result + C;
  end;
end;

function IsWordByteAt(const S: string; ByteIndex1Based: Integer): Boolean;
var
  C: Byte;
begin
  if (ByteIndex1Based < 1) or (ByteIndex1Based > Length(S)) then
    Exit(False);
  C := Ord(S[ByteIndex1Based]);
  if C >= $80 then
    Exit(True); { UTF-8 non-ASCII letter/diacritic }
  Result := (Chr(C) in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
end;

function PassesWholeWordCheck(const Input: string; MatchStart0Based, MatchByteLen: Integer): Boolean;
var
  Start1, End1: Integer;
begin
  if MatchByteLen <= 0 then
    Exit(True);
  Start1 := MatchStart0Based + 1;
  End1 := MatchStart0Based + MatchByteLen;
  if IsWordByteAt(Input, Start1) and IsWordByteAt(Input, Start1 - 1) then
    Exit(False);
  if IsWordByteAt(Input, End1) and IsWordByteAt(Input, End1 + 1) then
    Exit(False);
  Result := True;
end;

{ Applies Notepad++ case modifiers (\u, \l, \U, \L, \E) to an expanded replacement. }
function UTF8CharByteLen(B: Byte): Integer;
begin
  if (B and $80) = 0 then Result := 1
  else if (B and $E0) = $C0 then Result := 2
  else if (B and $F0) = $E0 then Result := 3
  else if (B and $F8) = $F0 then Result := 4
  else Result := 1;
end;

function ApplyCaseTransforms(const Tokens: string): string;
type
  TCaseMode = (cmNone, cmUpperAll, cmLowerAll);
  TNextCaseMode = (nmNone, nmUpperOne, nmLowerOne);
var
  I, CharLen: Integer;
  Mode: TCaseMode;
  NextMode: TNextCaseMode;
  Ch: string;
begin
  Result := '';
  Mode := cmNone;
  NextMode := nmNone;
  I := 1;
  while I <= Length(Tokens) do
  begin
    if (Tokens[I] = #1) and (I < Length(Tokens)) then
    begin
      case Tokens[I + 1] of
        'u': NextMode := nmUpperOne;
        'l': NextMode := nmLowerOne;
        'U': Mode := cmUpperAll;
        'L': Mode := cmLowerAll;
        'E': begin Mode := cmNone; NextMode := nmNone; end;
      end;
      Inc(I, 2);
      Continue;
    end;
    CharLen := UTF8CharByteLen(Ord(Tokens[I]));
    if CharLen < 1 then
      CharLen := 1;
    Ch := Copy(Tokens, I, CharLen);
    if NextMode = nmUpperOne then
    begin
      Ch := UTF8UpperCase(Ch);
      NextMode := nmNone;
    end
    else if NextMode = nmLowerOne then
    begin
      Ch := UTF8LowerCase(Ch);
      NextMode := nmNone;
    end
    else if Mode = cmUpperAll then
      Ch := UTF8UpperCase(Ch)
    else if Mode = cmLowerAll then
      Ch := UTF8LowerCase(Ch);
    Result := Result + Ch;
    Inc(I, CharLen);
  end;
end;

(* Notepad++-compatible regex replacement expander supporting:
  $0, $&, \0, $1..$99, ${1}..${99}, \1..\9, ${name}, $+{name}, $$,
  \n, \r, \t, \\, \xHH, \uHHHH, \u, \l, \U, \L, \E. *)
function ExpandNppRegexReplacement(const Replacement: string; const M: IMatch): string;
var
  I, J, GroupIdx, Val, Consumed: Integer;
  GroupName, GroupVal, RawOut: string;
  HasCaseOps: Boolean;
  C: Char;

  function SafeGroupValue(Idx: Integer): string;
  begin
    if (Idx >= 0) and (Idx < M.Groups.Count) and M.Groups[Idx].Matched then
      Result := M.Groups[Idx].Value
    else
      Result := '';
  end;

  function SafeNamedGroupValue(const Name: string): string;
  var
    Grp: ICaptureGroup;
  begin
    Result := '';
    try
      Grp := M.Groups.ItemsByName[Name];
      if Assigned(Grp) and Grp.Matched then
        Result := Grp.Value;
    except
      Result := '';
    end;
  end;

begin
  RawOut := '';
  HasCaseOps := False;
  I := 1;
  while I <= Length(Replacement) do
  begin
    if (Replacement[I] = '\') and (I < Length(Replacement)) then
    begin
      C := Replacement[I + 1];
      case C of
        'n': begin RawOut := RawOut + #10; Inc(I, 2); end;
        'r': begin RawOut := RawOut + #13; Inc(I, 2); end;
        't': begin RawOut := RawOut + #9; Inc(I, 2); end;
        '\': begin RawOut := RawOut + '\'; Inc(I, 2); end;
        '0'..'9':
          begin
            GroupIdx := Ord(C) - Ord('0');
            RawOut := RawOut + SafeGroupValue(GroupIdx);
            Inc(I, 2);
          end;
        'x':
          begin
            if ParseHexDigits(Replacement, I + 2, 2, Val, Consumed) then
            begin
              RawOut := RawOut + Chr(Val and $FF);
              Inc(I, 2 + Consumed);
            end
            else
            begin
              RawOut := RawOut + '\x';
              Inc(I, 2);
            end;
          end;
        'u':
          begin
            if ParseHexDigits(Replacement, I + 2, 4, Val, Consumed) and (Consumed = 4) then
            begin
              RawOut := RawOut + UnicodeToUTF8(Cardinal(Val));
              Inc(I, 6);
            end
            else
            begin
              RawOut := RawOut + #1 + 'u';
              HasCaseOps := True;
              Inc(I, 2);
            end;
          end;
        'l', 'U', 'L', 'E':
          begin
            RawOut := RawOut + #1 + C;
            HasCaseOps := True;
            Inc(I, 2);
          end;
      else
        begin
          RawOut := RawOut + C;
          Inc(I, 2);
        end;
      end;
    end
    else if (Replacement[I] = '$') and (I < Length(Replacement)) then
    begin
      C := Replacement[I + 1];
      if C = '$' then
      begin
        RawOut := RawOut + '$';
        Inc(I, 2);
      end
      else if C = '&' then
      begin
        RawOut := RawOut + M.Value;
        Inc(I, 2);
      end
      else if C in ['0'..'9'] then
      begin
        J := I + 1;
        GroupIdx := 0;
        while (J <= Length(Replacement)) and (Replacement[J] in ['0'..'9']) and ((J - I) <= 2) do
        begin
          GroupIdx := GroupIdx * 10 + (Ord(Replacement[J]) - Ord('0'));
          Inc(J);
        end;
        RawOut := RawOut + SafeGroupValue(GroupIdx);
        I := J;
      end
      else if (C = '{') or ((C = '+') and (I + 2 <= Length(Replacement)) and (Replacement[I + 2] = '{')) then
      begin
        if C = '+' then
          J := I + 3
        else
          J := I + 2;
        GroupName := '';
        while (J <= Length(Replacement)) and (Replacement[J] <> '}') do
        begin
          GroupName := GroupName + Replacement[J];
          Inc(J);
        end;
        if (J <= Length(Replacement)) and (Replacement[J] = '}') then
        begin
          if TryStrToInt(GroupName, GroupIdx) then
            GroupVal := SafeGroupValue(GroupIdx)
          else
            GroupVal := SafeNamedGroupValue(GroupName);
          RawOut := RawOut + GroupVal;
          I := J + 1;
        end
        else
        begin
          RawOut := RawOut + '$';
          Inc(I);
        end;
      end
      else
      begin
        RawOut := RawOut + '$';
        Inc(I);
      end;
    end
    else
    begin
      RawOut := RawOut + Replacement[I];
      Inc(I);
    end;
  end;
  if HasCaseOps then
    Result := ApplyCaseTransforms(RawOut)
  else
    Result := RawOut;
end;

{ Removes <...> markup (theWord tags, HTML) from a result preview. }
function StripPreviewMarkup(const S: string): string;
var
  Index: Integer;
  InTag: Boolean;
begin
  Result := '';
  InTag := False;
  for Index := 1 to Length(S) do
  begin
    if InTag then
    begin
      if S[Index] = '>' then
      begin
        InTag := False;
        if (Result <> '') and (Result[Length(Result)] <> ' ') then
          Result := Result + ' ';
      end;
    end
    else if S[Index] = '<' then
      InTag := True
    else
      Result := Result + S[Index];
  end;
  while Pos('  ', Result) > 0 do
    Result := StringReplace(Result, '  ', ' ', [rfReplaceAll]);
  Result := Trim(Result);
end;

function FlattenPreview(const S: string; RawTags: Boolean): string;
begin
  Result := StringReplace(S, #13, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #10, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #9, ' ', [rfReplaceAll]);
  if not RawTags then
    Result := StripPreviewMarkup(Result);
  if Length(Result) > 180 then
    Result := UTF8Copy(Result, 1, 140) + '...';
end;

procedure ShowFindInProject(AOwner: TComponent; AProject: TProject;
  ANavigate: TFindNavigateEvent; AStartOnReplace: Boolean);
begin
  if AProject = nil then
    Exit;
  if FindInstance = nil then
    FindInstance := TFrmFindInProject.Create(AOwner);
  FindInstance.OnNavigate := ANavigate;
  FindInstance.SetProject(AProject);
  FindInstance.SelectModeTab(AStartOnReplace);
  FindInstance.Show;
  FindInstance.BringToFront;
  if FindInstance.FFindCombo.CanFocus then
    FindInstance.FFindCombo.SetFocus;
  FindInstance.FFindCombo.SelectAll;
end;

procedure FindInProjectProjectChanged(AProject: TProject);
begin
  if FindInstance = nil then
    Exit;
  FindInstance.SetProject(AProject);
  if AProject = nil then
    FindInstance.Hide;
end;

{ TFrmFindInProject }

constructor TFrmFindInProject.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  ApplyFormStyle(Self);
  FCurrentIndex := -1;
  Position := poMainFormCenter;
  BorderStyle := bsSizeToolWin;
  FormStyle := fsStayOnTop;
  ClientWidth := 760;
  ClientHeight := 540;
  Constraints.MinWidth := 620;
  Constraints.MinHeight := 420;
  BuildControls;
  RefreshCaptions;
  UpdateModeUI;
  UpdateTabUI;
end;

destructor TFrmFindInProject.Destroy;
begin
  if FindInstance = Self then
    FindInstance := nil;
  inherited Destroy;
end;

procedure TFrmFindInProject.BuildControls;
begin
  FTabControl := TTabControl.Create(Self);
  FTabControl.Parent := Self;
  FTabControl.Align := alTop;
  FTabControl.Height := 30;
  FTabControl.OnChange := @TabControlChange;

  FControlsPanel := TPanel.Create(Self);
  FControlsPanel.Parent := Self;
  FControlsPanel.Align := alTop;
  FControlsPanel.Height := 248;
  FControlsPanel.BevelOuter := bvNone;

  { Left column: Find what, Replace with, Look in }
  FFindLabel := TLabel.Create(Self);
  FFindLabel.Parent := FControlsPanel;
  FFindLabel.SetBounds(12, 12, 92, 22);
  FFindLabel.Layout := tlCenter;

  FFindCombo := TComboBox.Create(Self);
  FFindCombo.Parent := FControlsPanel;
  FFindCombo.SetBounds(108, 10, 488, 26);
  FFindCombo.Anchors := [akLeft, akTop, akRight];
  FFindCombo.Style := csDropDown;
  FFindCombo.OnKeyDown := @EditKeyDown;
  FFindLabel.FocusControl := FFindCombo;

  FReplaceLabel := TLabel.Create(Self);
  FReplaceLabel.Parent := FControlsPanel;
  FReplaceLabel.SetBounds(12, 44, 92, 22);
  FReplaceLabel.Layout := tlCenter;

  FReplaceCombo := TComboBox.Create(Self);
  FReplaceCombo.Parent := FControlsPanel;
  FReplaceCombo.SetBounds(108, 42, 488, 26);
  FReplaceCombo.Anchors := [akLeft, akTop, akRight];
  FReplaceCombo.Style := csDropDown;
  FReplaceCombo.OnKeyDown := @EditKeyDown;
  FReplaceLabel.FocusControl := FReplaceCombo;

  FScopeLabel := TLabel.Create(Self);
  FScopeLabel.Parent := FControlsPanel;
  FScopeLabel.SetBounds(12, 76, 92, 22);
  FScopeLabel.Layout := tlCenter;

  FScopeCombo := TComboBox.Create(Self);
  FScopeCombo.Parent := FControlsPanel;
  FScopeCombo.SetBounds(108, 74, 270, 26);
  FScopeCombo.Style := csDropDownList;
  FScopeCombo.OnChange := @ScopeComboChange;
  FScopeLabel.FocusControl := FScopeCombo;

  { Options GroupBox }
  FOptionsGroup := TGroupBox.Create(Self);
  FOptionsGroup.Parent := FControlsPanel;
  FOptionsGroup.SetBounds(12, 108, 286, 132);

  FWholeWordCheck := TCheckBox.Create(Self);
  FWholeWordCheck.Parent := FOptionsGroup;
  FWholeWordCheck.SetBounds(10, 6, 264, 21);

  FMatchCaseCheck := TCheckBox.Create(Self);
  FMatchCaseCheck.Parent := FOptionsGroup;
  FMatchCaseCheck.SetBounds(10, 29, 264, 21);

  FWrapAroundCheck := TCheckBox.Create(Self);
  FWrapAroundCheck.Parent := FOptionsGroup;
  FWrapAroundCheck.SetBounds(10, 52, 264, 21);
  FWrapAroundCheck.Checked := True;

  FKeepAssocCheck := TCheckBox.Create(Self);
  FKeepAssocCheck.Parent := FOptionsGroup;
  FKeepAssocCheck.SetBounds(10, 75, 264, 21);
  FKeepAssocCheck.Checked := True;

  FRawTagsCheck := TCheckBox.Create(Self);
  FRawTagsCheck.Parent := FOptionsGroup;
  FRawTagsCheck.SetBounds(10, 98, 264, 21);
  FRawTagsCheck.OnClick := @RawTagsCheckClick;

  { Search Mode GroupBox }
  FModeGroup := TGroupBox.Create(Self);
  FModeGroup.Parent := FControlsPanel;
  FModeGroup.SetBounds(308, 108, 288, 132);
  FModeGroup.Anchors := [akLeft, akTop, akRight];

  FModeNormalRadio := TRadioButton.Create(Self);
  FModeNormalRadio.Parent := FModeGroup;
  FModeNormalRadio.SetBounds(10, 8, 266, 22);
  FModeNormalRadio.Checked := True;
  FModeNormalRadio.OnClick := @ModeRadioClick;

  FModeExtendedRadio := TRadioButton.Create(Self);
  FModeExtendedRadio.Parent := FModeGroup;
  FModeExtendedRadio.SetBounds(10, 34, 266, 22);
  FModeExtendedRadio.OnClick := @ModeRadioClick;

  FModeRegexRadio := TRadioButton.Create(Self);
  FModeRegexRadio.Parent := FModeGroup;
  FModeRegexRadio.SetBounds(10, 60, 266, 22);
  FModeRegexRadio.OnClick := @ModeRadioClick;

  FDotMatchesNewlineCheck := TCheckBox.Create(Self);
  FDotMatchesNewlineCheck.Parent := FModeGroup;
  FDotMatchesNewlineCheck.SetBounds(28, 86, 248, 22);

  { Right column: Notepad++-style action buttons }
  FFindNextButton := TButton.Create(Self);
  FFindNextButton.Parent := FControlsPanel;
  FFindNextButton.SetBounds(610, 9, 138, 28);
  FFindNextButton.Anchors := [akTop, akRight];
  FFindNextButton.OnClick := @FindNextButtonClick;

  FFindPrevButton := TButton.Create(Self);
  FFindPrevButton.Parent := FControlsPanel;
  FFindPrevButton.SetBounds(610, 41, 138, 28);
  FFindPrevButton.Anchors := [akTop, akRight];
  FFindPrevButton.OnClick := @FindPrevButtonClick;

  FCountButton := TButton.Create(Self);
  FCountButton.Parent := FControlsPanel;
  FCountButton.SetBounds(610, 73, 138, 28);
  FCountButton.Anchors := [akTop, akRight];
  FCountButton.OnClick := @CountButtonClick;

  FFindAllButton := TButton.Create(Self);
  FFindAllButton.Parent := FControlsPanel;
  FFindAllButton.SetBounds(610, 105, 138, 28);
  FFindAllButton.Anchors := [akTop, akRight];
  FFindAllButton.OnClick := @FindAllButtonClick;

  FReplaceButton := TButton.Create(Self);
  FReplaceButton.Parent := FControlsPanel;
  FReplaceButton.SetBounds(610, 141, 138, 28);
  FReplaceButton.Anchors := [akTop, akRight];
  FReplaceButton.OnClick := @ReplaceButtonClick;

  FReplaceAllButton := TButton.Create(Self);
  FReplaceAllButton.Parent := FControlsPanel;
  FReplaceAllButton.SetBounds(610, 173, 138, 28);
  FReplaceAllButton.Anchors := [akTop, akRight];
  FReplaceAllButton.OnClick := @ReplaceAllButtonClick;

  FCloseButton := TButton.Create(Self);
  FCloseButton.Parent := FControlsPanel;
  FCloseButton.SetBounds(610, 211, 138, 28);
  FCloseButton.Anchors := [akTop, akRight];
  FCloseButton.Cancel := True;
  FCloseButton.OnClick := @CloseButtonClick;

  { Status bar above results list }
  FStatusPanel := TPanel.Create(Self);
  FStatusPanel.Parent := Self;
  FStatusPanel.Align := alTop;
  FStatusPanel.Top := FTabControl.Height + FControlsPanel.Height;
  FStatusPanel.Height := 26;
  FStatusPanel.BevelOuter := bvNone;

  FCountLabel := TLabel.Create(Self);
  FCountLabel.Parent := FStatusPanel;
  FCountLabel.SetBounds(12, 2, 736, 22);
  FCountLabel.Anchors := [akLeft, akTop, akRight];
  FCountLabel.Layout := tlCenter;
  FCountLabel.Font.Style := [fsBold];

  { Interactive results list }
  FResultsList := TListBox.Create(Self);
  FResultsList.Parent := Self;
  FResultsList.Align := alClient;
  FResultsList.BorderSpacing.Left := 12;
  FResultsList.BorderSpacing.Right := 12;
  FResultsList.BorderSpacing.Bottom := 12;
  FResultsList.OnSelectionChange := @ResultsListSelectionChange;

  ClearResults;
end;

procedure TFrmFindInProject.RefreshCaptions;
var
  Scope, ActiveTab: Integer;
begin
  Caption := SFindTitle;
  ActiveTab := FTabControl.TabIndex;
  FTabControl.Tabs.BeginUpdate;
  try
    FTabControl.Tabs.Clear;
    FTabControl.Tabs.Add(STabFind);
    FTabControl.Tabs.Add(STabReplace);
  finally
    FTabControl.Tabs.EndUpdate;
  end;
  if (ActiveTab < 0) or (ActiveTab >= FTabControl.Tabs.Count) then
    ActiveTab := 0;
  FTabControl.TabIndex := ActiveTab;

  FFindLabel.Caption := SFindWhat;
  FReplaceLabel.Caption := SReplaceWith;
  FScopeLabel.Caption := SFindLookIn;

  FOptionsGroup.Caption := SOptionsTitle;
  FWholeWordCheck.Caption := SMatchWholeWord;
  FMatchCaseCheck.Caption := SMatchCase;
  FWrapAroundCheck.Caption := SWrapAround;
  FKeepAssocCheck.Caption := SKeepAssociations;
  FRawTagsCheck.Caption := SShowRawTags;

  FModeGroup.Caption := SSearchModeTitle;
  FModeNormalRadio.Caption := SModeNormal;
  FModeExtendedRadio.Caption := SModeExtended;
  FModeRegexRadio.Caption := SModeRegex;
  FDotMatchesNewlineCheck.Caption := SDotMatchesNewline;

  FFindNextButton.Caption := SBtnFindNext;
  FFindPrevButton.Caption := SBtnFindPrev;
  FCountButton.Caption := SBtnCount;
  FFindAllButton.Caption := SBtnFindAll;
  FReplaceButton.Caption := SBtnReplace;
  FReplaceAllButton.Caption := SBtnReplaceAll;
  FCloseButton.Caption := SBtnClose;

  Scope := FScopeCombo.ItemIndex;
  FScopeCombo.Items.Clear;
  FScopeCombo.Items.Add(SFindScopeDestination);  // 0: tbDestino
  FScopeCombo.Items.Add(SFindScopeSource);       // 1: tbOrigem
  FScopeCombo.Items.Add(SFindScopeReference1);   // 2: tbConsulta1
  FScopeCombo.Items.Add(SFindScopeReference2);   // 3: tbConsulta2
  FScopeCombo.Items.Add(SFindScopeStrongs);      // 4: ssStrongs (Find only)
  FScopeCombo.Items.Add(SFindScopeComments);     // 5: ssComments (Find only)
  FScopeCombo.Items.Add(SFindScopeAll);          // 6: ssAll (Find only)
  if (Scope < 0) or (Scope >= FScopeCombo.Items.Count) then
    Scope := 0;
  FScopeCombo.ItemIndex := Scope;
end;

procedure TFrmFindInProject.SelectModeTab(ReplaceTab: Boolean);
begin
  if ReplaceTab then
    FTabControl.TabIndex := 1
  else
    FTabControl.TabIndex := 0;
  UpdateTabUI;
end;

procedure TFrmFindInProject.UpdateModeUI;
begin
  FDotMatchesNewlineCheck.Enabled := FModeRegexRadio.Checked;
  FWholeWordCheck.Enabled := not FModeRegexRadio.Checked;
end;

procedure TFrmFindInProject.UpdateTabUI;
var
  IsReplace: Boolean;
  DummyType: TTipoTextoBiblico;
  CanReplaceScope: Boolean;
begin
  IsReplace := (FTabControl.TabIndex = 1);
  FReplaceLabel.Visible := IsReplace;
  FReplaceCombo.Visible := IsReplace;
  FReplaceButton.Visible := IsReplace;
  FReplaceAllButton.Visible := IsReplace;
  FKeepAssocCheck.Visible := IsReplace;
  CanReplaceScope := GetSelectedTextType(DummyType);
  FReplaceButton.Enabled := IsReplace and CanReplaceScope and (Length(FMatches) > 0);
  FReplaceAllButton.Enabled := IsReplace and CanReplaceScope;
end;

procedure TFrmFindInProject.AddToHistory(Combo: TComboBox; const Value: string);
var
  Idx: Integer;
begin
  if Value = '' then
    Exit;
  Idx := Combo.Items.IndexOf(Value);
  if Idx >= 0 then
    Combo.Items.Delete(Idx);
  Combo.Items.Insert(0, Value);
  while Combo.Items.Count > 20 do
    Combo.Items.Delete(Combo.Items.Count - 1);
  Combo.Text := Value;
end;

function TFrmFindInProject.GetSearchMode: TFindSearchMode;
begin
  if FModeRegexRadio.Checked then
    Result := fsmRegex
  else if FModeExtendedRadio.Checked then
    Result := fsmExtended
  else
    Result := fsmNormal;
end;

function TFrmFindInProject.GetSelectedTextType(out TextType: TTipoTextoBiblico): Boolean;
begin
  Result := True;
  case FScopeCombo.ItemIndex of
    0: TextType := tbDestino;
    1: TextType := tbOrigem;
    2: TextType := tbConsulta1;
    3: TextType := tbConsulta2;
  else
    TextType := tbDestino;
    Result := False;
  end;
end;

function TFrmFindInProject.GetScopeDisplayName: string;
begin
  if (FScopeCombo.ItemIndex >= 0) and (FScopeCombo.ItemIndex < FScopeCombo.Items.Count) then
    Result := FScopeCombo.Items[FScopeCombo.ItemIndex]
  else
    Result := SFindScopeDestination;
end;

function TFrmFindInProject.FormatRefID(const RefID: string): string;
var
  Parts: TStringArray;
  B, C, V: Integer;
  BName: string;
begin
  Parts := RefID.Split([',']);
  if Length(Parts) >= 3 then
  begin
    B := StrToIntDef(Parts[0], 0);
    C := StrToIntDef(Parts[1], 0);
    V := StrToIntDef(Parts[2], 0);
    BName := '';
    if Assigned(FProject) and (B > 0) then
      BName := FProject.TreeBookName(B);
    if BName = '' then
      BName := 'Book ' + IntToStr(B);
    Result := Format('%s %d:%d', [BName, C, V]);
  end
  else
    Result := RefID;
end;

function TFrmFindInProject.BuildSearchRegex(const Pattern: string;
  out ErrorMsg: string): IRegex;
var
  EffectivePattern: string;
  CompileOpts: TRegCompileOptions;
begin
  Result := nil;
  ErrorMsg := '';
  if Pattern = '' then
    Exit;

  CompileOpts := [rcoUTF8];
  if not FMatchCaseCheck.Checked then
    Include(CompileOpts, rcoIgnoreCase);

  case GetSearchMode of
    fsmNormal:
      EffectivePattern := EscapeRegexLiteral(Pattern);
    fsmExtended:
      EffectivePattern := EscapeRegexLiteral(UnescapeExtended(Pattern));
    fsmRegex:
      begin
        EffectivePattern := Pattern;
        if FDotMatchesNewlineCheck.Checked then
          Include(CompileOpts, rcoSingleLine);
      end;
  end;

  try
    Result := RegexCreate(EffectivePattern, CompileOpts);
  except
    on E: Exception do
    begin
      ErrorMsg := E.Message;
      Result := nil;
    end;
  end;
end;

function TFrmFindInProject.MatchAndReplaceText(const Input, RawReplacement: string;
  const Rx: IRegex; DoReplace: Boolean; out OutputText: string): Integer;
var
  Offset, FirstPos: Integer;
  M: IMatch;
  Mode: TFindSearchMode;
  WholeWord: Boolean;
  LiteralRepl, SubVal: string;
begin
  Result := 0;
  OutputText := Input;
  if (Rx = nil) or (Input = '') then
    Exit;

  Mode := GetSearchMode;
  WholeWord := (Mode <> fsmRegex) and FWholeWordCheck.Checked;

  if DoReplace then
  begin
    if Mode = fsmExtended then
      LiteralRepl := UnescapeExtended(RawReplacement)
    else
      LiteralRepl := RawReplacement;
    OutputText := '';
  end;

  Offset := 0;
  FirstPos := 0;
  M := Rx.Match(Input, Offset, []);
  while M.Matched do
  begin
    if (not WholeWord) or PassesWholeWordCheck(Input, M.Index, M.Length) then
    begin
      Inc(Result);
      if DoReplace then
      begin
        if M.Index > FirstPos then
          OutputText := OutputText + Copy(Input, FirstPos + 1, M.Index - FirstPos);
        if Mode = fsmRegex then
          SubVal := ExpandNppRegexReplacement(RawReplacement, M)
        else
          SubVal := LiteralRepl;
        OutputText := OutputText + SubVal;
        FirstPos := M.Index + M.Length;
      end;
    end;
    Offset := Max(M.Index + M.Length, M.Index + 1);
    if Offset > Length(Input) then
      Break;
    M := Rx.Match(Input, Offset, []);
  end;

  if DoReplace then
  begin
    if FirstPos < Length(Input) then
      OutputText := OutputText + Copy(Input, FirstPos + 1, MaxInt);
  end;
end;

function TFrmFindInProject.ScanProject(ComputeReplacement: Boolean;
  out TotalOccurrences: Integer; out ErrorMsg: string): Boolean;
var
  Rx: IRegex;
  TextType: TTipoTextoBiblico;
  Refs, Texts: TStringList;
  FallbackResults: TVerseSearchResults;
  I, HitCount, MatchIdx: Integer;
  OutText, RawRepl: string;
  ScopeEnum: TSearchScope;
begin
  Result := False;
  TotalOccurrences := 0;
  ErrorMsg := '';
  SetLength(FMatches, 0);

  if (FProject = nil) or (FFindCombo.Text = '') then
    Exit;

  Rx := BuildSearchRegex(FFindCombo.Text, ErrorMsg);
  if Rx = nil then
  begin
    MessageDlg(Format(SRegexErrorFmt, [ErrorMsg]), mtError, [mbOK], 0);
    Exit;
  end;

  RawRepl := FReplaceCombo.Text;
  MatchIdx := 0;

  if GetSelectedTextType(TextType) then
  begin
    Refs := TStringList.Create;
    Texts := TStringList.Create;
    try
      FProject.ReadTextColumn(TextType, Refs, Texts);
      for I := 0 to Refs.Count - 1 do
      begin
        HitCount := MatchAndReplaceText(Texts[I], RawRepl, Rx,
          ComputeReplacement, OutText);
        if HitCount > 0 then
        begin
          Inc(TotalOccurrences, HitCount);
          SetLength(FMatches, MatchIdx + 1);
          FMatches[MatchIdx].ReferenceID := Refs[I];
          FMatches[MatchIdx].FormattedRef := FormatRefID(Refs[I]);
          FMatches[MatchIdx].OriginalText := Texts[I];
          if ComputeReplacement then
            FMatches[MatchIdx].ReplacedText := OutText
          else
            FMatches[MatchIdx].ReplacedText := Texts[I];
          FMatches[MatchIdx].MatchCount := HitCount;
          Inc(MatchIdx);
        end;
      end;
    finally
      Texts.Free;
      Refs.Free;
    end;
    Result := True;
  end
  else
  begin
    { Non-editable search-only scopes (Strong's, Comments, All fields):
      use TProject.SearchVerses for Normal mode or scan SQLite columns. }
    case FScopeCombo.ItemIndex of
      4: ScopeEnum := ssStrongs;
      5: ScopeEnum := ssComments;
    else
      ScopeEnum := ssAll;
    end;
    FallbackResults := FProject.SearchVerses(Trim(FFindCombo.Text), ScopeEnum);
    for I := 0 to High(FallbackResults) do
    begin
      HitCount := MatchAndReplaceText(FallbackResults[I].PreviewText, '', Rx, False, OutText);
      if HitCount = 0 then
        HitCount := 1;
      Inc(TotalOccurrences, HitCount);
      SetLength(FMatches, MatchIdx + 1);
      FMatches[MatchIdx].ReferenceID := FallbackResults[I].ReferenceID;
      FMatches[MatchIdx].FormattedRef := FallbackResults[I].FormattedRef;
      FMatches[MatchIdx].OriginalText := FallbackResults[I].PreviewText;
      FMatches[MatchIdx].ReplacedText := FallbackResults[I].PreviewText;
      FMatches[MatchIdx].MatchCount := HitCount;
      Inc(MatchIdx);
    end;
    Result := True;
  end;
end;

procedure TFrmFindInProject.PopulateResultsList(ShowReplacePreview: Boolean);
var
  I: Integer;
  RawTags: Boolean;
  LineText, OrigPrev, ReplPrev: string;
begin
  RawTags := FRawTagsCheck.Checked or (FScopeCombo.ItemIndex = 4);
  FResultsList.Items.BeginUpdate;
  try
    FResultsList.Items.Clear;
    for I := 0 to High(FMatches) do
    begin
      OrigPrev := FlattenPreview(FMatches[I].OriginalText, RawTags);
      if ShowReplacePreview and (FMatches[I].ReplacedText <> FMatches[I].OriginalText) then
      begin
        ReplPrev := FlattenPreview(FMatches[I].ReplacedText, RawTags);
        LineText := Format('%s (%d)  |  %s  ==>  %s', [
          FMatches[I].FormattedRef, FMatches[I].MatchCount, OrigPrev, ReplPrev
        ]);
      end
      else
        LineText := Format('%s (%d)  |  %s', [
          FMatches[I].FormattedRef, FMatches[I].MatchCount, OrigPrev
        ]);
      FResultsList.Items.Add(LineText);
    end;
  finally
    FResultsList.Items.EndUpdate;
  end;
end;

procedure TFrmFindInProject.SetProject(AProject: TProject);
begin
  if FProject <> AProject then
    ClearResults;
  FProject := AProject;
  RefreshCaptions;
  UpdateTabUI;
end;

procedure TFrmFindInProject.ClearResults;
begin
  SetLength(FMatches, 0);
  FCurrentIndex := -1;
  FResultsList.Items.Clear;
  FCountLabel.Caption := SFindPrompt;
  UpdateTabUI;
end;

procedure TFrmFindInProject.GoToResult(Index: Integer);
begin
  if (Index < 0) or (Index > High(FMatches)) then
    Exit;
  FCurrentIndex := Index;
  if FResultsList.ItemIndex <> Index then
    FResultsList.ItemIndex := Index;
  if Assigned(FOnNavigate) then
    FOnNavigate(FMatches[Index].ReferenceID);
end;

procedure TFrmFindInProject.DoFindAll(NavigateFirst: Boolean);
var
  TotalOcc: Integer;
  ErrMsg: string;
  ShowRepl: Boolean;
begin
  if (FProject = nil) or (FFindCombo.Text = '') then
    Exit;

  AddToHistory(FFindCombo, FFindCombo.Text);
  if FTabControl.TabIndex = 1 then
    AddToHistory(FReplaceCombo, FReplaceCombo.Text);

  ShowRepl := (FTabControl.TabIndex = 1);
  FCountLabel.Caption := SFindSearching;
  Application.ProcessMessages;
  Screen.BeginWaitCursor;
  try
    if not ScanProject(ShowRepl, TotalOcc, ErrMsg) then
      Exit;
  finally
    Screen.EndWaitCursor;
  end;

  PopulateResultsList(ShowRepl);
  if Length(FMatches) = 0 then
    FCountLabel.Caption := SFindNoMatches
  else
    FCountLabel.Caption := Format(SFindCountFmt, [TotalOcc, Length(FMatches)]);

  FCurrentIndex := -1;
  UpdateTabUI;
  if NavigateFirst and (Length(FMatches) > 0) then
    GoToResult(0);
end;

procedure TFrmFindInProject.DoCount;
var
  TotalOcc: Integer;
  ErrMsg: string;
begin
  if (FProject = nil) or (FFindCombo.Text = '') then
    Exit;
  AddToHistory(FFindCombo, FFindCombo.Text);
  FCountLabel.Caption := SFindSearching;
  Application.ProcessMessages;
  Screen.BeginWaitCursor;
  try
    if not ScanProject(False, TotalOcc, ErrMsg) then
      Exit;
  finally
    Screen.EndWaitCursor;
  end;
  if Length(FMatches) = 0 then
    FCountLabel.Caption := SFindNoMatches
  else
    FCountLabel.Caption := Format(SFindCountFmt, [TotalOcc, Length(FMatches)]);
  PopulateResultsList(False);
  UpdateTabUI;
end;

procedure TFrmFindInProject.DoFindStep(ForwardDir: Boolean);
var
  NextIdx: Integer;
begin
  if Length(FMatches) = 0 then
  begin
    DoFindAll(True);
    Exit;
  end;

  if ForwardDir then
  begin
    if FCurrentIndex < High(FMatches) then
      NextIdx := FCurrentIndex + 1
    else if FWrapAroundCheck.Checked then
      NextIdx := 0
    else
      Exit;
  end
  else
  begin
    if FCurrentIndex > 0 then
      NextIdx := FCurrentIndex - 1
    else if FWrapAroundCheck.Checked then
      NextIdx := High(FMatches)
    else
      Exit;
  end;
  GoToResult(NextIdx);
end;

procedure TFrmFindInProject.DoReplaceCurrent;
var
  TextType: TTipoTextoBiblico;
  Refs, Texts: TStringList;
  Applied, TargetIdx, TotalOcc: Integer;
  ErrMsg, SavedRef: string;
begin
  if FProject = nil then
    Exit;
  if not GetSelectedTextType(TextType) then
  begin
    MessageDlg(SReplaceNotEditable, mtInformation, [mbOK], 0);
    Exit;
  end;
  if Length(FMatches) = 0 then
  begin
    DoFindAll(True);
    Exit;
  end;
  if (FCurrentIndex < 0) or (FCurrentIndex > High(FMatches)) then
    FCurrentIndex := 0;

  AddToHistory(FFindCombo, FFindCombo.Text);
  AddToHistory(FReplaceCombo, FReplaceCombo.Text);

  { Ensure we have up-to-date replacement text for the selected verse }
  if not ScanProject(True, TotalOcc, ErrMsg) then
    Exit;
  if Length(FMatches) = 0 then
  begin
    PopulateResultsList(True);
    FCountLabel.Caption := SFindNoMatches;
    UpdateTabUI;
    Exit;
  end;
  if FCurrentIndex > High(FMatches) then
    FCurrentIndex := 0;
  TargetIdx := FCurrentIndex;
  SavedRef := FMatches[TargetIdx].ReferenceID;

  Refs := TStringList.Create;
  Texts := TStringList.Create;
  try
    Refs.Add(FMatches[TargetIdx].ReferenceID);
    Texts.Add(FMatches[TargetIdx].ReplacedText);

    FProject.BeginChangeLogBatch;
    try
      FProject.SaveProject;
      Applied := FProject.ApplyVerseTexts(Refs, Texts, TextType,
        FKeepAssocCheck.Checked, nil, ErrMsg);
      if Applied > 0 then
      begin
        FProject.LogProjectChange('Find & Replace',
          Format('Replaced %d occurrence(s) in %s (%s)', [
            FMatches[TargetIdx].MatchCount,
            FMatches[TargetIdx].FormattedRef,
            GetScopeDisplayName
          ]),
          Format('Find: %s | Replace: %s', [FFindCombo.Text, FReplaceCombo.Text]));
        FProject.SaveProject;
      end;
    finally
      FProject.EndChangeLogBatch;
    end;

    if ErrMsg <> '' then
      MessageDlg(Format(SReplaceErrorFmt, [ErrMsg]), mtError, [mbOK], 0);
  finally
    Texts.Free;
    Refs.Free;
  end;

  { Re-scan and advance to next remaining match }
  if ScanProject(True, TotalOcc, ErrMsg) then
  begin
    PopulateResultsList(True);
    if Length(FMatches) = 0 then
    begin
      FCountLabel.Caption := Format(SReplaceDoneFmt, [1, 1]) + ' ' + SFindNoMatches;
      FCurrentIndex := -1;
    end
    else
    begin
      FCountLabel.Caption := Format(SFindCountFmt, [TotalOcc, Length(FMatches)]);
      if TargetIdx <= High(FMatches) then
      begin
        if (FMatches[TargetIdx].ReferenceID = SavedRef) and (TargetIdx < High(FMatches)) then
          GoToResult(TargetIdx + 1)
        else
          GoToResult(TargetIdx);
      end
      else if FWrapAroundCheck.Checked then
        GoToResult(0);
    end;
    UpdateTabUI;
  end;
end;

procedure TFrmFindInProject.DoReplaceAll;
var
  TextType: TTipoTextoBiblico;
  TotalOcc, Applied, I, ChangedCount: Integer;
  ErrMsg, AssocNotice, PromptMsg: string;
  Refs, Texts: TStringList;
begin
  if (FProject = nil) or (FFindCombo.Text = '') then
    Exit;
  if not GetSelectedTextType(TextType) then
  begin
    MessageDlg(SReplaceNotEditable, mtInformation, [mbOK], 0);
    Exit;
  end;

  AddToHistory(FFindCombo, FFindCombo.Text);
  AddToHistory(FReplaceCombo, FReplaceCombo.Text);

  { Flush current verse before scanning SQLite so no live edit is lost }
  FProject.SaveProject;

  FCountLabel.Caption := SFindSearching;
  Application.ProcessMessages;
  Screen.BeginWaitCursor;
  try
    if not ScanProject(True, TotalOcc, ErrMsg) then
      Exit;
  finally
    Screen.EndWaitCursor;
  end;

  PopulateResultsList(True);
  if Length(FMatches) = 0 then
  begin
    FCountLabel.Caption := SFindNoMatches;
    UpdateTabUI;
    Exit;
  end;

  Refs := TStringList.Create;
  Texts := TStringList.Create;
  try
    ChangedCount := 0;
    for I := 0 to High(FMatches) do
    begin
      if FMatches[I].ReplacedText <> FMatches[I].OriginalText then
      begin
        Refs.Add(FMatches[I].ReferenceID);
        Texts.Add(FMatches[I].ReplacedText);
        Inc(ChangedCount);
      end;
    end;

    if ChangedCount = 0 then
    begin
      FCountLabel.Caption := SFindNoMatches;
      Exit;
    end;

    if FKeepAssocCheck.Checked then
      AssocNotice := SReplaceAllAssocKeep
    else
      AssocNotice := SReplaceAllAssocClear;

    PromptMsg := Format(SReplaceAllConfirmFmt, [
      TotalOcc, ChangedCount, GetScopeDisplayName, AssocNotice
    ]);
    if MessageDlg(SReplaceAllTitle, PromptMsg, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;

    FProject.BeginChangeLogBatch;
    try
      FormPleaseWait.BeginOperation(Self, SReplaceProgressTitle, SReplaceProgressMsg, 1, '');
      try
        Applied := FProject.ApplyVerseTexts(Refs, Texts, TextType,
          FKeepAssocCheck.Checked, FormPleaseWait.ProgressBar1, ErrMsg);
      finally
        FormPleaseWait.FinishOperation;
      end;
      if Applied > 0 then
      begin
        FProject.LogProjectChange('Find & Replace',
          Format('Replaced %d occurrence(s) across %d verse(s) in %s', [
            TotalOcc, Applied, GetScopeDisplayName
          ]),
          Format('Find: %s | Replace: %s | Keep associations: %s', [
            FFindCombo.Text, FReplaceCombo.Text, BoolToStr(FKeepAssocCheck.Checked, True)
          ]));
        FProject.SaveProject;
      end;
    finally
      FProject.EndChangeLogBatch;
    end;

    if ErrMsg <> '' then
      MessageDlg(Format(SReplaceErrorFmt, [ErrMsg]), mtError, [mbOK], 0)
    else
    begin
      FCountLabel.Caption := Format(SReplaceDoneFmt, [TotalOcc, Applied]);
      SetLength(FMatches, 0);
      FCurrentIndex := -1;
      FResultsList.Items.Clear;
      UpdateTabUI;
    end;
  finally
    Texts.Free;
    Refs.Free;
  end;
end;

procedure TFrmFindInProject.TabControlChange(Sender: TObject);
begin
  UpdateTabUI;
  if Length(FMatches) > 0 then
    PopulateResultsList(FTabControl.TabIndex = 1);
end;

procedure TFrmFindInProject.ModeRadioClick(Sender: TObject);
begin
  UpdateModeUI;
end;

procedure TFrmFindInProject.ScopeComboChange(Sender: TObject);
begin
  if FScopeCombo.ItemIndex = 4 then
    FRawTagsCheck.Checked := True;
  UpdateTabUI;
end;

procedure TFrmFindInProject.RawTagsCheckClick(Sender: TObject);
begin
  if Length(FMatches) > 0 then
    PopulateResultsList(FTabControl.TabIndex = 1);
end;

procedure TFrmFindInProject.FindNextButtonClick(Sender: TObject);
begin
  DoFindStep(True);
end;

procedure TFrmFindInProject.FindPrevButtonClick(Sender: TObject);
begin
  DoFindStep(False);
end;

procedure TFrmFindInProject.CountButtonClick(Sender: TObject);
begin
  DoCount;
end;

procedure TFrmFindInProject.FindAllButtonClick(Sender: TObject);
begin
  DoFindAll(True);
end;

procedure TFrmFindInProject.ReplaceButtonClick(Sender: TObject);
begin
  DoReplaceCurrent;
end;

procedure TFrmFindInProject.ReplaceAllButtonClick(Sender: TObject);
begin
  DoReplaceAll;
end;

procedure TFrmFindInProject.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmFindInProject.ResultsListSelectionChange(Sender: TObject;
  User: Boolean);
begin
  if User and (FResultsList.ItemIndex >= 0) then
    GoToResult(FResultsList.ItemIndex);
end;

procedure TFrmFindInProject.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Key := 0;
    DoFindAll(True);
  end
  else if Key = VK_F3 then
  begin
    Key := 0;
    DoFindStep(not (ssShift in Shift));
  end
  else if (Key = VK_H) and (ssCtrl in Shift) then
  begin
    Key := 0;
    SelectModeTab(True);
    if FReplaceCombo.CanFocus then
      FReplaceCombo.SetFocus;
  end
  else if (Key = VK_F) and (ssCtrl in Shift) then
  begin
    Key := 0;
    SelectModeTab(False);
    if FFindCombo.CanFocus then
      FFindCombo.SetFocus;
  end;
end;

end.
