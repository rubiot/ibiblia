unit FormFindInProject;

(* Find & Replace in Project: a modeless Notepad++-style search and bulk-replace
  window for Dark-Skinned iBiblia (v1.1.0.091).
  Features:
  - Top Find / Replace tabs (Ctrl+F / Ctrl+H)
  - Find what & Replace with history dropdowns + Swap button
  - Action buttons: Find Next, Find Prev, Find All, Replace, Replace All,
    Undo (Ctrl+Z), Redo (Ctrl+Y), Close
  - 7-step session Undo/Redo history that restores all modified verses even
    after the project has been saved, with confirmation prompts and
    association preservation
  - Look in scope (Destination, Source, Ref 1, Ref 2, Strong's/Morph, Comments, All)
  - Range filter (Whole project, Current book, Current chapter)
  - Options: Match case, Wrap around, Keep word associations, Show raw tags
  - Search Mode: Normal, Extended (\n, \r, \t, \0, \x.., \u....),
    Regular expression (PCRE) + . matches newline, capture groups $1/\1/${name},
    and \u/\l/\U/\L/\E case transforms
  - Transparency (On losing focus / Always + opacity slider)
  - Before ==> After live diff preview in results list
  - Responsive AlRight / AlClient layout (zero clipping on Windows & Linux GTK2) *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Math, Forms, Controls, Graphics, StdCtrls,
  ExtCtrls, ComCtrls, Dialogs, LCLType, LazUTF8, PCRE,
  ui_style, Project, unitpleasewait;

const
  MAX_FIND_REPLACE_HISTORY = 7;

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

  TFindReplaceSnapshot = record
    Description: string;
    ScopeName: string;
    TextType: TTipoTextoBiblico;
    KeepAssociations: Boolean;
    Refs: array of string;
    BeforeTexts: array of string;
    AfterTexts: array of string;
  end;

  { TFrmFindInProject }

  TFrmFindInProject = class(TForm)
  private
    FProject: TProject;
    FMatches: array of TFindMatchEntry;
    FCurrentIndex: Integer;
    FUndoStack: array of TFindReplaceSnapshot;
    FRedoStack: array of TFindReplaceSnapshot;
    FOnNavigate: TFindNavigateEvent;

    FTabControl: TTabControl;
    FTopContainer: TPanel;
    FLeftPanel: TPanel;
    FButtonsPanel: TPanel;

    FFindLabel: TLabel;
    FFindCombo: TComboBox;
    FSwapButton: TButton;
    FReplaceLabel: TLabel;
    FReplaceCombo: TComboBox;
    FScopeLabel: TLabel;
    FScopeCombo: TComboBox;
    FRangeLabel: TLabel;
    FRangeCombo: TComboBox;

    FOptionsGroup: TGroupBox;
    FMatchCaseCheck: TCheckBox;
    FWrapAroundCheck: TCheckBox;
    FKeepAssocCheck: TCheckBox;
    FRawTagsCheck: TCheckBox;

    FModeGroup: TGroupBox;
    FModeNormalRadio: TRadioButton;
    FModeExtendedRadio: TRadioButton;
    FModeRegexRadio: TRadioButton;
    FDotMatchesNewlineCheck: TCheckBox;

    FTransGroup: TGroupBox;
    FTransEnableCheck: TCheckBox;
    FTransOnBlurRadio: TRadioButton;
    FTransAlwaysRadio: TRadioButton;
    FTransTrackBar: TTrackBar;

    FFindNextButton: TButton;
    FFindPrevButton: TButton;
    FFindAllButton: TButton;
    FReplaceButton: TButton;
    FReplaceAllButton: TButton;
    FUndoButton: TButton;
    FRedoButton: TButton;
    FCloseButton: TButton;

    FStatusPanel: TPanel;
    FCountLabel: TLabel;
    FResultsList: TListBox;

    procedure BuildControls;
    procedure LayoutLeftPanel;
    procedure RefreshCaptions;
    procedure UpdateModeUI;
    procedure UpdateTabUI;
    procedure UpdateUndoRedoButtons;
    procedure ClearUndoRedoHistory;
    procedure PushUndoSnapshot(const Snap: TFindReplaceSnapshot);
    procedure PushRedoSnapshot(const Snap: TFindReplaceSnapshot);
    procedure UpdateTransparency(IsActive: Boolean);
    procedure AddToHistory(Combo: TComboBox; const Value: string);
    function GetSearchMode: TFindSearchMode;
    function GetSelectedTextType(out TextType: TTipoTextoBiblico): Boolean;
    function GetScopeDisplayName: string;
    function GetRangeDisplayName: string;
    function MatchesSelectedRange(const RefID: string): Boolean;
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
    procedure DoFindStep(ForwardDir: Boolean);
    procedure DoReplaceCurrent;
    procedure DoReplaceAll;
    procedure DoUndo;
    procedure DoRedo;

    procedure FormShowHandler(Sender: TObject);
    procedure FormActivateHandler(Sender: TObject);
    procedure FormDeactivateHandler(Sender: TObject);
    procedure FormKeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LeftPanelResize(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure ModeRadioClick(Sender: TObject);
    procedure ScopeComboChange(Sender: TObject);
    procedure RawTagsCheckClick(Sender: TObject);
    procedure SwapButtonClick(Sender: TObject);
    procedure TransControlChanged(Sender: TObject);
    procedure FindNextButtonClick(Sender: TObject);
    procedure FindPrevButtonClick(Sender: TObject);
    procedure FindAllButtonClick(Sender: TObject);
    procedure ReplaceButtonClick(Sender: TObject);
    procedure ReplaceAllButtonClick(Sender: TObject);
    procedure UndoButtonClick(Sender: TObject);
    procedure RedoButtonClick(Sender: TObject);
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
  SFindRange = 'Ran&ge:';
  SSwapHint = 'Swap Find what and Replace with';
  SOptionsTitle = 'Options';
  SMatchCase = 'Match &case';
  SWrapAround = 'Wrap aro&und';
  SKeepAssociations = '&Keep word associations';
  SShowRawTags = 'Show raw &tags in preview';
  SSearchModeTitle = 'Search Mode';
  SModeNormal = '&Normal';
  SModeExtended = 'E&xtended (\n, \r, \t, \0, \x...)';
  SModeRegex = 'Regular &expression';
  SDotMatchesNewline = '. matches new&line';
  STransTitle = 'Transparency';
  STransEnable = 'Enab&le';
  STransOnBlur = 'On losing focus';
  STransAlways = 'Always';
  SBtnFindNext = '&Find Next';
  SBtnFindPrev = 'Find &Prev';
  SBtnFindAll = 'Find &All';
  SBtnReplace = '&Replace';
  SBtnReplaceAll = 'Replace A&ll';
  SBtnUndo = '&Undo';
  SBtnUndoCountFmt = '&Undo (%d)';
  SBtnRedo = 'Re&do';
  SBtnRedoCountFmt = 'Re&do (%d)';
  SBtnClose = '&Close';
  SFindPrompt = 'Enter text, tags, or regex and click Find All / Find Next (Ctrl+F = Find, Ctrl+H = Replace, Ctrl+Z/Y = Undo/Redo).';
  SFindSearching = 'Searching project...';
  SFindCountFmt = '%d occurrence(s) in %d verse(s) [%s]';
  SFindNoMatches = '0 occurrences found.';
  SReplaceNotEditable = 'Replace works on Destination text, Source text, Reference text 1, or Reference text 2. Please select one of those fields in "Look in".';
  SReplaceAllConfirmFmt = 'Replace %d occurrence(s) across %d verse(s) in %s (%s)?' + LineEnding + LineEnding + '%s';
  SReplaceAllAssocKeep = 'Existing word associations will be preserved wherever words remain intact, and changes will be saved to the project.';
  SReplaceAllAssocClear = 'Warning: "Keep word associations" is unchecked. Associations in modified verses may be cleared.';
  SReplaceAllTitle = 'Confirm Replace All';
  SReplaceProgressTitle = 'Find & Replace';
  SReplaceProgressMsg = 'Applying replacements and preserving associations...';
  SReplaceDoneFmt = 'Replaced %d occurrence(s) across %d verse(s) and saved the project.';
  SReplaceErrorFmt = 'Replace completed with error: %s';
  SUndoNothing = 'Nothing to undo in this Find & Replace session.';
  SRedoNothing = 'Nothing to redo in this Find & Replace session.';
  SUndoConfirmTitle = 'Confirm Undo Find & Replace';
  SUndoConfirmFmt = 'Undo the following Find & Replace operation?' + LineEnding + LineEnding +
    '%s' + LineEnding + LineEnding +
    'This will restore %d verse(s) in %s to their text before the replacement (even if the project was already saved) and save the project.';
  SRedoConfirmTitle = 'Confirm Redo Find & Replace';
  SRedoConfirmFmt = 'Redo the following Find & Replace operation?' + LineEnding + LineEnding +
    '%s' + LineEnding + LineEnding +
    'This will re-apply the replacement to %d verse(s) in %s and save the project.';
  SUndoProgressMsg = 'Undoing Find & Replace changes and preserving associations...';
  SRedoProgressMsg = 'Redoing Find & Replace changes and preserving associations...';
  SUndoDoneFmt = 'Undid "%s" (%d verse(s) restored and saved).';
  SRedoDoneFmt = 'Redid "%s" (%d verse(s) updated and saved).';
  SUndoErrorFmt = 'Undo completed with error: %s';
  SRedoErrorFmt = 'Redo completed with error: %s';
  SRegexErrorFmt = 'Invalid regular expression: %s';
  SFindScopeDestination = 'Destination text';
  SFindScopeSource = 'Source text';
  SFindScopeReference1 = 'Reference text 1';
  SFindScopeReference2 = 'Reference text 2';
  SFindScopeStrongs = 'Strong''s / morphology tags';
  SFindScopeComments = 'Comments';
  SFindScopeAll = 'All fields';
  SRangeWholeProject = 'Whole project';
  SRangeCurrentBook = 'Current book';
  SRangeCurrentChapter = 'Current chapter';

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

function UTF8CharByteLen(B: Byte): Integer;
begin
  if (B and $80) = 0 then Result := 1
  else if (B and $E0) = $C0 then Result := 2
  else if (B and $F0) = $E0 then Result := 3
  else if (B and $F8) = $F0 then Result := 4
  else Result := 1;
end;

{ Applies Notepad++ case modifiers (\u, \l, \U, \L, \E) to an expanded replacement. }
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
  FindInstance.LayoutLeftPanel;
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
  KeyPreview := True;
  Position := poMainFormCenter;
  BorderStyle := bsSizeToolWin;
  FormStyle := fsStayOnTop;
  ClientWidth := 860;
  ClientHeight := 600;
  Constraints.MinWidth := 720;
  Constraints.MinHeight := 480;
  OnShow := @FormShowHandler;
  OnActivate := @FormActivateHandler;
  OnDeactivate := @FormDeactivateHandler;
  OnKeyDown := @FormKeyDownHandler;
  BuildControls;
  RefreshCaptions;
  UpdateModeUI;
  UpdateTabUI;
  LayoutLeftPanel;
end;

destructor TFrmFindInProject.Destroy;
begin
  if FindInstance = Self then
    FindInstance := nil;
  inherited Destroy;
end;

procedure TFrmFindInProject.BuildControls;
begin
  { 1. Top Find | Replace tab bar — pinned at Top = 0 }
  FTabControl := TTabControl.Create(Self);
  FTabControl.Parent := Self;
  FTabControl.Top := 0;
  FTabControl.Align := alTop;
  FTabControl.Height := 32;
  FTabControl.OnChange := @TabControlChange;

  { 2. Main controls container directly below tab bar }
  FTopContainer := TPanel.Create(Self);
  FTopContainer.Parent := Self;
  FTopContainer.Top := 32;
  FTopContainer.Align := alTop;
  FTopContainer.Height := 288;
  FTopContainer.BevelOuter := bvNone;

  { 2a. Right-hand action button column (AlRight = never clips!) }
  FButtonsPanel := TPanel.Create(Self);
  FButtonsPanel.Parent := FTopContainer;
  FButtonsPanel.Align := alRight;
  FButtonsPanel.Width := 156;
  FButtonsPanel.BevelOuter := bvNone;

  FFindNextButton := TButton.Create(Self);
  FFindNextButton.Parent := FButtonsPanel;
  FFindNextButton.SetBounds(6, 8, 140, 28);
  FFindNextButton.OnClick := @FindNextButtonClick;

  FFindPrevButton := TButton.Create(Self);
  FFindPrevButton.Parent := FButtonsPanel;
  FFindPrevButton.SetBounds(6, 40, 140, 28);
  FFindPrevButton.OnClick := @FindPrevButtonClick;

  FFindAllButton := TButton.Create(Self);
  FFindAllButton.Parent := FButtonsPanel;
  FFindAllButton.SetBounds(6, 72, 140, 28);
  FFindAllButton.OnClick := @FindAllButtonClick;

  FReplaceButton := TButton.Create(Self);
  FReplaceButton.Parent := FButtonsPanel;
  FReplaceButton.SetBounds(6, 108, 140, 28);
  FReplaceButton.OnClick := @ReplaceButtonClick;

  FReplaceAllButton := TButton.Create(Self);
  FReplaceAllButton.Parent := FButtonsPanel;
  FReplaceAllButton.SetBounds(6, 140, 140, 28);
  FReplaceAllButton.OnClick := @ReplaceAllButtonClick;

  FUndoButton := TButton.Create(Self);
  FUndoButton.Parent := FButtonsPanel;
  FUndoButton.SetBounds(6, 176, 140, 28);
  FUndoButton.OnClick := @UndoButtonClick;

  FRedoButton := TButton.Create(Self);
  FRedoButton.Parent := FButtonsPanel;
  FRedoButton.SetBounds(6, 208, 140, 28);
  FRedoButton.OnClick := @RedoButtonClick;

  FCloseButton := TButton.Create(Self);
  FCloseButton.Parent := FButtonsPanel;
  FCloseButton.SetBounds(6, 248, 140, 28);
  FCloseButton.Cancel := True;
  FCloseButton.OnClick := @CloseButtonClick;

  { 2b. Left panel (AlClient = takes all remaining width, dynamically laid out) }
  FLeftPanel := TPanel.Create(Self);
  FLeftPanel.Parent := FTopContainer;
  FLeftPanel.Align := alClient;
  FLeftPanel.BevelOuter := bvNone;
  FLeftPanel.OnResize := @LeftPanelResize;

  FFindLabel := TLabel.Create(Self);
  FFindLabel.Parent := FLeftPanel;
  FFindLabel.Layout := tlCenter;

  FFindCombo := TComboBox.Create(Self);
  FFindCombo.Parent := FLeftPanel;
  FFindCombo.Style := csDropDown;
  FFindCombo.OnKeyDown := @EditKeyDown;
  FFindLabel.FocusControl := FFindCombo;

  FSwapButton := TButton.Create(Self);
  FSwapButton.Parent := FLeftPanel;
  FSwapButton.Caption := '<->';
  FSwapButton.ShowHint := True;
  FSwapButton.OnClick := @SwapButtonClick;

  FReplaceLabel := TLabel.Create(Self);
  FReplaceLabel.Parent := FLeftPanel;
  FReplaceLabel.Layout := tlCenter;

  FReplaceCombo := TComboBox.Create(Self);
  FReplaceCombo.Parent := FLeftPanel;
  FReplaceCombo.Style := csDropDown;
  FReplaceCombo.OnKeyDown := @EditKeyDown;
  FReplaceLabel.FocusControl := FReplaceCombo;

  FScopeLabel := TLabel.Create(Self);
  FScopeLabel.Parent := FLeftPanel;
  FScopeLabel.Layout := tlCenter;

  FScopeCombo := TComboBox.Create(Self);
  FScopeCombo.Parent := FLeftPanel;
  FScopeCombo.Style := csDropDownList;
  FScopeCombo.OnChange := @ScopeComboChange;
  FScopeLabel.FocusControl := FScopeCombo;

  FRangeLabel := TLabel.Create(Self);
  FRangeLabel.Parent := FLeftPanel;
  FRangeLabel.Layout := tlCenter;

  FRangeCombo := TComboBox.Create(Self);
  FRangeCombo.Parent := FLeftPanel;
  FRangeCombo.Style := csDropDownList;
  FRangeLabel.FocusControl := FRangeCombo;

  { Options GroupBox }
  FOptionsGroup := TGroupBox.Create(Self);
  FOptionsGroup.Parent := FLeftPanel;

  FMatchCaseCheck := TCheckBox.Create(Self);
  FMatchCaseCheck.Parent := FOptionsGroup;
  FMatchCaseCheck.SetBounds(10, 8, 200, 24);

  FWrapAroundCheck := TCheckBox.Create(Self);
  FWrapAroundCheck.Parent := FOptionsGroup;
  FWrapAroundCheck.SetBounds(10, 36, 200, 24);
  FWrapAroundCheck.Checked := True;

  FKeepAssocCheck := TCheckBox.Create(Self);
  FKeepAssocCheck.Parent := FOptionsGroup;
  FKeepAssocCheck.SetBounds(10, 64, 200, 24);
  FKeepAssocCheck.Checked := True;

  FRawTagsCheck := TCheckBox.Create(Self);
  FRawTagsCheck.Parent := FOptionsGroup;
  FRawTagsCheck.SetBounds(10, 92, 200, 24);
  FRawTagsCheck.OnClick := @RawTagsCheckClick;

  { Search Mode GroupBox }
  FModeGroup := TGroupBox.Create(Self);
  FModeGroup.Parent := FLeftPanel;

  FModeNormalRadio := TRadioButton.Create(Self);
  FModeNormalRadio.Parent := FModeGroup;
  FModeNormalRadio.SetBounds(10, 8, 220, 24);
  FModeNormalRadio.Checked := True;
  FModeNormalRadio.OnClick := @ModeRadioClick;

  FModeExtendedRadio := TRadioButton.Create(Self);
  FModeExtendedRadio.Parent := FModeGroup;
  FModeExtendedRadio.SetBounds(10, 36, 220, 24);
  FModeExtendedRadio.OnClick := @ModeRadioClick;

  FModeRegexRadio := TRadioButton.Create(Self);
  FModeRegexRadio.Parent := FModeGroup;
  FModeRegexRadio.SetBounds(10, 64, 220, 24);
  FModeRegexRadio.OnClick := @ModeRadioClick;

  FDotMatchesNewlineCheck := TCheckBox.Create(Self);
  FDotMatchesNewlineCheck.Parent := FModeGroup;
  FDotMatchesNewlineCheck.SetBounds(26, 92, 204, 24);

  { Transparency GroupBox }
  FTransGroup := TGroupBox.Create(Self);
  FTransGroup.Parent := FLeftPanel;

  FTransEnableCheck := TCheckBox.Create(Self);
  FTransEnableCheck.Parent := FTransGroup;
  FTransEnableCheck.SetBounds(10, 8, 160, 24);
  FTransEnableCheck.OnClick := @TransControlChanged;

  FTransOnBlurRadio := TRadioButton.Create(Self);
  FTransOnBlurRadio.Parent := FTransGroup;
  FTransOnBlurRadio.SetBounds(10, 36, 160, 24);
  FTransOnBlurRadio.Checked := True;
  FTransOnBlurRadio.OnClick := @TransControlChanged;

  FTransAlwaysRadio := TRadioButton.Create(Self);
  FTransAlwaysRadio.Parent := FTransGroup;
  FTransAlwaysRadio.SetBounds(10, 62, 160, 24);
  FTransAlwaysRadio.OnClick := @TransControlChanged;

  FTransTrackBar := TTrackBar.Create(Self);
  FTransTrackBar.Parent := FTransGroup;
  FTransTrackBar.SetBounds(8, 90, 160, 30);
  FTransTrackBar.Min := 90;
  FTransTrackBar.Max := 255;
  FTransTrackBar.Position := 190;
  FTransTrackBar.Frequency := 15;
  FTransTrackBar.TickStyle := tsNone;
  FTransTrackBar.OnChange := @TransControlChanged;

  { 3. Status bar above results list }
  FStatusPanel := TPanel.Create(Self);
  FStatusPanel.Parent := Self;
  FStatusPanel.Top := 320;
  FStatusPanel.Align := alTop;
  FStatusPanel.Height := 26;
  FStatusPanel.BevelOuter := bvNone;

  FCountLabel := TLabel.Create(Self);
  FCountLabel.Parent := FStatusPanel;
  FCountLabel.Align := alClient;
  FCountLabel.BorderSpacing.Left := 12;
  FCountLabel.BorderSpacing.Right := 12;
  FCountLabel.Layout := tlCenter;
  FCountLabel.Font.Style := [fsBold];

  { 4. Interactive results list }
  FResultsList := TListBox.Create(Self);
  FResultsList.Parent := Self;
  FResultsList.Align := alClient;
  FResultsList.BorderSpacing.Left := 12;
  FResultsList.BorderSpacing.Right := 12;
  FResultsList.BorderSpacing.Bottom := 12;
  FResultsList.OnSelectionChange := @ResultsListSelectionChange;

  ClearResults;
end;

procedure TFrmFindInProject.LayoutLeftPanel;
var
  W, Pad, LabelW, InputLeft, RightEdge, SwapW, ComboW: Integer;
  HalfArea, ScopeW, RangeLabelW, RangeComboW: Integer;
  BoxTop, BoxH, TotalBoxW, Gap, W1, W2, W3: Integer;
begin
  if not Assigned(FLeftPanel) then
    Exit;
  W := FLeftPanel.ClientWidth;
  if W < 360 then
    W := 520;

  Pad := 12;
  LabelW := 88;
  InputLeft := Pad + LabelW + 4;
  RightEdge := W - 8;
  SwapW := 38;

  { Row 1: Find what + Swap button }
  ComboW := Max(120, RightEdge - InputLeft - SwapW - 6);
  FFindLabel.SetBounds(Pad, 10, LabelW, 24);
  FFindCombo.SetBounds(InputLeft, 9, ComboW, 28);
  FSwapButton.SetBounds(InputLeft + ComboW + 6, 9, SwapW, 28);

  { Row 2: Replace with }
  FReplaceLabel.SetBounds(Pad, 44, LabelW, 24);
  FReplaceCombo.SetBounds(InputLeft, 43, ComboW, 28);

  { Row 3: Look in + Range }
  RangeLabelW := 48;
  HalfArea := RightEdge - InputLeft - RangeLabelW - 14;
  ScopeW := Max(140, (HalfArea * 56) div 100);
  RangeComboW := Max(110, RightEdge - (InputLeft + ScopeW + 10 + RangeLabelW + 4));
  FScopeLabel.SetBounds(Pad, 78, LabelW, 24);
  FScopeCombo.SetBounds(InputLeft, 77, ScopeW, 28);
  FRangeLabel.SetBounds(InputLeft + ScopeW + 10, 78, RangeLabelW, 24);
  FRangeCombo.SetBounds(InputLeft + ScopeW + 10 + RangeLabelW + 4, 77, RangeComboW, 28);

  { Row 4: Options, Search Mode, Transparency GroupBoxes }
  BoxTop := 114;
  BoxH := 162;
  Gap := 8;
  TotalBoxW := Max(340, RightEdge - Pad - (Gap * 2));
  W1 := (TotalBoxW * 34) div 100;
  W2 := (TotalBoxW * 38) div 100;
  W3 := TotalBoxW - W1 - W2;

  FOptionsGroup.SetBounds(Pad, BoxTop, W1, BoxH);
  FModeGroup.SetBounds(Pad + W1 + Gap, BoxTop, W2, BoxH);
  FTransGroup.SetBounds(Pad + W1 + Gap + W2 + Gap, BoxTop, W3, BoxH);

  FMatchCaseCheck.Width := Max(80, FOptionsGroup.ClientWidth - 18);
  FWrapAroundCheck.Width := Max(80, FOptionsGroup.ClientWidth - 18);
  FKeepAssocCheck.Width := Max(80, FOptionsGroup.ClientWidth - 18);
  FRawTagsCheck.Width := Max(80, FOptionsGroup.ClientWidth - 18);

  FModeNormalRadio.Width := Max(80, FModeGroup.ClientWidth - 18);
  FModeExtendedRadio.Width := Max(80, FModeGroup.ClientWidth - 18);
  FModeRegexRadio.Width := Max(80, FModeGroup.ClientWidth - 18);
  FDotMatchesNewlineCheck.Width := Max(80, FModeGroup.ClientWidth - 34);

  FTransEnableCheck.Width := Max(70, FTransGroup.ClientWidth - 18);
  FTransOnBlurRadio.Width := Max(70, FTransGroup.ClientWidth - 18);
  FTransAlwaysRadio.Width := Max(70, FTransGroup.ClientWidth - 18);
  FTransTrackBar.Width := Max(70, FTransGroup.ClientWidth - 16);
end;

procedure TFrmFindInProject.RefreshCaptions;
var
  Scope, RangeIdx, ActiveTab: Integer;
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
  FRangeLabel.Caption := SFindRange;
  FSwapButton.Hint := SSwapHint;

  FOptionsGroup.Caption := SOptionsTitle;
  FMatchCaseCheck.Caption := SMatchCase;
  FWrapAroundCheck.Caption := SWrapAround;
  FKeepAssocCheck.Caption := SKeepAssociations;
  FRawTagsCheck.Caption := SShowRawTags;

  FModeGroup.Caption := SSearchModeTitle;
  FModeNormalRadio.Caption := SModeNormal;
  FModeExtendedRadio.Caption := SModeExtended;
  FModeRegexRadio.Caption := SModeRegex;
  FDotMatchesNewlineCheck.Caption := SDotMatchesNewline;

  FTransGroup.Caption := STransTitle;
  FTransEnableCheck.Caption := STransEnable;
  FTransOnBlurRadio.Caption := STransOnBlur;
  FTransAlwaysRadio.Caption := STransAlways;

  FFindNextButton.Caption := SBtnFindNext;
  FFindPrevButton.Caption := SBtnFindPrev;
  FFindAllButton.Caption := SBtnFindAll;
  FReplaceButton.Caption := SBtnReplace;
  FReplaceAllButton.Caption := SBtnReplaceAll;
  FCloseButton.Caption := SBtnClose;

  Scope := FScopeCombo.ItemIndex;
  FScopeCombo.Items.Clear;
  FScopeCombo.Items.Add(SFindScopeDestination);
  FScopeCombo.Items.Add(SFindScopeSource);
  FScopeCombo.Items.Add(SFindScopeReference1);
  FScopeCombo.Items.Add(SFindScopeReference2);
  FScopeCombo.Items.Add(SFindScopeStrongs);
  FScopeCombo.Items.Add(SFindScopeComments);
  FScopeCombo.Items.Add(SFindScopeAll);
  if (Scope < 0) or (Scope >= FScopeCombo.Items.Count) then
    Scope := 0;
  FScopeCombo.ItemIndex := Scope;

  RangeIdx := FRangeCombo.ItemIndex;
  FRangeCombo.Items.Clear;
  FRangeCombo.Items.Add(SRangeWholeProject);
  FRangeCombo.Items.Add(SRangeCurrentBook);
  FRangeCombo.Items.Add(SRangeCurrentChapter);
  if (RangeIdx < 0) or (RangeIdx >= FRangeCombo.Items.Count) then
    RangeIdx := 0;
  FRangeCombo.ItemIndex := RangeIdx;

  UpdateUndoRedoButtons;
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
  FSwapButton.Visible := IsReplace;
  FReplaceButton.Visible := IsReplace;
  FReplaceAllButton.Visible := IsReplace;
  FKeepAssocCheck.Visible := IsReplace;

  CanReplaceScope := GetSelectedTextType(DummyType);
  FReplaceButton.Enabled := IsReplace and CanReplaceScope and (Length(FMatches) > 0);
  FReplaceAllButton.Enabled := IsReplace and CanReplaceScope;

  FTransOnBlurRadio.Enabled := FTransEnableCheck.Checked;
  FTransAlwaysRadio.Enabled := FTransEnableCheck.Checked;
  FTransTrackBar.Enabled := FTransEnableCheck.Checked;
  UpdateUndoRedoButtons;
end;

procedure TFrmFindInProject.UpdateUndoRedoButtons;
begin
  if Length(FUndoStack) > 0 then
  begin
    FUndoButton.Caption := Format(SBtnUndoCountFmt, [Length(FUndoStack)]);
    FUndoButton.Enabled := (FProject <> nil);
  end
  else
  begin
    FUndoButton.Caption := SBtnUndo;
    FUndoButton.Enabled := False;
  end;

  if Length(FRedoStack) > 0 then
  begin
    FRedoButton.Caption := Format(SBtnRedoCountFmt, [Length(FRedoStack)]);
    FRedoButton.Enabled := (FProject <> nil);
  end
  else
  begin
    FRedoButton.Caption := SBtnRedo;
    FRedoButton.Enabled := False;
  end;
end;

procedure TFrmFindInProject.ClearUndoRedoHistory;
begin
  SetLength(FUndoStack, 0);
  SetLength(FRedoStack, 0);
  UpdateUndoRedoButtons;
end;

procedure TFrmFindInProject.PushUndoSnapshot(const Snap: TFindReplaceSnapshot);
var
  I, N: Integer;
begin
  N := Length(FUndoStack);
  if N >= MAX_FIND_REPLACE_HISTORY then
  begin
    for I := 0 to N - 2 do
      FUndoStack[I] := FUndoStack[I + 1];
    FUndoStack[N - 1] := Snap;
  end
  else
  begin
    SetLength(FUndoStack, N + 1);
    FUndoStack[N] := Snap;
  end;
  UpdateUndoRedoButtons;
end;

procedure TFrmFindInProject.PushRedoSnapshot(const Snap: TFindReplaceSnapshot);
var
  I, N: Integer;
begin
  N := Length(FRedoStack);
  if N >= MAX_FIND_REPLACE_HISTORY then
  begin
    for I := 0 to N - 2 do
      FRedoStack[I] := FRedoStack[I + 1];
    FRedoStack[N - 1] := Snap;
  end
  else
  begin
    SetLength(FRedoStack, N + 1);
    FRedoStack[N] := Snap;
  end;
  UpdateUndoRedoButtons;
end;

procedure TFrmFindInProject.UpdateTransparency(IsActive: Boolean);
begin
  FTransOnBlurRadio.Enabled := FTransEnableCheck.Checked;
  FTransAlwaysRadio.Enabled := FTransEnableCheck.Checked;
  FTransTrackBar.Enabled := FTransEnableCheck.Checked;

  if not FTransEnableCheck.Checked then
  begin
    AlphaBlend := False;
    AlphaBlendValue := 255;
  end
  else if FTransAlwaysRadio.Checked or (not IsActive) then
  begin
    AlphaBlendValue := Byte(EnsureRange(FTransTrackBar.Position, 90, 255));
    AlphaBlend := True;
  end
  else
  begin
    AlphaBlend := False;
    AlphaBlendValue := 255;
  end;
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

function TFrmFindInProject.GetRangeDisplayName: string;
begin
  if (FRangeCombo.ItemIndex >= 0) and (FRangeCombo.ItemIndex < FRangeCombo.Items.Count) then
    Result := FRangeCombo.Items[FRangeCombo.ItemIndex]
  else
    Result := SRangeWholeProject;
end;

function TFrmFindInProject.MatchesSelectedRange(const RefID: string): Boolean;
var
  CurParts: TStringArray;
  Prefix: string;
begin
  if (FRangeCombo.ItemIndex <= 0) or (FProject = nil) or (FProject.ID = '') then
    Exit(True);
  CurParts := FProject.ID.Split([',']);
  if Length(CurParts) < 2 then
    Exit(True);
  if FRangeCombo.ItemIndex = 1 then
    Prefix := CurParts[0] + ','
  else
    Prefix := CurParts[0] + ',' + CurParts[1] + ',';
  Result := (Pos(Prefix, RefID) = 1);
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
  LiteralRepl, SubVal: string;
begin
  Result := 0;
  OutputText := Input;
  if (Rx = nil) or (Input = '') then
    Exit;

  Mode := GetSearchMode;

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
        if not MatchesSelectedRange(Refs[I]) then
          Continue;
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
    case FScopeCombo.ItemIndex of
      4: ScopeEnum := ssStrongs;
      5: ScopeEnum := ssComments;
    else
      ScopeEnum := ssAll;
    end;
    FallbackResults := FProject.SearchVerses(Trim(FFindCombo.Text), ScopeEnum);
    for I := 0 to High(FallbackResults) do
    begin
      if not MatchesSelectedRange(FallbackResults[I].ReferenceID) then
        Continue;
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
  begin
    ClearResults;
    ClearUndoRedoHistory;
  end;
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
    FCountLabel.Caption := Format(SFindCountFmt, [TotalOcc, Length(FMatches), GetRangeDisplayName]);

  FCurrentIndex := -1;
  UpdateTabUI;
  if NavigateFirst and (Length(FMatches) > 0) then
    GoToResult(0);
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
  Snap: TFindReplaceSnapshot;
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

  if FMatches[TargetIdx].ReplacedText = FMatches[TargetIdx].OriginalText then
    Exit;

  Refs := TStringList.Create;
  Texts := TStringList.Create;
  try
    Refs.Add(FMatches[TargetIdx].ReferenceID);
    Texts.Add(FMatches[TargetIdx].ReplacedText);

    Snap.Description := Format('Replace in %s ("%s" -> "%s")', [
      FMatches[TargetIdx].FormattedRef, FFindCombo.Text, FReplaceCombo.Text
    ]);
    Snap.ScopeName := GetScopeDisplayName;
    Snap.TextType := TextType;
    Snap.KeepAssociations := FKeepAssocCheck.Checked;
    SetLength(Snap.Refs, 1);
    SetLength(Snap.BeforeTexts, 1);
    SetLength(Snap.AfterTexts, 1);
    Snap.Refs[0] := FMatches[TargetIdx].ReferenceID;
    Snap.BeforeTexts[0] := FMatches[TargetIdx].OriginalText;
    Snap.AfterTexts[0] := FMatches[TargetIdx].ReplacedText;

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
        SetLength(FRedoStack, 0);
        PushUndoSnapshot(Snap);
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
      FCountLabel.Caption := Format(SFindCountFmt, [TotalOcc, Length(FMatches), GetRangeDisplayName]);
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
  Snap: TFindReplaceSnapshot;
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
      TotalOcc, ChangedCount, GetScopeDisplayName, GetRangeDisplayName, AssocNotice
    ]);
    if MessageDlg(SReplaceAllTitle, PromptMsg, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;

    Snap.Description := Format('Replace All (%d verse(s) in %s: "%s" -> "%s")', [
      ChangedCount, GetScopeDisplayName, FFindCombo.Text, FReplaceCombo.Text
    ]);
    Snap.ScopeName := GetScopeDisplayName;
    Snap.TextType := TextType;
    Snap.KeepAssociations := FKeepAssocCheck.Checked;
    SetLength(Snap.Refs, ChangedCount);
    SetLength(Snap.BeforeTexts, ChangedCount);
    SetLength(Snap.AfterTexts, ChangedCount);
    Applied := 0;
    for I := 0 to High(FMatches) do
    begin
      if FMatches[I].ReplacedText <> FMatches[I].OriginalText then
      begin
        Snap.Refs[Applied] := FMatches[I].ReferenceID;
        Snap.BeforeTexts[Applied] := FMatches[I].OriginalText;
        Snap.AfterTexts[Applied] := FMatches[I].ReplacedText;
        Inc(Applied);
      end;
    end;

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
          Format('Replaced %d occurrence(s) across %d verse(s) in %s (%s)', [
            TotalOcc, Applied, GetScopeDisplayName, GetRangeDisplayName
          ]),
          Format('Find: %s | Replace: %s | Keep associations: %s', [
            FFindCombo.Text, FReplaceCombo.Text, BoolToStr(FKeepAssocCheck.Checked, True)
          ]));
        FProject.SaveProject;
        SetLength(FRedoStack, 0);
        PushUndoSnapshot(Snap);
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

procedure TFrmFindInProject.DoUndo;
var
  Snap: TFindReplaceSnapshot;
  Refs, Texts: TStringList;
  I, Applied: Integer;
  ErrMsg, PromptMsg: string;
begin
  if FProject = nil then
    Exit;
  if Length(FUndoStack) = 0 then
  begin
    MessageDlg(SUndoNothing, mtInformation, [mbOK], 0);
    Exit;
  end;

  Snap := FUndoStack[High(FUndoStack)];
  PromptMsg := Format(SUndoConfirmFmt, [
    Snap.Description, Length(Snap.Refs), Snap.ScopeName
  ]);
  if MessageDlg(SUndoConfirmTitle, PromptMsg, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  Refs := TStringList.Create;
  Texts := TStringList.Create;
  try
    for I := 0 to High(Snap.Refs) do
    begin
      Refs.Add(Snap.Refs[I]);
      Texts.Add(Snap.BeforeTexts[I]);
    end;

    FProject.BeginChangeLogBatch;
    try
      FormPleaseWait.BeginOperation(Self, SReplaceProgressTitle, SUndoProgressMsg, 1, '');
      try
        Applied := FProject.ApplyVerseTexts(Refs, Texts, Snap.TextType,
          Snap.KeepAssociations, FormPleaseWait.ProgressBar1, ErrMsg);
      finally
        FormPleaseWait.FinishOperation;
      end;
      if Applied > 0 then
      begin
        FProject.LogProjectChange('Find & Replace Undo',
          Format('Undid %s (%d verse(s) restored)', [Snap.Description, Applied]),
          'Restored pre-replacement verse texts from Find & Replace Undo history.');
        FProject.SaveProject;
      end;
    finally
      FProject.EndChangeLogBatch;
    end;

    if ErrMsg <> '' then
      MessageDlg(Format(SUndoErrorFmt, [ErrMsg]), mtError, [mbOK], 0)
    else
    begin
      SetLength(FUndoStack, Length(FUndoStack) - 1);
      PushRedoSnapshot(Snap);
      FCountLabel.Caption := Format(SUndoDoneFmt, [Snap.Description, Applied]);
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

procedure TFrmFindInProject.DoRedo;
var
  Snap: TFindReplaceSnapshot;
  Refs, Texts: TStringList;
  I, Applied: Integer;
  ErrMsg, PromptMsg: string;
begin
  if FProject = nil then
    Exit;
  if Length(FRedoStack) = 0 then
  begin
    MessageDlg(SRedoNothing, mtInformation, [mbOK], 0);
    Exit;
  end;

  Snap := FRedoStack[High(FRedoStack)];
  PromptMsg := Format(SRedoConfirmFmt, [
    Snap.Description, Length(Snap.Refs), Snap.ScopeName
  ]);
  if MessageDlg(SRedoConfirmTitle, PromptMsg, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  Refs := TStringList.Create;
  Texts := TStringList.Create;
  try
    for I := 0 to High(Snap.Refs) do
    begin
      Refs.Add(Snap.Refs[I]);
      Texts.Add(Snap.AfterTexts[I]);
    end;

    FProject.BeginChangeLogBatch;
    try
      FormPleaseWait.BeginOperation(Self, SReplaceProgressTitle, SRedoProgressMsg, 1, '');
      try
        Applied := FProject.ApplyVerseTexts(Refs, Texts, Snap.TextType,
          Snap.KeepAssociations, FormPleaseWait.ProgressBar1, ErrMsg);
      finally
        FormPleaseWait.FinishOperation;
      end;
      if Applied > 0 then
      begin
        FProject.LogProjectChange('Find & Replace Redo',
          Format('Redid %s (%d verse(s) updated)', [Snap.Description, Applied]),
          'Re-applied replacement from Find & Replace Redo history.');
        FProject.SaveProject;
      end;
    finally
      FProject.EndChangeLogBatch;
    end;

    if ErrMsg <> '' then
      MessageDlg(Format(SRedoErrorFmt, [ErrMsg]), mtError, [mbOK], 0)
    else
    begin
      SetLength(FRedoStack, Length(FRedoStack) - 1);
      PushUndoSnapshot(Snap);
      FCountLabel.Caption := Format(SRedoDoneFmt, [Snap.Description, Applied]);
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

procedure TFrmFindInProject.FormShowHandler(Sender: TObject);
begin
  { Ensure top-to-bottom stacking order on Win32 and GTK2 }
  FTabControl.Top := 0;
  FTopContainer.Top := FTabControl.Height;
  FStatusPanel.Top := FTabControl.Height + FTopContainer.Height;
  LayoutLeftPanel;
  UpdateTransparency(True);
end;

procedure TFrmFindInProject.FormActivateHandler(Sender: TObject);
begin
  UpdateTransparency(True);
end;

procedure TFrmFindInProject.FormDeactivateHandler(Sender: TObject);
begin
  UpdateTransparency(False);
end;

procedure TFrmFindInProject.FormKeyDownHandler(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and not (ssAlt in Shift) then
  begin
    if (Key = VK_Z) and (Length(FUndoStack) > 0) then
    begin
      Key := 0;
      DoUndo;
    end
    else if (Key = VK_Y) and (Length(FRedoStack) > 0) then
    begin
      Key := 0;
      DoRedo;
    end;
  end;
end;

procedure TFrmFindInProject.LeftPanelResize(Sender: TObject);
begin
  LayoutLeftPanel;
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

procedure TFrmFindInProject.SwapButtonClick(Sender: TObject);
var
  Tmp: string;
begin
  Tmp := FFindCombo.Text;
  FFindCombo.Text := FReplaceCombo.Text;
  FReplaceCombo.Text := Tmp;
end;

procedure TFrmFindInProject.TransControlChanged(Sender: TObject);
begin
  UpdateTransparency(Active);
end;

procedure TFrmFindInProject.FindNextButtonClick(Sender: TObject);
begin
  DoFindStep(True);
end;

procedure TFrmFindInProject.FindPrevButtonClick(Sender: TObject);
begin
  DoFindStep(False);
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

procedure TFrmFindInProject.UndoButtonClick(Sender: TObject);
begin
  DoUndo;
end;

procedure TFrmFindInProject.RedoButtonClick(Sender: TObject);
begin
  DoRedo;
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
