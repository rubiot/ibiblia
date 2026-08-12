unit project_exporter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Math, Forms, Dialogs, ComCtrls, LCLIntf, fgl, PCRE,
  BibleScope, MySwordModule, TheWordDictionary, ConcordanceExport,
  ai_batch_work_item, syntagm, Verse, project_verse_repository, verse_render_buffer,
  SuggestionManager, ONTTokenizer;

type
  TRegexList = specialize TFPGList<IRegex>;

  TExportSessionProc = procedure of object;
  TExportReferenceFunc = function: string of object;

  TConcordanceExportPhase = (
    cepReadingAssociations,
    cepFormattingEntries,
    cepWritingModule,
    cepFinalizing
  );

  TConcordanceExportProgressEvent = procedure(
    Phase: TConcordanceExportPhase; Current, Total: Integer) of object;

  TDestinationTextExportOptions = record
    ExportMorphology: Boolean;
    ExportNAAsItalics: Boolean;
    MarkReusedStrongs: Boolean;
    MarkUntranslatedStrongs: Boolean;
    ExportComments: Boolean;
    Properties: string;
    ErrorDialogTitle: string;
    CorruptedDataMessage: string;
    ExportToFileErrorMessage: string;
    ErrorReportFileName: string;
    ErrorReportCreatedMessage: string;
  end;

  TInterlinearTextExportOptions = record
    Properties: string;
    ErrorDialogTitle: string;
    CorruptedDataMessage: string;
    ExportToFileErrorMessage: string;
    VerseRuleReplaceErrorMessage: string;
    ErrorReportFileName: string;
    ErrorReportCreatedMessage: string;
  end;

  TConcordanceExportOptions = record
    Detailed: Boolean;
    Mode: TConcordanceExportMode;
    ContextMode: TConcordanceContextMode;
    ContextMaxCharacters: Integer;
    UseIndexedOccurrences: Boolean;
    Abbreviation: string;
    LanguageId: string;
    AnalyticalCaption: string;
    SyntheticCaption: string;
    ErrorDialogTitle: string;
    CorruptedDataMessage: string;
    ErrorReportFileName: string;
    ErrorReportCreatedMessage: string;
  end;

  TProjectExporter = class
  private
    FScope: TEscopoTexto;
    FVerseRepository: TProjectVerseRepository;
    FVerseRenderBuffer: TVerseRenderBuffer;
    FActiveConcordance: TConcordance;
    FIndexedIncludeVerseContext: Boolean;
    FIndexedProcessed: Integer;
    FIndexedTotal: Integer;
    FIndexedProgressEvent: TConcordanceExportProgressEvent;
    FLastIndexedReference: string;
    FLastIndexedContext: string;
    procedure RequireDependencies;
    class procedure ConfigureProgressBar(ProgressBar: TProgressBar;
      MaxValue: Integer); static;
    class procedure AdvanceProgressBar(ProgressBar: TProgressBar;
      ProcessMessagesEvery: Integer); static;
    class function CurrentReference(GetReference: TExportReferenceFunc): string; static;
    class function FormatCorruptionMessage(const BaseMessage, Reference,
      Details: string): string; static;
    class function StrongBelongsToScope(const Strong: string;
      Scope: TEscopoTexto): Boolean; static;
    class procedure SaveErrorReport(Errors: TStringList; const FileName,
      DialogTitle, DialogMessage: string); static;
    class function EncodeComments(const Comments: string): string; static;
    class procedure WriteTheWordBible(Verses: TStringList; const FileName,
      Properties: string); static;
    procedure WriteMySwordBible(Verses: TStringList; const FileName,
      Properties: string);
    procedure AddIndexedConcordanceOccurrence(
      const Data: TConcordanceOccurrenceData);
    function PlainVerseText(const XML: string): string;
  public
    constructor Create(AScope: TEscopoTexto; AVerseRepository: TProjectVerseRepository;
      AVerseRenderBuffer: TVerseRenderBuffer);
    procedure Attach(AScope: TEscopoTexto; AVerseRepository: TProjectVerseRepository;
      AVerseRenderBuffer: TVerseRenderBuffer);
    procedure ExportDestinationTextWithStrongs(const FileName: string;
      ProgressBar: TProgressBar; const Options: TDestinationTextExportOptions;
      StartSession, FinishSession: TExportSessionProc;
      GetReference: TExportReferenceFunc);
    procedure ExportInterlinearText(const FileName: string;
      ProgressBar: TProgressBar; RulesRx: TRegexList; ReplaceTo: TStringList;
      const Options: TInterlinearTextExportOptions;
      StartSession, FinishSession: TExportSessionProc;
      GetReference: TExportReferenceFunc);
    procedure ExportConcordance(const FileName: string;
      ProgressBar: TProgressBar; const Options: TConcordanceExportOptions;
      StartSession, FinishSession: TExportSessionProc;
      GetReference: TExportReferenceFunc;
      SuggestionManager: TSuggestionManager = nil;
      ProgressEvent: TConcordanceExportProgressEvent = nil);
  end;

{$INCLUDE include/bookdefs.inc}

const
  QStrongs: array[etOT..etONT] of SmallInt = (8674, 5624, 14298);

implementation

constructor TProjectExporter.Create(AScope: TEscopoTexto;
  AVerseRepository: TProjectVerseRepository; AVerseRenderBuffer: TVerseRenderBuffer);
begin
  inherited Create;
  Attach(AScope, AVerseRepository, AVerseRenderBuffer);
end;

procedure TProjectExporter.Attach(AScope: TEscopoTexto;
  AVerseRepository: TProjectVerseRepository; AVerseRenderBuffer: TVerseRenderBuffer);
begin
  FScope := AScope;
  FVerseRepository := AVerseRepository;
  FVerseRenderBuffer := AVerseRenderBuffer;
end;

procedure TProjectExporter.RequireDependencies;
begin
  if not Assigned(FVerseRepository) then
    raise Exception.Create('Project exporter repository is not attached');

  if not Assigned(FVerseRenderBuffer) then
    raise Exception.Create('Project exporter render buffer is not attached');

  if FScope = etNone then
    raise Exception.Create('Project exporter scope is not attached');
end;

class procedure TProjectExporter.ConfigureProgressBar(ProgressBar: TProgressBar;
  MaxValue: Integer);
begin
  if not Assigned(ProgressBar) then
    Exit;

  ProgressBar.Style := pbstNormal;
  ProgressBar.Position := 0;
  ProgressBar.Min := 0;
  ProgressBar.Max := MaxValue;
  ProgressBar.Step := 1;
  ProgressBar.Visible := True;
end;

class procedure TProjectExporter.AdvanceProgressBar(ProgressBar: TProgressBar;
  ProcessMessagesEvery: Integer);
begin
  if not Assigned(ProgressBar) then
    Exit;

  ProgressBar.StepIt;
  if (ProcessMessagesEvery > 0) and ((ProgressBar.Position mod ProcessMessagesEvery) = 0) then
    Application.ProcessMessages;
end;

class function TProjectExporter.CurrentReference(
  GetReference: TExportReferenceFunc): string;
begin
  if Assigned(GetReference) then
    Result := GetReference()
  else
    Result := '';
end;

class function TProjectExporter.FormatCorruptionMessage(const BaseMessage,
  Reference, Details: string): string;
begin
  Result := BaseMessage + #13#10#13#10 + Reference + #13#10 + Details;
end;

class function TProjectExporter.StrongBelongsToScope(const Strong: string;
  Scope: TEscopoTexto): Boolean;
var
  NormalizedStrong: string;
begin
  NormalizedStrong := UpperCase(Strong);
  case Scope of
    etOT:
      Result := NormalizedStrong.StartsWith('H');
    etNT:
      Result := NormalizedStrong.StartsWith('G');
    etONT:
      Result := True;
  else
    Result := False;
  end;
end;

class procedure TProjectExporter.SaveErrorReport(Errors: TStringList;
  const FileName, DialogTitle, DialogMessage: string);
var
  MessageText: string;
begin
  if (not Assigned(Errors)) or (Errors.Count = 0) or (FileName = '') then
    Exit;

  Errors.SaveToFile(FileName);
  if DialogMessage = '' then
    MessageText := FileName
  else if Pos('%s', DialogMessage) > 0 then
    MessageText := Format(DialogMessage, [FileName])
  else
    MessageText := DialogMessage + #13#10 + FileName;

  MessageDlg(DialogTitle, MessageText, mtWarning, [mbOK], 0);
end;

class function TProjectExporter.EncodeComments(const Comments: string): string;
begin
  Result := AnsiReplaceStr(
              AnsiReplaceStr(
                AnsiReplaceStr(Comments, #13#10, '<CM>'),
                #13, '<CM>'),
              #10, '<CM>');
end;

class procedure TProjectExporter.WriteTheWordBible(Verses: TStringList;
  const FileName, Properties: string);
var
  Stream: TFileStream;
begin
  Verses.Add('');
  Verses.Add(Properties);
  Verses[0] := #239#187#191 + Verses.Strings[0];
  Stream := TFileStream.Create(FileName, fmCreate or fmOpenWrite or fmShareDenyNone);
  try
    Stream.Size := 0;
    Verses.SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TProjectExporter.WriteMySwordBible(Verses: TStringList;
  const FileName, Properties: string);
var
  Module: TMySwordModule;
  BookIndex: SmallInt;
  ChapterIndex: SmallInt;
  VerseIndex: SmallInt;
  VerseLineIndex: SmallInt;
begin
  Module := TMySwordModule.Create(FileName, FScope in [etOT, etONT],
    FScope in [etNT, etONT], True, Properties);
  try
    VerseLineIndex := 0;
    for BookIndex := 0 to QLivros[FScope] - 1 do
      for ChapterIndex := 0 to QCapitulos[FScope][BookIndex] - 1 do
        for VerseIndex := 0 to QVersiculos[FScope][QCapLivros[FScope][BookIndex] + ChapterIndex - 1] - 1 do
        begin
          Module.AddVerse(Verses[VerseLineIndex], BookIndex + OffsetLivros[FScope] + 1,
            ChapterIndex + 1, VerseIndex + 1);
          Inc(VerseLineIndex);
        end;
    Module.Commit;
  finally
    Module.Free;
  end;
end;

procedure TProjectExporter.ExportDestinationTextWithStrongs(
  const FileName: string; ProgressBar: TProgressBar;
  const Options: TDestinationTextExportOptions; StartSession,
  FinishSession: TExportSessionProc; GetReference: TExportReferenceFunc);
var
  VerseData: TVerseData;
  Lines: TStringList;
  Errors: TStringList;
  HasTags: Boolean;
  RenderedLine: string;
  CorruptionDetails: string;
begin
  RequireDependencies;

  Lines := nil;
  Errors := nil;
  try
    ConfigureProgressBar(ProgressBar, QLinhas[FScope]);
    Lines := TStringList.Create;
    Errors := TStringList.Create;

    if Assigned(StartSession) then
      StartSession;
    try
      FVerseRepository.First;
      while not FVerseRepository.EOF do
      begin
        VerseData := FVerseRepository.ReadCurrentVerseData;
        RenderedLine := VerseData.TranslatedText;
        if (VerseData.Associations <> '') and
          (not FVerseRenderBuffer.TryBuildDestinationLineONT(
              VerseData.OriginalText,
              VerseData.TranslatedText,
              VerseData.Associations,
              Options.ExportMorphology,
              Options.ExportNAAsItalics,
              Options.MarkReusedStrongs,
              Options.MarkUntranslatedStrongs,
              RenderedLine,
              CorruptionDetails)) then
        begin
          Errors.Add(FormatCorruptionMessage(Options.CorruptedDataMessage,
            CurrentReference(GetReference), CorruptionDetails) + #13#10);
          RenderedLine := VerseData.TranslatedText;
        end;

        if Options.ExportComments and not VerseData.Comments.IsEmpty then
          RenderedLine := RenderedLine + '<RF>' + EncodeComments(VerseData.Comments) + '<Rf>';

        Lines.Add(RenderedLine);

        AdvanceProgressBar(ProgressBar, 50);
        FVerseRepository.Next;
      end;
    finally
      if Assigned(FinishSession) then
        FinishSession;
    end;

    try
      if FileName.EndsWith('.mybible') then
        WriteMySwordBible(Lines, FileName, Options.Properties)
      else
      begin
        HasTags := (Lines.Count > 0) and
          ((Pos('<WG', Lines[0]) > 0) or (Pos('<WH', Lines[0]) > 0));
        if (Lines.Count > 0) and not HasTags then
          Lines[0] := Lines[0] + '<_MORPH_>';

        WriteTheWordBible(Lines, FileName, Options.Properties);
      end;
    except
      on E: Exception do
        MessageDlg(Options.ErrorDialogTitle,
          Options.ExportToFileErrorMessage + #13#10 + E.Message, mtError, [mbOK], 0);
    end;

    if Errors.Count > 0 then
      SaveErrorReport(Errors, Options.ErrorReportFileName,
        Options.ErrorDialogTitle, Options.ErrorReportCreatedMessage);
  finally
    FreeAndNil(Lines);
    FreeAndNil(Errors);
    if Assigned(ProgressBar) then
      ProgressBar.Visible := False;
  end;
end;

procedure TProjectExporter.ExportInterlinearText(const FileName: string;
  ProgressBar: TProgressBar; RulesRx: TRegexList; ReplaceTo: TStringList;
  const Options: TInterlinearTextExportOptions; StartSession,
  FinishSession: TExportSessionProc; GetReference: TExportReferenceFunc);
var
  RuleIndex: Integer;
  VerseData: TVerseData;
  Lines: TStringList;
  Errors: TStringList;
  RenderedLine: string;
  CorruptionDetails: string;
  RenderMode: TRenderInterlinearMode;
begin
  RequireDependencies;

  Lines := nil;
  Errors := nil;
  try
    ConfigureProgressBar(ProgressBar, QLinhas[FScope]);
    Lines := TStringList.Create;
    Errors := TStringList.Create;
    if FileName.EndsWith('.mybible') then
      RenderMode := rimMySword
    else
      RenderMode := rimTheWord;

    if Assigned(StartSession) then
      StartSession;
    try
      FVerseRepository.First;
      while not FVerseRepository.EOF do
      begin
        VerseData := FVerseRepository.ReadCurrentVerseData;
        RenderedLine := VerseData.OriginalText;
        if (VerseData.Associations <> '') and
          (not FVerseRenderBuffer.TryBuildInterlinearLine(
              VerseData.OriginalText,
              VerseData.TranslatedText,
              VerseData.Associations,
              RenderMode,
              RenderedLine,
              CorruptionDetails)) then
        begin
          Errors.Add(FormatCorruptionMessage(Options.CorruptedDataMessage,
            CurrentReference(GetReference), CorruptionDetails) + #13#10);
          RenderedLine := VerseData.OriginalText;
        end;

        if Assigned(RulesRx) then
          for RuleIndex := 0 to RulesRx.Count - 1 do
          begin
            try
              RenderedLine := RulesRx[RuleIndex].Replace(RenderedLine,
                ReplaceTo[RuleIndex]);
            except
              on E: Exception do
                Errors.Add(Options.VerseRuleReplaceErrorMessage + #13#10#13#10 +
                  CurrentReference(GetReference) + #13#10 + E.Message + #13#10);
            end;
          end;

        Lines.Add(RenderedLine);

        AdvanceProgressBar(ProgressBar, 50);
        FVerseRepository.Next;
      end;
    finally
      if Assigned(FinishSession) then
        FinishSession;
    end;

    try
      if FileName.EndsWith('.mybible') then
        WriteMySwordBible(Lines, FileName, Options.Properties)
      else
        WriteTheWordBible(Lines, FileName, Options.Properties);
    except
      on E: Exception do
        MessageDlg(Options.ErrorDialogTitle,
          Options.ExportToFileErrorMessage + #13#10 + E.Message,
          mtError, [mbOK], 0);
    end;

    if Errors.Count > 0 then
      SaveErrorReport(Errors, Options.ErrorReportFileName,
        Options.ErrorDialogTitle, Options.ErrorReportCreatedMessage);
  finally
    FreeAndNil(Lines);
    FreeAndNil(Errors);
    if Assigned(ProgressBar) then
      ProgressBar.Visible := False;
  end;
end;

function TProjectExporter.PlainVerseText(const XML: string): string;
var
  Token: TSyntagmToken;
  Tokenizer: TONTTokenizer;
begin
  Result := '';
  Tokenizer := TTokenizerFactory.CreateTokenizer(
    FVerseRenderBuffer.DestinationVerse.TokenizerVersion, XML);
  try
    while Tokenizer.ReadSyntagm(Token) <> skNull do
    begin
      if Token.Kind = skTag then
      begin
        if Token.Value = '<RF>' then
        begin
          Tokenizer.ReadToTag(Token, '<Rf>');
          Continue;
        end;
        if AnsiStartsStr('<TS', Token.Value) then
        begin
          Tokenizer.ReadToTag(Token, '<Ts>');
          Continue;
        end;
      end;
      if Token.Kind in [skWhitespace, skSyntagm, skPunctuation] then
        Result := Result + Token.Value;
    end;
  finally
    Tokenizer.Free;
  end;
  Result := Trim(Result);
end;

procedure TProjectExporter.AddIndexedConcordanceOccurrence(
  const Data: TConcordanceOccurrenceData);
var
  Context: string;
begin
  if not Assigned(FActiveConcordance) then
    Exit;

  Context := '';
  if FIndexedIncludeVerseContext then
  begin
    if Data.ReferenceID <> FLastIndexedReference then
    begin
      FLastIndexedReference := Data.ReferenceID;
      FLastIndexedContext := PlainVerseText(Data.DestinationText);
    end;
    Context := FLastIndexedContext;
  end;

  FActiveConcordance.AddOccurrence(Data.SourcePhrase, Data.TargetPhrase,
    Data.ReferenceID, Context, Data.Occurrences, Data.ReferenceOrder);
  Inc(FIndexedProcessed);
  if Assigned(FIndexedProgressEvent) and
    ((FIndexedProcessed mod 100) = 0) then
  begin
    FIndexedProgressEvent(cepReadingAssociations,
      FIndexedProcessed, FIndexedTotal);
    Application.ProcessMessages;
  end;
end;

procedure TProjectExporter.ExportConcordance(const FileName: string;
  ProgressBar: TProgressBar; const Options: TConcordanceExportOptions;
  StartSession, FinishSession: TExportSessionProc;
  GetReference: TExportReferenceFunc; SuggestionManager: TSuggestionManager;
  ProgressEvent: TConcordanceExportProgressEvent);
var
  VerseData: TVerseData;
  Concordance: TConcordance;
  Entries: TStringList;
  Errors: TStringList;
  Dictionary: TTheWordDictionary;
  LemmaRegex: IRegex;
  Match: IMatch;
  AssociationData: TAssociationPairDataArray;
  Entry: string;
  Strong: string;
  Lemma: string;
  CorruptionDetails: string;
  TitleText: string;
  Context: string;
  ReferenceOrder: Integer;
  FormattedEntries: Integer;
  ReadingTotal: Integer;
  UseIndexedOccurrences: Boolean;
  UnindexedReferences: TStringList;

  procedure BuildDestinationContext(out PlainText: string;
    out SyntagmRanges: TConcordanceHighlightRangeArray);
  var
    ClosingTag: string;
    FirstVisibleByte: Integer;
    Index: Integer;
    LastVisibleByte: Integer;
    Syntagm: TSyntagm;
  begin
    PlainText := '';
    SyntagmRanges := nil;
    SetLength(SyntagmRanges,
      FVerseRenderBuffer.DestinationVerse.Syntagms.Count);
    ClosingTag := '';
    for Index := 0 to FVerseRenderBuffer.DestinationVerse.Syntagms.Count - 1 do
    begin
      Syntagm := FVerseRenderBuffer.DestinationVerse.Syntagms[Index];
      if Syntagm.Kind = skTag then
      begin
        if not ClosingTag.IsEmpty then
        begin
          if SameText(Syntagm.Text, ClosingTag) then
            ClosingTag := '';
        end
        else if SameText(Syntagm.Text, '<RF>') then
          ClosingTag := '<Rf>'
        else if AnsiStartsText('<TS', Syntagm.Text) then
          ClosingTag := '<Ts>';
        Continue;
      end;
      if not ClosingTag.IsEmpty or
         not (Syntagm.Kind in [skWhitespace, skSyntagm, skPunctuation]) then
        Continue;

      SyntagmRanges[Index].StartIndex := Length(PlainText) + 1;
      SyntagmRanges[Index].Length := Length(Syntagm.Text);
      PlainText := PlainText + Syntagm.Text;
    end;

    FirstVisibleByte := 1;
    while (FirstVisibleByte <= Length(PlainText)) and
          (PlainText[FirstVisibleByte] <= ' ') do
      Inc(FirstVisibleByte);
    LastVisibleByte := Length(PlainText);
    while (LastVisibleByte >= FirstVisibleByte) and
          (PlainText[LastVisibleByte] <= ' ') do
      Dec(LastVisibleByte);

    if (FirstVisibleByte > 1) or (LastVisibleByte < Length(PlainText)) then
    begin
      PlainText := Copy(PlainText, FirstVisibleByte,
        LastVisibleByte - FirstVisibleByte + 1);
      for Index := 0 to High(SyntagmRanges) do
        if SyntagmRanges[Index].Length > 0 then
        begin
          Dec(SyntagmRanges[Index].StartIndex, FirstVisibleByte - 1);
          if (SyntagmRanges[Index].StartIndex < 1) or
             (SyntagmRanges[Index].StartIndex +
              SyntagmRanges[Index].Length - 1 > Length(PlainText)) then
          begin
            SyntagmRanges[Index].StartIndex := 0;
            SyntagmRanges[Index].Length := 0;
          end;
        end;
    end;
  end;

  function GetAssociationHighlightRanges(
    const Data: TAssociationPairData;
    const SyntagmRanges: TConcordanceHighlightRangeArray):
    TConcordanceHighlightRangeArray;
  var
    Index: Integer;
    RangeIndex: Integer;
    SyntagmIndex: Integer;
  begin
    Result := nil;
    for Index := 0 to High(Data.TargetSyntagmIndexes) do
    begin
      SyntagmIndex := Data.TargetSyntagmIndexes[Index];
      if (SyntagmIndex < 0) or (SyntagmIndex > High(SyntagmRanges)) or
         (SyntagmRanges[SyntagmIndex].Length <= 0) then
      begin
        Result := nil;
        Exit;
      end;
      RangeIndex := Length(Result);
      SetLength(Result, RangeIndex + 1);
      Result[RangeIndex] := SyntagmRanges[SyntagmIndex];
    end;
  end;

  procedure ReportProgress(Phase: TConcordanceExportPhase;
    Current, Total: Integer; ForceMessages: Boolean = False);
  begin
    if Assigned(ProgressEvent) then
      ProgressEvent(Phase, Current, Total)
    else if Assigned(ProgressBar) then
    begin
      if Total > 0 then
      begin
        ProgressBar.Style := pbstNormal;
        ProgressBar.Max := Total;
        ProgressBar.Position := Min(Current, Total);
      end
      else
        ProgressBar.Style := pbstMarquee;
      ProgressBar.Visible := True;
      ProgressBar.Update;
    end;
    if ForceMessages then
      Application.ProcessMessages;
  end;

  procedure ReadCanonicalAssociations;
  var
    AssociationIndex: Integer;
    HighlightRanges: TConcordanceHighlightRangeArray;
    SyntagmRanges: TConcordanceHighlightRangeArray;
  begin
    HighlightRanges := nil;
    SyntagmRanges := nil;
    ReadingTotal := QLinhas[FScope];
    ReportProgress(cepReadingAssociations, 0, ReadingTotal, True);
    ReferenceOrder := 0;
    FVerseRepository.First;
    while not FVerseRepository.EOF do
    begin
      VerseData := FVerseRepository.ReadCurrentVerseData;
      if VerseData.Associations <> '' then
      begin
        SetLength(AssociationData, 0);
        if FVerseRenderBuffer.TryExtractAssociationPairData(
            VerseData.OriginalText,
            VerseData.TranslatedText,
            VerseData.Associations,
            plAll,
            AssociationData,
            CorruptionDetails) then
        begin
          Context := '';
          SetLength(SyntagmRanges, 0);
          if Options.ContextMode <> ccmReferencesOnly then
            BuildDestinationContext(Context, SyntagmRanges);
          for AssociationIndex := 0 to High(AssociationData) do
          begin
            HighlightRanges := GetAssociationHighlightRanges(
              AssociationData[AssociationIndex], SyntagmRanges);
            Concordance.AddOccurrence(
              AssociationData[AssociationIndex].SourcePhrase,
              AssociationData[AssociationIndex].TargetPhrase,
              FVerseRepository.CurrentReferenceID, Context, 1,
              ReferenceOrder, HighlightRanges);
          end;
        end
        else
          Errors.Add(FormatCorruptionMessage(Options.CorruptedDataMessage,
            CurrentReference(GetReference), CorruptionDetails) + #13#10);
      end;

      Inc(ReferenceOrder);
      if ((ReferenceOrder mod 20) = 0) then
        ReportProgress(cepReadingAssociations, ReferenceOrder,
          ReadingTotal, True);
      FVerseRepository.Next;
    end;
    ReportProgress(cepReadingAssociations, ReadingTotal,
      ReadingTotal, True);
  end;

  procedure ReadIndexedAssociations;
  begin
    ReadingTotal := SuggestionManager.IndexedConcordanceOccurrenceCount;
    ReportProgress(cepReadingAssociations, 0, ReadingTotal, True);
    FActiveConcordance := Concordance;
    FIndexedIncludeVerseContext :=
      Options.ContextMode <> ccmReferencesOnly;
    FIndexedProcessed := 0;
    FIndexedTotal := ReadingTotal;
    FIndexedProgressEvent := ProgressEvent;
    FLastIndexedReference := '';
    FLastIndexedContext := '';
    try
      SuggestionManager.EnumerateIndexedConcordanceOccurrences(
        @AddIndexedConcordanceOccurrence);
      ReportProgress(cepReadingAssociations, ReadingTotal,
        ReadingTotal, True);
    finally
      FActiveConcordance := nil;
      FIndexedProgressEvent := nil;
      FLastIndexedReference := '';
      FLastIndexedContext := '';
    end;
  end;

  function IndexedOccurrencesCoverCanonicalData(
    References: TStringList): Boolean;
  var
    ReferenceIndex: Integer;
    MissingPairs: TStringList;
    MissingVerse: TVerseData;
  begin
    Result := True;
    for ReferenceIndex := 0 to References.Count - 1 do
    begin
      if not FVerseRepository.TryReadVerseData(
          References[ReferenceIndex], '', MissingVerse) then
      begin
        Result := False;
        Exit;
      end;

      MissingPairs := nil;
      if FVerseRenderBuffer.TryExtractPairsList(
          MissingVerse.OriginalText, MissingVerse.TranslatedText,
          MissingVerse.Associations, plAll, MissingPairs,
          CorruptionDetails) then
      begin
        try
          // An associated verse without an indexed pair means the cache is
          // incomplete. A verse whose associations are all ignored syntagms
          // legitimately contributes no concordance entry.
          if MissingPairs.Count > 0 then
          begin
            Result := False;
            Exit;
          end;
        finally
          MissingPairs.Free;
        end;
      end
      else
        Errors.Add(FormatCorruptionMessage(Options.CorruptedDataMessage,
          References[ReferenceIndex], CorruptionDetails) + #13#10);
    end;
  end;
begin
  RequireDependencies;

  Concordance := nil;
  Entries := nil;
  Errors := nil;
  Dictionary := nil;
  try
    Concordance := TConcordance.Create;
    Concordance.Mode := Options.Mode;
    Concordance.ContextMode := Options.ContextMode;
    Concordance.ContextMaxCharacters := Options.ContextMaxCharacters;
    Errors := TStringList.Create;

    if Options.Mode = cemAnalytical then
      TitleText := Format('%s %s', [Options.Abbreviation, Options.AnalyticalCaption])
    else
      TitleText := Format('%s %s', [Options.Abbreviation, Options.SyntheticCaption]);

    Dictionary := TTheWordDictionary.Create(FileName, True);
    with Dictionary do
    begin
      Title := TitleText;
      TitleEnglish := Format('%s %s', [Options.Abbreviation,
        IfThen(Options.Mode = cemAnalytical,
          'Analytical Concordance', 'Synthetic Concordance')]);
      Language := Options.LanguageId;
      Description := TitleText;
      EnglishDescription := TitleEnglish;
      VersionDate := FormatDateTime('YYYY-MM-DD', Now);
      Abbrev := Options.Abbreviation;
    end;

    if Assigned(StartSession) then
      StartSession;
    try
      UseIndexedOccurrences := Options.UseIndexedOccurrences and
        (Options.Mode = cemSynthetic) and
        (Options.ContextMode = ccmReferencesOnly) and
        Assigned(SuggestionManager);
      if UseIndexedOccurrences then
      begin
        UnindexedReferences := SuggestionManager.GetUnindexedAssociatedReferences;
        try
          UseIndexedOccurrences :=
            not SuggestionManager.IndexedConcordanceHasTaggedTargets and
            IndexedOccurrencesCoverCanonicalData(UnindexedReferences);
        finally
          UnindexedReferences.Free;
        end;
      end;

      if UseIndexedOccurrences then
        ReadIndexedAssociations
      else
        ReadCanonicalAssociations;

      Entries := TStringList.Create;
      Entries.LoadFromFile('lemmas.dat');
      LemmaRegex := RegexCreate('^(.*?),(.*?)$', [rcoUTF8]);
      FormattedEntries := 0;
      ReportProgress(cepFormattingEntries, 0, QStrongs[FScope], True);
      for Entry in Entries do
      begin
        Match := LemmaRegex.Match(Entry);
        if Match.Success then
        begin
          Strong := Match.Groups[1].Value;
          if StrongBelongsToScope(Strong, FScope) then
          begin
            Lemma := Match.Groups[2].Value;
            Dictionary.AddEntry(Strong, Lemma, Concordance.GetStrongRtf(Lemma, Strong));
            Inc(FormattedEntries);
            if (FormattedEntries mod 20) = 0 then
              ReportProgress(cepFormattingEntries, FormattedEntries,
                QStrongs[FScope], True);
          end;
        end;
      end;

      ReportProgress(cepFormattingEntries, QStrongs[FScope],
        QStrongs[FScope], True);
      ReportProgress(cepWritingModule, 0, 0, True);
      Dictionary.Commit;
      ReportProgress(cepFinalizing, 1, 1, True);
    finally
      if Assigned(FinishSession) then
        FinishSession;
    end;

    if Errors.Count > 0 then
      SaveErrorReport(Errors, Options.ErrorReportFileName,
        Options.ErrorDialogTitle, Options.ErrorReportCreatedMessage);
  finally
    Application.ProcessMessages;
    FreeAndNil(Entries);
    FreeAndNil(Errors);
    FreeAndNil(Concordance);
    FreeAndNil(Dictionary);
    if Assigned(ProgressBar) then
      ProgressBar.Visible := False;
  end;
end;

end.
