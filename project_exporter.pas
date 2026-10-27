unit project_exporter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Math, Forms, ComCtrls, LCLIntf, fgl, PCRE,
  BibleScope, MySwordModule, TheWordDictionary, ConcordanceExport,
  ai_batch_work_item, syntagm, Verse, project_verse_repository, verse_render_buffer,
  SuggestionManager, ONTTokenizer, text_direction, rf_note_scanner,
  association_highlight_projection, export_file_staging;

type
  TRegexList = specialize TFPGList<IRegex>;

  TExportStatus = (esFailed, esSuccess, esWarnings);
  TExportResult = record
    Status: TExportStatus;
    OutputFileName: string;
    ProcessedVerseCount: Integer;
    WarningCount: Integer;
    ErrorReportFileName: string;
    ErrorMessage: string;
  end;

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
    Direction: TTextDirection;
    ErrorDialogTitle: string;
    CorruptedDataMessage: string;
    ExportToFileErrorMessage: string;
    ErrorReportFileName: string;
    ErrorReportCreatedMessage: string;
  end;

  TInterlinearTextExportOptions = record
    Properties: string;
    Direction: TTextDirection;
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
    FIndexedVerseCount: Integer;
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
    class procedure CompleteExport(var ExportResult: TExportResult;
      Errors: TStringList; const FileName: string); static;

    class procedure WriteTheWordBible(Verses: TStringList; const FileName,
      Properties: string; Direction: TTextDirection); static;
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
    function ExportDestinationTextWithStrongs(const FileName: string;
      ProgressBar: TProgressBar; const Options: TDestinationTextExportOptions;
      StartSession, FinishSession: TExportSessionProc;
      GetReference: TExportReferenceFunc): TExportResult;
    function ExportInterlinearText(const FileName: string;
      ProgressBar: TProgressBar; RulesRx: TRegexList; ReplaceTo: TStringList;
      const Options: TInterlinearTextExportOptions;
      StartSession, FinishSession: TExportSessionProc;
      GetReference: TExportReferenceFunc): TExportResult;
    function ExportConcordance(const FileName: string;
      ProgressBar: TProgressBar; const Options: TConcordanceExportOptions;
      StartSession, FinishSession: TExportSessionProc;
      GetReference: TExportReferenceFunc;
      SuggestionManager: TSuggestionManager = nil;
      ProgressEvent: TConcordanceExportProgressEvent = nil): TExportResult;
  end;

{$INCLUDE include/bookdefs.inc}

const
  QStrongs: array[etOT..etONT] of SmallInt = (8674, 5624, 14298);

implementation

resourcestring
  SExportWarningReportFailed = 'Could not save the export warnings report: %s';

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

class procedure TProjectExporter.CompleteExport(var ExportResult: TExportResult;
  Errors: TStringList; const FileName: string);
var
  Staging: TExportFileStaging;
begin
  ExportResult.Status := esSuccess;
  if not Assigned(Errors) then
    Exit;
  ExportResult.WarningCount := Errors.Count;
  if Errors.Count = 0 then
    Exit;
  ExportResult.Status := esWarnings;
  if FileName = '' then
    Exit;
  Staging := nil;
  try
    try
      Staging := TExportFileStaging.Create(FileName);
      Errors.SaveToFile(Staging.StagingFileName);
      Staging.Publish;
      ExportResult.ErrorReportFileName := Staging.FinalFileName;
    except
      on E: Exception do
      begin
        Inc(ExportResult.WarningCount);
        ExportResult.ErrorMessage := Format(SExportWarningReportFailed, [E.Message]);
      end;
    end;
  finally
    Staging.Free;
  end;
end;

class procedure TProjectExporter.WriteTheWordBible(Verses: TStringList;
  const FileName, Properties: string; Direction: TTextDirection);
var
  Stream: TFileStream;
begin
  Verses.Add('');
  Verses.Add(NormalizeTheWordR2LProperties(Properties, Direction));
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

function TProjectExporter.ExportDestinationTextWithStrongs(
  const FileName: string; ProgressBar: TProgressBar;
  const Options: TDestinationTextExportOptions; StartSession,
  FinishSession: TExportSessionProc; GetReference: TExportReferenceFunc): TExportResult;
var
  VerseData: TVerseData;
  Lines: TStringList;
  Errors: TStringList;
  HasTags: Boolean;
  RenderedLine: string;
  CorruptionDetails: string;
  ProjectionBaseMessage: string;
  Reference: string;
  Staging: TExportFileStaging;
begin
  Result := Default(TExportResult);
  Lines := nil;
  Errors := nil;
  Staging := nil;
  try
    try
    RequireDependencies;
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

        try
          RenderedLine := string(AppendEditorialCommentAsRF(
            UnicodeString(RenderedLine), UnicodeString(VerseData.Comments),
            Options.ExportComments));
        except
          on E: ERFEditorialCommentProjectionError do
          begin
            Reference := CurrentReference(GetReference);
            if Reference = '' then
              Reference := FVerseRepository.CurrentReferenceID;
            ProjectionBaseMessage := Options.CorruptedDataMessage;
            if ProjectionBaseMessage = '' then
              ProjectionBaseMessage :=
                'Unable to export editorial comment as a translator note.';
            raise ERFEditorialCommentProjectionError.Create(
              FormatCorruptionMessage(ProjectionBaseMessage, Reference,
                E.Message));
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

    Result.ProcessedVerseCount := Lines.Count;
    Staging := TExportFileStaging.Create(FileName);
      if SameText(ExtractFileExt(FileName), '.mybible') then
        WriteMySwordBible(Lines, Staging.StagingFileName, Options.Properties)
      else
      begin
        HasTags := (Lines.Count > 0) and
          ((Pos('<WG', Lines[0]) > 0) or (Pos('<WH', Lines[0]) > 0));
        if (Lines.Count > 0) and not HasTags then
          Lines[0] := Lines[0] + '<_MORPH_>';

        WriteTheWordBible(Lines, Staging.StagingFileName, Options.Properties,
          Options.Direction);
      end;
    if SameText(ExtractFileExt(FileName), '.mybible') then
      Staging.Publish(epSQLiteModule)
    else
      Staging.Publish(epTextModule);
    Result.OutputFileName := Staging.FinalFileName;
    CompleteExport(Result, Errors, Options.ErrorReportFileName);
    except
      on E: Exception do
      begin
        Result.Status := esFailed;
        Result.ErrorMessage := E.Message;
      end;
    end;
  finally
    Staging.Free;
    FreeAndNil(Lines);
    FreeAndNil(Errors);
    if Assigned(ProgressBar) then
      ProgressBar.Visible := False;
  end;
end;

function TProjectExporter.ExportInterlinearText(const FileName: string;
  ProgressBar: TProgressBar; RulesRx: TRegexList; ReplaceTo: TStringList;
  const Options: TInterlinearTextExportOptions; StartSession,
  FinishSession: TExportSessionProc; GetReference: TExportReferenceFunc): TExportResult;
var
  RuleIndex: Integer;
  VerseData: TVerseData;
  Lines: TStringList;
  Errors: TStringList;
  RenderedLine: string;
  CorruptionDetails: string;
  RenderMode: TRenderInterlinearMode;
  Staging: TExportFileStaging;
begin
  Result := Default(TExportResult);
  Lines := nil;
  Errors := nil;
  Staging := nil;
  try
    try
    RequireDependencies;
    ConfigureProgressBar(ProgressBar, QLinhas[FScope]);
    Lines := TStringList.Create;
    Errors := TStringList.Create;
    if SameText(ExtractFileExt(FileName), '.mybible') then
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

    Result.ProcessedVerseCount := Lines.Count;
    Staging := TExportFileStaging.Create(FileName);
      if SameText(ExtractFileExt(FileName), '.mybible') then
        WriteMySwordBible(Lines, Staging.StagingFileName, Options.Properties)
      else
        WriteTheWordBible(Lines, Staging.StagingFileName, Options.Properties,
          Options.Direction);
    if SameText(ExtractFileExt(FileName), '.mybible') then
      Staging.Publish(epSQLiteModule)
    else
      Staging.Publish(epTextModule);
    Result.OutputFileName := Staging.FinalFileName;
    CompleteExport(Result, Errors, Options.ErrorReportFileName);
    except
      on E: Exception do
      begin
        Result.Status := esFailed;
        Result.ErrorMessage := E.Message;
      end;
    end;
  finally
    Staging.Free;
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
  RFBody: string;
  RFClosingTag: string;
begin
  Result := '';
  Tokenizer := TTokenizerFactory.CreateTokenizer(
    FVerseRenderBuffer.DestinationVerse.TokenizerVersion, XML);
  try
    while Tokenizer.ReadSyntagm(Token) <> skNull do
    begin
      if Token.Kind = skTag then
      begin
        if IsRFOpeningTag(Token.Value) then
        begin
          ReadRFNoteBody(Tokenizer, RFBody, RFClosingTag);
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
  if Data.ReferenceID <> FLastIndexedReference then
  begin
    Inc(FIndexedVerseCount);
    FLastIndexedReference := Data.ReferenceID;
    if FIndexedIncludeVerseContext then
      FLastIndexedContext := PlainVerseText(Data.DestinationText);
  end;
  if FIndexedIncludeVerseContext then
    Context := FLastIndexedContext;

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

function TProjectExporter.ExportConcordance(const FileName: string;
  ProgressBar: TProgressBar; const Options: TConcordanceExportOptions;
  StartSession, FinishSession: TExportSessionProc;
  GetReference: TExportReferenceFunc; SuggestionManager: TSuggestionManager;
  ProgressEvent: TConcordanceExportProgressEvent): TExportResult;
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
  Staging: TExportFileStaging;

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
            BuildVisibleDestinationProjection(
              FVerseRenderBuffer.DestinationVerse, Context, SyntagmRanges);
          for AssociationIndex := 0 to High(AssociationData) do
          begin
            if not TryProjectAssociationHighlightRanges(
                AssociationData[AssociationIndex], Context, SyntagmRanges,
                HighlightRanges) then
              HighlightRanges := nil;
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
    FIndexedVerseCount := 0;
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
  Result := Default(TExportResult);
  Concordance := nil;
  Entries := nil;
  Errors := nil;
  Dictionary := nil;
  Staging := nil;
  try
    try
    RequireDependencies;
    Concordance := TConcordance.Create;
    Concordance.Mode := Options.Mode;
    Concordance.ContextMode := Options.ContextMode;
    Concordance.ContextMaxCharacters := Options.ContextMaxCharacters;
    Errors := TStringList.Create;

    if Options.Mode = cemAnalytical then
      TitleText := Format('%s %s', [Options.Abbreviation, Options.AnalyticalCaption])
    else
      TitleText := Format('%s %s', [Options.Abbreviation, Options.SyntheticCaption]);

    Staging := TExportFileStaging.Create(FileName);
    Dictionary := TTheWordDictionary.Create(Staging.StagingFileName, True);
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
      begin
        ReadIndexedAssociations;
        Result.ProcessedVerseCount := FIndexedVerseCount;
      end
      else
      begin
        ReadCanonicalAssociations;
        Result.ProcessedVerseCount := ReferenceOrder;
      end;

      Entries := TStringList.Create;
      Entries.LoadFromFile(IncludeTrailingPathDelimiter(
        ExtractFilePath(ParamStr(0))) + 'lemmas.dat');
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

    FreeAndNil(Dictionary);
    Staging.Publish(epSQLiteModule);
    Result.OutputFileName := Staging.FinalFileName;
    CompleteExport(Result, Errors, Options.ErrorReportFileName);
    except
      on E: Exception do
      begin
        Result.Status := esFailed;
        Result.ErrorMessage := E.Message;
      end;
    end;
  finally
    Application.ProcessMessages;
    FreeAndNil(Entries);
    FreeAndNil(Errors);
    FreeAndNil(Concordance);
    FreeAndNil(Dictionary);
    Staging.Free;
    if Assigned(ProgressBar) then
      ProgressBar.Visible := False;
  end;
end;

end.
