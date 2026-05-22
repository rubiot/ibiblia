unit project_exporter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Forms, Dialogs, ComCtrls, LCLIntf, fgl, PCRE,
  BibleScope, MySwordModule, TheWordDictionary, ConcordanceExport,
  ai_batch_work_item, syntagm, project_verse_repository, verse_render_buffer;

type
  TRegexList = specialize TFPGList<IRegex>;

  TExportSessionProc = procedure of object;
  TExportReferenceFunc = function: string of object;

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
  end;

  TInterlinearTextExportOptions = record
    Properties: string;
    ErrorDialogTitle: string;
    CorruptedDataMessage: string;
    ExportToFileErrorMessage: string;
    VerseRuleReplaceErrorMessage: string;
  end;

  TConcordanceExportOptions = record
    Detailed: Boolean;
    Abbreviation: string;
    LanguageId: string;
    AnalyticalCaption: string;
    SyntheticCaption: string;
    ErrorDialogTitle: string;
    CorruptedDataMessage: string;
  end;

  TProjectExporter = class
  private
    FScope: TEscopoTexto;
    FVerseRepository: TProjectVerseRepository;
    FVerseRenderBuffer: TVerseRenderBuffer;
    procedure RequireDependencies;
    class procedure ConfigureProgressBar(ProgressBar: TProgressBar;
      MaxValue: Integer); static;
    class procedure AdvanceProgressBar(ProgressBar: TProgressBar;
      ProcessMessagesEvery: Integer); static;
    class function CurrentReference(GetReference: TExportReferenceFunc): string; static;
    class function FormatCorruptionMessage(const BaseMessage, Reference,
      Details: string): string; static;
    class function EncodeComments(const Comments: string): string; static;
    class procedure WriteTheWordBible(Verses: TStringList; const FileName,
      Properties: string); static;
    procedure WriteMySwordBible(Verses: TStringList; const FileName,
      Properties: string);
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
      GetReference: TExportReferenceFunc);
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
        if VerseData.Associations = '' then
          Lines.Add(VerseData.TranslatedText)
        else if FVerseRenderBuffer.TryBuildDestinationLineONT(
            VerseData.OriginalText,
            VerseData.TranslatedText,
            VerseData.Associations,
            Options.ExportMorphology,
            Options.ExportNAAsItalics,
            Options.MarkReusedStrongs,
            Options.MarkUntranslatedStrongs,
            RenderedLine,
            CorruptionDetails) then
          Lines.Add(RenderedLine)
        else
          Errors.Add(FormatCorruptionMessage(Options.CorruptedDataMessage,
            CurrentReference(GetReference), CorruptionDetails) + #13#10);

        if Options.ExportComments and not VerseData.Comments.IsEmpty then
          Lines[Lines.Count - 1] := Lines[Lines.Count - 1] + '<RF>' +
            EncodeComments(VerseData.Comments) + '<Rf>';

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
    begin
      Errors.SaveToFile(Options.ErrorReportFileName);
      OpenDocument(Options.ErrorReportFileName);
    end;
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
  RenderedLine: string;
  CorruptionDetails: string;
  RenderMode: TRenderInterlinearMode;
begin
  RequireDependencies;

  Lines := nil;
  try
    ConfigureProgressBar(ProgressBar, QLinhas[FScope]);
    Lines := TStringList.Create;
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
        if VerseData.Associations = '' then
          Lines.Add(VerseData.OriginalText)
        else if FVerseRenderBuffer.TryBuildInterlinearLine(
            VerseData.OriginalText,
            VerseData.TranslatedText,
            VerseData.Associations,
            RenderMode,
            RenderedLine,
            CorruptionDetails) then
          Lines.Add(RenderedLine)
        else
          MessageDlg(Options.ErrorDialogTitle,
            FormatCorruptionMessage(Options.CorruptedDataMessage,
              CurrentReference(GetReference), CorruptionDetails),
            mtError, [mbOK], 0);

        if Assigned(RulesRx) then
          for RuleIndex := 0 to RulesRx.Count - 1 do
          begin
            try
              Lines[Lines.Count - 1] := RulesRx[RuleIndex].Replace(
                Lines[Lines.Count - 1], ReplaceTo[RuleIndex]);
            except
              on E: Exception do
                ShowMessage(Options.VerseRuleReplaceErrorMessage + #13#10 + E.Message);
            end;
          end;

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
        MessageDlg(Options.ErrorDialogTitle, Options.ExportToFileErrorMessage,
          mtError, [mbOK], 0);
    end;
  finally
    FreeAndNil(Lines);
    if Assigned(ProgressBar) then
      ProgressBar.Visible := False;
  end;
end;

procedure TProjectExporter.ExportConcordance(const FileName: string;
  ProgressBar: TProgressBar; const Options: TConcordanceExportOptions;
  StartSession, FinishSession: TExportSessionProc;
  GetReference: TExportReferenceFunc);
var
  VerseData: TVerseData;
  Concordance: TConcordance;
  Entries: TStringList;
  Dictionary: TTheWordDictionary;
  LemmaRegex: IRegex;
  Match: IMatch;
  Pairs: TStringList;
  Entry: string;
  Strong: string;
  Lemma: string;
  CorruptionDetails: string;
  TitleText: string;
begin
  RequireDependencies;

  Concordance := nil;
  Entries := nil;
  Dictionary := nil;
  try
    ConfigureProgressBar(ProgressBar, QLinhas[FScope] + QStrongs[etONT]);

    Concordance := TConcordance.Create;
    Concordance.Detailed := Options.Detailed;

    if Options.Detailed then
      TitleText := Format('%s %s', [Options.Abbreviation, Options.AnalyticalCaption])
    else
      TitleText := Format('%s %s', [Options.Abbreviation, Options.SyntheticCaption]);

    Dictionary := TTheWordDictionary.Create(FileName, True);
    with Dictionary do
    begin
      Title := TitleText;
      TitleEnglish := Format('%s %s', [Options.Abbreviation,
        IfThen(Options.Detailed, 'Analytical Concordance', 'Synthetic Concordance')]);
      Language := Options.LanguageId;
      Description := TitleText;
      EnglishDescription := TitleEnglish;
      VersionDate := FormatDateTime('YYYY-MM-DD', Now);
      Abbrev := Options.Abbreviation;
    end;

    if Assigned(StartSession) then
      StartSession;
    try
      FVerseRepository.First;
      while not FVerseRepository.EOF do
      begin
        VerseData := FVerseRepository.ReadCurrentVerseData;
        if VerseData.Associations <> '' then
        begin
          if FVerseRenderBuffer.TryExtractPairsList(
              VerseData.OriginalText,
              VerseData.TranslatedText,
              VerseData.Associations,
              plAll,
              Pairs,
              CorruptionDetails) then
          begin
            Concordance.AddPhrase(Pairs, FVerseRepository.CurrentReferenceID);
            Pairs.Free;
          end
          else
            MessageDlg(Options.ErrorDialogTitle,
              FormatCorruptionMessage(Options.CorruptedDataMessage,
                CurrentReference(GetReference), CorruptionDetails),
              mtError, [mbOK], 0);
        end;

        AdvanceProgressBar(ProgressBar, 20);
        FVerseRepository.Next;
      end;

      Entries := TStringList.Create;
      Entries.LoadFromFile('lemmas.dat');
      LemmaRegex := RegexCreate('^(.*?),(.*?)$', [rcoUTF8]);
      for Entry in Entries do
      begin
        Match := LemmaRegex.Match(Entry);
        if Match.Success then
        begin
          Strong := Match.Groups[1].Value;
          Lemma := Match.Groups[2].Value;
          Dictionary.AddEntry(Strong, Lemma, Concordance.GetStrongRtf(Lemma, Strong));
        end;

        AdvanceProgressBar(ProgressBar, 20);
      end;

      Dictionary.Commit;
    finally
      if Assigned(FinishSession) then
        FinishSession;
    end;
  finally
    Application.ProcessMessages;
    FreeAndNil(Entries);
    FreeAndNil(Concordance);
    FreeAndNil(Dictionary);
    if Assigned(ProgressBar) then
      ProgressBar.Visible := False;
  end;
end;

end.