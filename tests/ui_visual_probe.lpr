program ui_visual_probe;

{$mode objfpc}{$H+}
{$codepage utf8}

uses
  Interfaces, Forms, Controls, Classes, SysUtils, Graphics, StdCtrls, ComCtrls,
  ExtCtrls, Menus, IniFiles, Math, Types, FPImage, LCLType, LCLTranslator, sqlite3conn,
  sqldb, ImgList, ActnList, IntfGraphics, formPrincipal, Project, BibleScope, BibleTreeView,
  project_database_bootstrap, status_catalog, status_visuals, ui_icons,
  FormDictionaryPopup, FormProjectProperties, FormNewProject, FormProjectExport,
  FormApplicationPreferences, FormMergeProjects, FormApplyPatch, FormExportPatch,
  FormLlmOptions, FormAiProcessing, FormFlowExamples, FormStatusCatalog,
  FormVerseRules, FormInterlinearVerseRules, FormTheWordHighlighterColors,
  UnitPleaseWait, unitabout, PatchFile, ai_flow_config,
  theword_translator_note_editor, theword_ont_codec_v2;

{$R *.res}

type
  TModalProbe = class
    Name: string;
    procedure Capture(Data: PtrInt);
  end;

  TIconFamilyModalProbe = class(TModalProbe)
    Family: TAppIconFamily;
    procedure SelectAndCapture(Data: PtrInt);
  end;

  TStatusDrawTrace = class
    Original: TDrawItemEvent;
    Count: Integer;
    procedure Draw(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
  end;

  TProbeExceptionHandler = class
    procedure Handle(Sender: TObject; E: Exception);
  end;

var
  OutputDir, ProbeLanguage: string;
  TargetDPI: Integer;
  Report: TStringList;
  Definitions: TStatusDefinitions;
  Counts: TStatusCounts;
  ExceptionHandler: TProbeExceptionHandler;
  StatusDrawTrace: TStatusDrawTrace;

function Option(const Name, Default: string): string;
var
  I: Integer;
begin
  Result := Default;
  for I := 1 to ParamCount - 1 do
    if ParamStr(I) = Name then
      Exit(ParamStr(I + 1));
end;

procedure Log(const Value: string);
begin
  WriteLn(Value);
  Flush(Output);
  Report.Add(Value);
  Report.SaveToFile(OutputDir + 'report.tsv');
end;

procedure Require(Condition: Boolean; const MessageText: string);
begin
  if not Condition then
    raise Exception.Create(MessageText);
  Log('PASS'#9 + MessageText);
end;

procedure TProbeExceptionHandler.Handle(Sender: TObject; E: Exception);
begin
  WriteLn(StdErr, E.ClassName + ': ' + E.Message);
  DumpExceptionBackTrace(StdErr);
  Flush(StdErr);
  if Assigned(Report) then
    Log('FAIL'#9 + E.ClassName + #9 + E.Message);
  Halt(1);
end;
procedure BuildCatalog;
var
  I: Integer;
begin
  SetLength(Definitions, 12);
  SetLength(Counts, 12);
  for I := 0 to 11 do
  begin
    Definitions[I].Id := I;
    Definitions[I].Caption := Format('Etapa personalizada %d', [I + 1]);
    Definitions[I].Color := RGBToColor(32 + I * 16, 195 - I * 10, 65 + I * 12);
    Definitions[I].DisplayOrder := 11 - I;
    Counts[I].StatusId := I;
    Counts[I].Count := I + 1;
  end;
  Definitions[0].Color := clFuchsia;
  Definitions[1].Color := clBlack;
end;

procedure CheckStatusImages;
var
  Images: TImageList;
  Resolution: TCustomImageListResolution;
  Bitmap: TBitmap;
  Pixels: TLazIntfImage;
  I, J, X, Size, Padding, Border, Index: Integer;
  Segments: TStatusSegmentWidths;
  ActualColor: TFPColor;
  ExpectedColor: TColor;
begin
  Images := TImageList.Create(nil);
  Bitmap := TBitmap.Create;
  try
    ConfigureStatusImageList(Images);
    Require(Images.Scaled, 'Custom status images scale with control DPI');
    for I := 0 to High(Definitions) do
      AddStatusSwatch(Images, Definitions[I].Color);
    Index := AddStatusAggregateImage(Images, Counts, 78, Definitions);
    for I := Low(StatusMarkerResolutions) to High(StatusMarkerResolutions) do
    begin
      Size := StatusMarkerResolutions[I];
      Require(Images.FindResolution(Size, Resolution),
        Format('Native status resolution %dpx is present', [Size]));
      Resolution.GetBitmap(Index, Bitmap);
      Pixels := Bitmap.CreateIntfImage;
      try
        Require(Pixels.Colors[0, 0].Alpha = 0, 'Status marker padding is transparent');
        Padding := Max(1, Round(Size * 2 / StatusMarkerSize));
        Border := Max(1, Round(Size / StatusMarkerSize));
        X := Padding + Border;
        Segments := BuildStatusSegmentWidths(Counts, Definitions, 78, Size - 2 * X);
        Require(Length(Segments) = 12, 'Every custom state remains visible in aggregate');
        for J := 0 to High(Segments) do
        begin
          ActualColor := Pixels.Colors[X, Size div 2];
          ExpectedColor := ColorToRGB(GetStatusColor(Definitions, Segments[J].StatusId));
          Require((ActualColor.Alpha > 0) and
            (ActualColor.Red shr 8 = (ExpectedColor and $FF)) and
            (ActualColor.Green shr 8 = ((ExpectedColor shr 8) and $FF)) and
            (ActualColor.Blue shr 8 = ((ExpectedColor shr 16) and $FF)),
            Format('Custom color and display order preserved at %dpx, state %d',
              [Size, Segments[J].StatusId]));
          Inc(X, Segments[J].Width);
        end;
      finally
        Pixels.Free;
      end;
    end;
  finally
    Bitmap.Free;
    Images.Free;
  end;
end;

procedure CreateFixture(const FileName: string);
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Query: TSQLQuery;
  I: Integer;
  procedure Info(const Key, Value: string);
  begin
    Query.SQL.Text := 'INSERT INTO info(id, valor) VALUES(:key, :value)';
    Query.Params.ParamByName('key').AsString := Key;
    Query.Params.ParamByName('value').AsString := Value;
    Query.ExecSQL;
  end;
begin
  TProjectDatabaseBootstrap.CreateProjectDatabase(FileName, etNT);
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  Query := TSQLQuery.Create(nil);
  try
    Connection.Transaction := Transaction;
    Connection.DatabaseName := FileName;
    Connection.Open;
    Transaction.StartTransaction;
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Info('descricao', 'iBiblia — projeto de demonstração visual');
    Info('status.catalog', SaveStatusDefinitions(Definitions));
    Info('fonte0.nome', 'Times New Roman');
    Info('fonte0.tamanho', '24');
    Info('fonte1.nome', 'Georgia');
    Info('fonte1.tamanho', '22');
    Info('fonte2.nome', 'Times New Roman');
    Info('fonte2.tamanho', '24');
    Info('direction.text2', 'rtl');
    for I := 1 to 12 do
    begin
      Query.SQL.Text := 'UPDATE pares SET pare_texto_origem=:src,' +
        'pare_texto_destino=:dst,pare_texto_consulta1=:ref,' +
        'pare_comentarios=:notes,pare_situacao=:status WHERE pare_id=:id';
      Query.Params.ParamByName('src').AsString := 'Ἐν ἀρχῇ ἦν ὁ λόγος, καὶ ὁ λόγος ἦν πρὸς τὸν θεόν.';
      Query.Params.ParamByName('dst').AsString := 'No princípio era a Palavra, e a Palavra estava com Deus.';
      Query.Params.ParamByName('ref').AsString := 'בְּרֵאשִׁית בָּרָא אֱלֹהִים אֵת הַשָּׁמַיִם וְאֵת הָאָרֶץ';
      Query.Params.ParamByName('notes').AsString := 'Projeto de teste: notas e textos não serão publicados.';
      Query.Params.ParamByName('status').AsInteger := I - 1;
      Query.Params.ParamByName('id').AsString := Format('40,1,%d', [I]);
      Query.ExecSQL;
    end;
    Transaction.Commit;
  finally
    Query.Free;
    Transaction.Free;
    Connection.Free;
  end;
end;

procedure InspectLayout(Control: TWinControl; const Surface: string);
var
  I, TextWidth: Integer;
  Child: TControl;
  LabelControl: TLabel;
  TextValue: string;
begin
  for I := 0 to Control.ControlCount - 1 do
  begin
    Child := Control.Controls[I];
    if not Child.Visible then
      Continue;
    if (Child is TTabSheet) and (Control is TPageControl) and
      (TPageControl(Control).ActivePage <> Child) then
      Continue;
    if (Child is TCustomLabel) or (Child is TCustomButton) or
       (Child is TCustomComboBox) then
    begin
      if not (Control is TScrollBox) and
        ((Child.Left < -1) or (Child.Top < -1) or
        (Child.Left + Child.Width > Control.ClientWidth + 1) or
        (Child.Top + Child.Height > Control.ClientHeight + 1)) then
        Log('REVIEW_BOUNDS'#9 + Surface + #9 + Child.Name + #9 + Child.ClassName +
          #9 + Format('%d,%d %dx%d / %dx%d', [Child.Left, Child.Top,
            Child.Width, Child.Height, Control.ClientWidth, Control.ClientHeight]));
    end;
    if Child is TLabel then
    begin
      LabelControl := TLabel(Child);
      if Pos('project-export', Surface) = 1 then
        Log('LABEL'#9 + Surface + #9 + Child.Name + #9 + LabelControl.Caption);
      if not LabelControl.WordWrap and not LabelControl.AutoSize and
        (LabelControl.Caption <> '') then
      begin
        TextValue := StringReplace(LabelControl.Caption, '&', '', [rfReplaceAll]);
        LabelControl.Canvas.Font.Assign(LabelControl.Font);
        TextWidth := LabelControl.Canvas.TextWidth(TextValue);
        if TextWidth > LabelControl.Width + 1 then
          Log('REVIEW_TEXT'#9 + Surface + #9 + Child.Name + #9 +
            Format('%d required / %d available', [TextWidth, Child.Width]));
      end;
    end;
    if Child is TWinControl then
      InspectLayout(TWinControl(Child), Surface);
  end;
end;

procedure Capture(Form: TForm; const Name: string);
var
  Bitmap: TBitmap;
  PNG: TPortableNetworkGraphic;
begin
  Form.Realign;
  Application.ProcessMessages;
  Form.Update;
  Bitmap := Form.GetFormImage;
  PNG := TPortableNetworkGraphic.Create;
  try
    PNG.Assign(Bitmap);
    PNG.SaveToFile(OutputDir + Name + '.png');
    Log('CAPTURE'#9 + Name + #9 + Format('%dx%d @ %dPPI',
      [Bitmap.Width, Bitmap.Height, Form.PixelsPerInch]));
    InspectLayout(Form, Name);
  finally
    PNG.Free;
    Bitmap.Free;
  end;
end;

procedure CapturePages(Form: TForm; const Name: string);
var
  I, J, OriginalPage: Integer;
  Pages: TPageControl;
begin
  Log('START'#9 + Name);
  Form.Position := poDesigned;
  Form.Left := -20000;
  Form.Top := -20000;
  Form.Show;
  Application.ProcessMessages;
  if Form.PixelsPerInch <> TargetDPI then
    Form.AutoAdjustLayout(lapAutoAdjustForDPI, Form.PixelsPerInch, TargetDPI,
      Form.Width, Form.Width);
  Capture(Form, Name);
  for I := 0 to Form.ComponentCount - 1 do
    if Form.Components[I] is TPageControl then
    begin
      Pages := TPageControl(Form.Components[I]);
      OriginalPage := Pages.PageIndex;
      for J := 0 to Pages.PageCount - 1 do
      begin
        Pages.PageIndex := J;
        Capture(Form, Name + '-page-' + IntToStr(I) + '-' + IntToStr(J));
      end;
      Pages.PageIndex := OriginalPage;
    end;
end;

procedure TModalProbe.Capture(Data: PtrInt);
var
  ActiveForm: TForm;
begin
  if not (Screen.ActiveCustomForm is TForm) then
    raise Exception.Create('Expected an active modal form');
  ActiveForm := TForm(Screen.ActiveCustomForm);
  CapturePages(ActiveForm, Name);
  ActiveForm.ModalResult := mrCancel;
end;

procedure TIconFamilyModalProbe.SelectAndCapture(Data: PtrInt);
var
  ActiveForm: TForm;
  Combo: TComboBox;
  I, SelectedIndex: Integer;
  Ini: TIniFile;
begin
  Require(Screen.ActiveCustomForm is TFrmApplicationPreferences,
    'Preferences opens through the main window action');
  ActiveForm := TForm(Screen.ActiveCustomForm);
  Require(ActiveForm.FindComponent('IconFamilyComboBox') is TComboBox,
    'Icon family selector is available');
  Combo := TComboBox(ActiveForm.FindComponent('IconFamilyComboBox'));
  Require(Combo.Items.Count = 4, 'Preferences offers all four icon families');
  SelectedIndex := -1;
  for I := 0 to Combo.Items.Count - 1 do
    if PtrInt(Combo.Items.Objects[I]) = Ord(Family) then
    begin
      SelectedIndex := I;
      Break;
    end;
  Require(SelectedIndex >= 0, 'Requested family is present in the selector');
  Require(Assigned(Combo.OnChange), 'Icon family selector has its change handler');
  Combo.ItemIndex := SelectedIndex;
  Combo.OnChange(Combo);
  Require(GetAppIconFamily = Family,
    'Preferences callback applies icon family: ' + AppIconFamilyId(Family));

  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'iBiblia.ini');
  try
    Require(Ini.ReadString('opcoes', 'ui.icon.family', '') = AppIconFamilyId(Family),
      'Preferences persists icon family: ' + AppIconFamilyId(Family));
    Require(Ini.ReadString('opcoes', 'language', '') = ProbeLanguage,
      'Icon selection preserves the interface language');
    Require(not Ini.ReadBool('opcoes', 'auto.save', True),
      'Icon selection preserves disabled automatic saving');
    Require(not Ini.ReadBool('opcoes', 'synctheword', True) and
      not Ini.ReadBool('opcoes', 'syncibiblia', True),
      'Icon selection preserves disabled theWord synchronization');
  finally
    Ini.Free;
  end;
  inherited Capture(Data);
end;

procedure CaptureAndFree(Form: TForm; const Name: string);
begin
  try
    CapturePages(Form, Name);
  finally
    Form.Free;
  end;
end;

procedure TStatusDrawTrace.Draw(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  List: TListBox;
  StatusId: Integer;
  Color: TColor;
begin
  List := TListBox(Control);
  if Count < 30 then
  begin
    StatusId := -1;
    if (Index >= 0) and (Index < List.Items.Count) then
      StatusId := PtrUInt(List.Items.Objects[Index]);
    Color := GetStatusColor(CurrentProject.GetStatusDefinitions, StatusId);
    Log('DRAW_BEFORE'#9 + Format('%d id%d rect%d,%d,%d,%d list%dx%d itemH%d ppi%d scale14=%d color=%s expected=%s brush=%s',
      [Index, StatusId, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,
       List.Width, List.Height, List.ItemHeight, List.Font.PixelsPerInch,
       List.Scale96ToForm(14), IntToHex(ColorToRGB(List.Color), 6),
       IntToHex(ColorToRGB(Color), 6), IntToHex(ColorToRGB(List.Canvas.Brush.Color), 6)]));
  end;
  if Assigned(Original) then
    Original(Control, Index, ARect, State);
  if Count < 30 then
    Log('DRAW_AFTER'#9 + Format('%d brush=%s penWidth%d', [Index,
      IntToHex(ColorToRGB(List.Canvas.Brush.Color), 6), List.Canvas.Pen.Width]));
  Inc(Count);
end;
procedure LogStatusReview(Control: TWinControl);
var
  I: Integer;
  List: TListBox;
begin
  if Control is TCustomListBox then
  begin
    List := TListBox(Control);
    if (List.Items.Count = 12) and not Assigned(StatusDrawTrace) then
    begin
      StatusDrawTrace := TStatusDrawTrace.Create;
      StatusDrawTrace.Original := List.OnDrawItem;
      List.OnDrawItem := @StatusDrawTrace.Draw;
    end;
    for I := 0 to List.Items.Count - 1 do
      Log('STATUS_REVIEW_ITEM'#9 + IntToStr(I) + #9 +
        IntToStr(PtrUInt(List.Items.Objects[I])) + #9 + List.Items[I]);
  end;
  for I := 0 to Control.ControlCount - 1 do
    if Control.Controls[I] is TWinControl then
      LogStatusReview(TWinControl(Control.Controls[I]));
end;
procedure CheckStatusReviewKeyboard;
var
  List: TListBox;
  SelectedId, OldTop, BeforeCount, FilteredCount, Iteration: Integer;
  Key: Word;

  function FindList(Control: TWinControl): TListBox;
  var
    I: Integer;
  begin
    Result := nil;
    if (Control is TListBox) and (TListBox(Control).Items.Count = 12) then
      Exit(TListBox(Control));
    for I := 0 to Control.ControlCount - 1 do
      if Control.Controls[I] is TWinControl then
      begin
        Result := FindList(TWinControl(Control.Controls[I]));
        if Assigned(Result) then
          Exit;
      end;
  end;

  function VisibleVerseCount: Integer;
  var
    I: Integer;
    Node: TTreeNode;
  begin
    Result := 0;
    for I := 0 to MainForm.TreeView1.Items.Count - 1 do
    begin
      Node := MainForm.TreeView1.Items[I];
      if not Node.HasChildren and Node.Visible then
        Inc(Result);
    end;
  end;

  procedure CheckSelection(const Stage: string);
  begin
    Require(List.ItemIndex >= 0, Stage + ': selected item survives reconstruction');
    Require(Integer(PtrUInt(List.Items.Objects[List.ItemIndex])) = SelectedId,
      Stage + ': selection follows the stable status ID');
    Require(List.TopIndex = OldTop, Stage + ': scroll position is preserved');
  end;

begin
  List := FindList(MainForm);
  Require(Assigned(List), 'Status review list exists for keyboard regression');
  Require(Assigned(List.OnClick) and Assigned(List.OnKeyDown),
    'Status review mouse and keyboard handlers are connected');
  BeforeCount := VisibleVerseCount;
  // Deliberately move the display item before the catalog rebuild. Retaining
  // the old numeric index would select another status after the click.
  List.Items.Exchange(5, 7);
  List.ItemIndex := 5;
  List.TopIndex := 4;
  OldTop := List.TopIndex;
  SelectedId := Integer(PtrUInt(List.Items.Objects[List.ItemIndex]));
  List.OnClick(List);
  CheckSelection('Mouse toggle');
  Require(List.ItemIndex = 7, 'Rebuilt selection follows the reordered status');
  FilteredCount := VisibleVerseCount;
  Require(FilteredCount = BeforeCount - 1, 'Mouse toggle hides its one synthetic verse');
  for Iteration := 1 to 3 do
  begin
    Key := VK_SPACE;
    List.OnKeyDown(List, Key, []);
    Require(Key = 0, 'Space is consumed by the status filter');
    CheckSelection('Space toggle ' + IntToStr(Iteration));
    if Odd(Iteration) then
      Require(VisibleVerseCount = BeforeCount, 'Space restores the filtered verse')
    else
      Require(VisibleVerseCount = FilteredCount, 'Repeated Space filters the same status');
  end;
  List.ItemIndex := -1;
  List.TopIndex := 0;
end;
procedure CheckPersistentDialogLanguageSwitch;
var
  PropertiesDialog: TFrmProjectProperties;
  ExportDialog: TProjectExportForm;
  PatchDialog: TFrmApplyPatch;
  InterlinearDialog: TInterlinearVerseRulesForm;
  VerseRulesDialog: TVerseRulesForm;
  TestLanguage, ExpectedCancel, ExpectedProperties, ExpectedExport: string;
  Pass: Integer;
begin
  PropertiesDialog := TFrmProjectProperties.Create(nil);
  ExportDialog := TProjectExportForm.Create(nil);
  PatchDialog := TFrmApplyPatch.Create(nil);
  InterlinearDialog := TInterlinearVerseRulesForm.Create(nil);
  VerseRulesDialog := TVerseRulesForm.Create(nil);
  try
    Require(PatchDialog.LoadPatch(OutputDir + 'visual-fixture.ibpatch'),
      'Language regression opens its synthetic patch');
    for Pass := 0 to 2 do
    begin
      if Pass = 1 then
      begin
        TestLanguage := 'en';
        ExpectedCancel := 'Cancel';
        ExpectedProperties := 'Project settings';
        ExpectedExport := 'Export project';
      end
      else
      begin
        TestLanguage := 'pt';
        ExpectedCancel := 'Cancelar';
        ExpectedProperties := 'Configurações do projeto';
        ExpectedExport := 'Exportar projeto';
      end;
      SetDefaultLang(TestLanguage, ExtractFilePath(ParamStr(0)) + 'languages', 'ui_visual_probe');
      PropertiesDialog.Translate;
      PropertiesDialog.SetProject(CurrentProject);
      ExportDialog.SetProject(CurrentProject);
      PatchDialog.FormShow(PatchDialog);
      InterlinearDialog.Project := CurrentProject;
      VerseRulesDialog.FormShow(VerseRulesDialog);
      Require(PropertiesDialog.Caption = ExpectedProperties,
        'Persistent project settings title updates to ' + TestLanguage);
      Require(PropertiesDialog.BtnCancel.Caption = ExpectedCancel,
        'Persistent project settings Cancel updates to ' + TestLanguage);
      Require(ExportDialog.Caption = ExpectedExport,
        'Persistent export title updates to ' + TestLanguage);
      Require(ExportDialog.BtnCancel.Caption = ExpectedCancel,
        'Persistent export Cancel updates to ' + TestLanguage);
      Require(PatchDialog.Button2.Caption = ExpectedCancel,
        'Persistent patch Cancel updates to ' + TestLanguage);
      Require(InterlinearDialog.CancelButton.Caption = ExpectedCancel,
        'Persistent interlinear rules Cancel updates to ' + TestLanguage);
      Require(VerseRulesDialog.CancelButton.Caption = '&' + ExpectedCancel,
        'Persistent verse rules Cancel updates with its shortcut to ' + TestLanguage);
    end;
  finally
    VerseRulesDialog.Free;
    InterlinearDialog.Free;
    PatchDialog.Free;
    ExportDialog.Free;
    PropertiesDialog.Free;
    SetDefaultLang(ProbeLanguage, ExtractFilePath(ParamStr(0)) + 'languages', 'ui_visual_probe');
  end;
end;
procedure PopulateAITree(Control: TWinControl);
var
  I: Integer;
  Refs: TStringList;
  Node: TTreeNode;
  Tree: TBibleTreeView;
begin
  if Control is TBibleTreeView then
  begin
    Tree := TBibleTreeView(Control);
    Refs := TStringList.Create;
    try
      for I := 1 to 7 do
        Refs.Add(Format('40,1,%d', [I]));
      Tree.VerseList := Refs;
    finally
      Refs.Free;
    end;
    I := 0;
    Node := Tree.Items.GetFirstNode;
    while Assigned(Node) do
    begin
      if Node.Level = 2 then
      begin
        Node.StateIndex := I;
        Inc(I);
      end;
      Node := Node.GetNext;
    end;
    Tree.FullExpand;
  end;
  for I := 0 to Control.ControlCount - 1 do
    if Control.Controls[I] is TWinControl then
      PopulateAITree(TWinControl(Control.Controls[I]));
end;

procedure CaptureIconFamilies;
var
  PropertiesForm: TFrmProjectProperties;
  AIForm: TFrmAIProcessing;
  AboutForm: TFormAbout;
  ModalProbe: TIconFamilyModalProbe;
  OriginalFamily, Family: TAppIconFamily;
  TextPage: TWinControl;
  PreferencesAction: TCustomAction;
  Pass, ActionIndex: Integer;
  Prefix: string;

  procedure PrepareForm(Form: TForm);
  begin
    Form.Position := poDesigned;
    Form.Left := -20000;
    Form.Top := -20000;
    Form.Show;
    Application.ProcessMessages;
    if Form.PixelsPerInch <> TargetDPI then
      Form.AutoAdjustLayout(lapAutoAdjustForDPI, Form.PixelsPerInch,
        TargetDPI, Form.Width, Form.Width);
  end;

begin
  OriginalFamily := GetAppIconFamily;
  PropertiesForm := nil;
  AIForm := nil;
  AboutForm := nil;
  ModalProbe := nil;
  try
    PropertiesForm := TFrmProjectProperties.Create(nil);
    PropertiesForm.SetProject(CurrentProject);
    TextPage := PropertiesForm.TabControlTextType.Parent;
    while Assigned(TextPage) and not (TextPage is TTabSheet) do
      TextPage := TextPage.Parent;
    Require(TextPage is TTabSheet, 'Project text settings page is available');
    TTabSheet(TextPage).PageControl.ActivePage := TTabSheet(TextPage);

    AIForm := TFrmAIProcessing.Create(nil);
    PopulateAITree(AIForm);
    AboutForm := TFormAbout.Create(nil);
    ModalProbe := TIconFamilyModalProbe.Create;
    PrepareForm(MainForm);
    PrepareForm(PropertiesForm);
    PrepareForm(AIForm);
    PrepareForm(AboutForm);
    PreferencesAction := nil;
    for ActionIndex := 0 to MainForm.ActionList1.ComponentCount - 1 do
      if (MainForm.ActionList1.Components[ActionIndex] is TCustomAction) and
        (TCustomAction(MainForm.ActionList1.Components[ActionIndex]).ImageIndex =
          AppIconIndex(aiPreferences)) then
      begin
        PreferencesAction := TCustomAction(MainForm.ActionList1.Components[ActionIndex]);
        Break;
      end;
    Require(Assigned(PreferencesAction), 'Main window preferences action is available');

    for Pass := Ord(Low(TAppIconFamily)) to Ord(High(TAppIconFamily)) + 1 do
    begin
      if Pass > Ord(High(TAppIconFamily)) then
      begin
        Family := OriginalFamily;
        Prefix := 'icons-restored-' + AppIconFamilyId(Family);
      end
      else
      begin
        Family := TAppIconFamily(Pass);
        Prefix := 'icons-' + AppIconFamilyId(Family);
      end;
      Log('START' + #9 + Prefix);
      ModalProbe.Family := Family;
      ModalProbe.Name := Prefix + '-preferences';
      Application.QueueAsyncCall(@ModalProbe.SelectAndCapture, 0);
      PreferencesAction.Execute;
      Application.ProcessMessages;
      Require(GetAppIconFamily = Family, 'Active icon family: ' + AppIconFamilyId(Family));
      Require(MainForm.RadioGroupStatus.Items.Count = 12,
        'Custom status catalog survives icon switch: ' + AppIconFamilyId(Family));
      Capture(MainForm, Prefix + '-main');
      Capture(PropertiesForm, Prefix + '-project-settings-texts');
      Capture(AIForm, Prefix + '-ai-processing');
      Capture(AboutForm, Prefix + '-about');

    end;
  finally
    SetAppIconFamily(OriginalFamily);
    ModalProbe.Free;
    AboutForm.Free;
    AIForm.Free;
    PropertiesForm.Free;
  end;
end;

procedure CloseProbe;
begin
  FormPleaseWait.Free;
  FormPleaseWait := nil;
  FrmDictionaryPopup.Free;
  FrmDictionaryPopup := nil;
  CurrentProject.SaveProject;
  MainForm.ActionCloseProjectExecute(nil);
  MainForm.Free;
  MainForm := nil;
  FreeAndNil(Report);
end;

procedure Run;
var
  FixturePath, PatchPath: string;
  MenuItem: TMenuItem;
  Dialog: TForm;
  LLM: TFrmLLMOptions;
  AI: TFrmAIProcessing;
  ExportForm: TProjectExportForm;
  PropertiesForm: TFrmProjectProperties;
  NewForm: TFrmNewProject;
  Rules: TInterlinearVerseRulesForm;
  ApplyPatchForm: TFrmApplyPatch;
  ExportPatchForm: TFrmExportPatch;
  Patch: TPatchFile;
  ModalProbe: TModalProbe;
  Remaps: TStatusRemaps;
  Examples: TAIFlowExampleArray;
  Preferences: TApplicationPreferences;
  Ini: TIniFile;
  EditedNoteBody: UnicodeString;
  MappingDefinitions: TStatusDefinitions;
  MappingIndex: Integer;
  NotePreview: TTranslatorNotePreviewWindow;
begin
  ProbeLanguage := Option('--language', 'pt');
  TargetDPI := StrToInt(Option('--dpi', '96'));
  OutputDir := IncludeTrailingPathDelimiter(ExpandFileName(Option('--output',
    ExtractFilePath(ParamStr(0)) + 'captures-' + ProbeLanguage + '-' + IntToStr(TargetDPI))));
  ForceDirectories(OutputDir);
  Report := TStringList.Create;
  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'iBiblia.ini');
  try
    Ini.WriteString('opcoes', 'language', ProbeLanguage);
    Ini.WriteString('opcoes', 'ui.icon.family',
      Option('--initial-icon-family', 'connected'));
    Ini.WriteBool('opcoes', 'synctheword', False);
    Ini.WriteBool('opcoes', 'syncibiblia', False);
    Ini.WriteBool('opcoes', 'auto.save', False);
    Ini.WriteBool('opcoes', 'sugestoes.automaticas', False);
    Ini.WriteBool('leiaute', 'principal.maximized', False);
    Ini.WriteInteger('leiaute', 'principal.largura', 1280);
    Ini.WriteInteger('leiaute', 'principal.altura', 900);
    Ini.EraseSection('recentes');
  finally
    Ini.Free;
  end;
  Application.Initialize;
  ExceptionHandler := TProbeExceptionHandler.Create;
  Application.OnException := @ExceptionHandler.Handle;
  Application.ShowMainForm := False;
  SetDefaultLang(ProbeLanguage, ExtractFilePath(ParamStr(0)) + 'languages', 'ui_visual_probe');
  BuildCatalog;
  CheckStatusImages;
  FixturePath := OutputDir + 'visual-fixture.bib';
  CreateFixture(FixturePath);
  Application.CreateForm(TMainForm, MainForm);
  Require(GetAppIconFamily = AppIconFamilyFromId(
    Option('--initial-icon-family', 'connected')),
    'Main window restores the saved icon family at startup: ' +
      AppIconFamilyId(GetAppIconFamily));
  Application.CreateForm(TFrmDictionaryPopup, FrmDictionaryPopup);
  Application.CreateForm(TFormPleaseWait, FormPleaseWait);
  MainForm.OnShow := nil; // Never reopen a persisted recent project in this probe.
  MainForm.Position := poDesigned;
  MainForm.Left := -20000;
  MainForm.Top := -20000;
  MainForm.Show;
  MenuItem := TMenuItem.Create(nil);
  try
    MenuItem.Caption := FixturePath;
    MainForm.RecentProjectClick(MenuItem);
  finally
    MenuItem.Free;
  end;
  Require(Assigned(CurrentProject), 'Synthetic project opened');
  CurrentProject.GoToReference('40,1,1');
  MainForm.TreeView1.Items.GetFirstNode.Expand(False);
  MainForm.TreeView1.Items.GetFirstNode.GetFirstChild.Expand(False);
  if Option('--icon-families', '0') = '1' then
  begin
    CaptureIconFamilies;
    Log('COMPLETE'#9 + ProbeLanguage + #9 + IntToStr(TargetDPI) + #9 + 'icon-families');
    CloseProbe;
    Exit;
  end;
  CapturePages(MainForm, 'main');
  LogStatusReview(MainForm);
  Require(MainForm.RadioGroupStatus.Items.Count = 12, 'All twelve custom statuses appear in selector');
  Require(CurrentProject.VerseObjects[tbOrigem].Font.Name = 'Times New Roman', 'Greek source font preserved');
  Require(CurrentProject.VerseObjects[tbDestino].Font.Name = 'Georgia', 'Destination font preserved');
  Require(CurrentProject.VerseObjects[tbDestino].Font.Size = 22, 'Destination font point size preserved');
  MainForm.ClientWidth := Round(1024 * TargetDPI / 96);
  Capture(MainForm, 'main-narrow');
  CheckStatusReviewKeyboard;
  ModalProbe := TModalProbe.Create;
  try
    ModalProbe.Name := 'preferences';
    Preferences.Language := ProbeLanguage;
    Preferences.PopupTrigger := ptMouseHover;
    Preferences.AutoSave := False;
    Preferences.IconFamily := GetAppIconFamily;
    Application.QueueAsyncCall(@ModalProbe.Capture, 0);
    TFrmApplicationPreferences.Execute(MainForm, Preferences, nil);
  finally
    ModalProbe.Free;
  end;


  NewForm := TFrmNewProject.Create(nil);
  NewForm.ResetDraft('Novo projeto de associação');
  CaptureAndFree(NewForm, 'new-project');
  PropertiesForm := TFrmProjectProperties.Create(nil);
  PropertiesForm.SetProject(CurrentProject);
  CaptureAndFree(PropertiesForm, 'project-settings');
  ExportForm := TProjectExportForm.Create(nil);
  ExportForm.SetProject(CurrentProject);
  CaptureAndFree(ExportForm, 'project-export');
  LLM := TFrmLLMOptions.Create(nil);
  LLM.SetProject(CurrentProject);
  CaptureAndFree(LLM, 'llm-options');
  AI := TFrmAIProcessing.Create(nil);
  PopulateAITree(AI);
  CaptureAndFree(AI, 'ai-processing');
  Dialog := TFrmMergeProjects.Create(nil);
  TFrmMergeProjects(Dialog).InitializeForm;
  CaptureAndFree(Dialog, 'merge-projects');
  ExportPatchForm := TFrmExportPatch.Create(nil);
  ExportPatchForm.Scope := etNT;
  CaptureAndFree(ExportPatchForm, 'export-patch');
  Patch := TPatchFile.Create;
  PatchPath := OutputDir + 'visual-fixture.ibpatch';
  try
    Patch.Add('40,1,1', CurrentProject.VerseObjects[tbOrigem].Text,
      'No princípio era a Palavra.', '', 'Proposta de nota para revisão.', 2);
    Patch.StatusCatalog := SaveStatusDefinitions(Definitions);
    Patch.Save(PatchPath);
  finally
    Patch.Free;
  end;
  ApplyPatchForm := TFrmApplyPatch.Create(nil);
  Require(ApplyPatchForm.LoadPatch(PatchPath), 'Synthetic patch is reviewable');
  CaptureAndFree(ApplyPatchForm, 'apply-patch');
  Rules := TInterlinearVerseRulesForm.Create(nil);
  Rules.Project := CurrentProject;
  CaptureAndFree(Rules, 'interlinear-rules');
  Dialog := TVerseRulesForm.Create(nil);
  TVerseRulesForm(Dialog).AddVerseRule('(<FI>)(.*?)(<Fi>)', '$2');
  CaptureAndFree(Dialog, 'verse-rules');
  CaptureAndFree(TFrmTheWordHighlighterColors.Create(nil), 'syntax-colors');
  CaptureAndFree(TFormAbout.Create(nil), 'about');

  ModalProbe := TModalProbe.Create;
  try

    ModalProbe.Name := 'status-catalog';
    Application.QueueAsyncCall(@ModalProbe.Capture, 0);
    TFrmStatusCatalog.Execute(Definitions, Counts, Remaps);
    ModalProbe.Name := 'patch-status-mapping';
    MappingDefinitions := Copy(Definitions, 0, Length(Definitions));
    Patch := TPatchFile.Create;
    try
      for MappingIndex := 0 to High(MappingDefinitions) do
      begin
        MappingDefinitions[MappingIndex].Caption := Format('Estado do patch %d', [MappingIndex + 1]);
        Patch.Add(Format('40,1,%d', [MappingIndex + 1]), '', '', '', '', MappingIndex);
      end;
      Patch.StatusCatalog := SaveStatusDefinitions(MappingDefinitions);
      Application.QueueAsyncCall(@ModalProbe.Capture, 0);
      Require(CurrentProject.ApplyProjectPatch(Patch, nil) = 0,
        'Cancelling status mapping preserves the synthetic project');
    finally
      Patch.Free;
    end;
    ModalProbe.Name := 'flow-examples';
    Examples := nil;
    Application.QueueAsyncCall(@ModalProbe.Capture, 0);
    TFrmFlowExamples.Execute(MainForm, CurrentProject, 'association', Examples);
    ModalProbe.Name := 'translator-note-editor';
    Application.QueueAsyncCall(@ModalProbe.Capture, 0);
    ExecuteTranslatorNoteEditor(MainForm, Rect(100, 100, 120, 120), True,
      twlcFullWysiwyg, '*', 'Nota de estudo com <b>ênfase</b> e texto grego λόγος.', nil);
    ModalProbe.Name := 'note-body-editor';
    Application.QueueAsyncCall(@ModalProbe.Capture, 0);
    ExecuteTheWordBodyEditor(MainForm, Rect(100, 100, 120, 120), 'Comentários',
      'Comentário de demonstração com <i>formatação preservada</i>.', EditedNoteBody);
  finally
    ModalProbe.Free;
  end;

  NotePreview := TTranslatorNotePreviewWindow.Create(nil);
  NotePreview.ShowNote('Nota de estudo com <b>ênfase</b>.', Rect(-20000, -20000, -19980, -19980));
  CaptureAndFree(NotePreview, 'translator-note-preview');
  FormPleaseWait.SetMessage('Preparando projeto para revisão');
  FormPleaseWait.SetDetail('Organizando os versículos e as associações.');
  CapturePages(FormPleaseWait, 'please-wait');
  FormPleaseWait.Hide;
  FrmDictionaryPopup.AddStrong('G3056', '{\rtf1\ansi Palavra, discurso, mensagem.}');
  CapturePages(FrmDictionaryPopup, 'dictionary');
  FrmDictionaryPopup.Hide;
  CheckPersistentDialogLanguageSwitch;
  Log('COMPLETE'#9 + ProbeLanguage + #9 + IntToStr(TargetDPI));
  CloseProbe;
end;

begin
  try
    Run;
  except
    on E: Exception do
    begin
      WriteLn(StdErr, E.ClassName + ': ' + E.Message);
      DumpExceptionBackTrace(StdErr);
      Flush(StdErr);
      if Assigned(Report) then
        Log('FAIL'#9 + E.ClassName + #9 + E.Message);
      Halt(1);
    end;
  end;
end.
