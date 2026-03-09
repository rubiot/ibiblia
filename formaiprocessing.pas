unit formaiprocessing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, BibleTreeView, Versiculo, CommentsView,
  ai_processing_manager, ai_flows, ai_translation_flow, ai_association_flow,
  PatchFile;

type
  TAIProcessingStatus = (apsIdle, apsRunning, apsCancelling, apsCompleted, apsCancelled);

  { TAIProcessingBatch }
  
  TAIProcessingBatch = class
  private
    FReference: TStringList;
    FSrcText: TStringList;
    FDstText: TStringList;
    FPairs: TStringList;
    FComments: TStringList;
    FStatus: TStringList;
    FErrors: TStringList;
    FProcessingState: TStringList; // 'pending', 'processing', 'success', 'error'
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(ref, src, dst, pairs, comments: string; status: integer);
    procedure SetState(index: integer; state: string);
    procedure SetError(index: integer; error: string);
    function Count: integer;
    
    property Reference: TStringList read FReference;
    property SourceText: TStringList read FSrcText;
    property DestinationText: TStringList read FDstText;
    property Pairs: TStringList read FPairs;
    property Comments: TStringList read FComments;
    property Status: TStringList read FStatus;
    property ProcessingState: TStringList read FProcessingState;
    property Errors: TStringList read FErrors;
  end;

  { TFrmAIProcessing }

  TFrmAIProcessing = class(TForm)
    BtnCancel: TButton;
    BtnApply: TButton;
    BtnClose: TButton;
    BtnDeselectAll: TButton;
    BtnSelectAll: TButton;
    GroupBoxVerses: TGroupBox;
    LabelProgress: TLabel;
    LabelETA: TLabel;
    LabelSrcText: TLabel;
    LabelDstText: TLabel;
    LabelAssociations: TLabel;
    LabelComments: TLabel;
    LabelStatus: TLabel;
    MemoComments: TMemo;
    PanelTop: TPanel;
    PanelBottom: TPanel;
    ProgressBar1: TProgressBar;
    ScrollBoxSrcText: TScrollBox;
    ScrollBoxDstText: TScrollBox;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    TabControlPreview: TTabControl;
    TimerUpdate: TTimer;
    procedure BtnApplyClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnDeselectAllClick(Sender: TObject);
    procedure BtnSelectAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TabControlPreviewChange(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
  private
    FBibleTreeView: TBibleTreeView;
    FBatch: TAIProcessingBatch;
    FManager: TAIProcessingManager;
    FFlow: IAIVerseFlow;
    FProject: TObject; // TProjeto - stored as TObject to avoid circular reference
    FIndex: integer;
    FSrcVerse: TVersiculo;
    FDstVerse: TVersiculo;
    FActualCommentsMemo: TCommentsView;
    FStatus: TAIProcessingStatus;
    FOriginalData: TAIProcessingBatch; // Store original project data for comparison
    
    procedure OnVerseChange(index: integer);
      procedure EnsureVersesCreated;
    procedure LoadControlsFromBatch;
    procedure LoadControlsFromOriginal;
    procedure UpdateProgress;
    procedure UpdateTreeNodeStates;
    procedure PumpProcessing;
    procedure OnJobStateChanged(Sender: TObject; JobIndex: Integer; NewState: TAIJobState; ErrorMsg: string);
    procedure OnJobCompletedEvent(Sender: TObject; JobIndex: Integer; Result: TAIFlowResult);
  public
    constructor Create(AOwner: TComponent); override;
    function StartProcessing(Manager: TAIProcessingManager; Flow: IAIVerseFlow; 
                             Verses: TList; Project: TObject): boolean;
    procedure Apply;
    property Batch: TAIProcessingBatch read FBatch;
  end;

var
  FrmAIProcessing: TFrmAIProcessing;

resourcestring
  SAIProcessingTitle = 'AI Processing';
  SAIProcessingInProgress = 'Processing: %d of %d verses (%d%%)';
  SAIProcessingCompleted = 'Processing completed: %d successful, %d failed';
  SAIProcessingCancelling = 'Cancelling...';
  SAIProcessingCancelled = 'Processing cancelled';
  SEstimatedTimeRemaining = 'Estimated time remaining: %s';
  SApplySuccessful = 'Changes applied successfully';
  SCancelConfirmation = 'Are you sure you want to cancel the AI processing?';
  SConfirmation = 'Confirmation';
  SDiscardConfirmation = 'Are you sure you want to discard the AI results?';

implementation

{$R *.lfm}

uses Projeto, ai_debug;

{ TAIProcessingBatch }

constructor TAIProcessingBatch.Create;
begin
  FReference := TStringList.Create;
  FSrcText := TStringList.Create;
  FDstText := TStringList.Create;
  FPairs := TStringList.Create;
  FComments := TStringList.Create;
  FStatus := TStringList.Create;
  FErrors := TStringList.Create;
  FProcessingState := TStringList.Create;
end;

destructor TAIProcessingBatch.Destroy;
begin
  FReference.Free;
  FSrcText.Free;
  FDstText.Free;
  FPairs.Free;
  FComments.Free;
  FStatus.Free;
  FErrors.Free;
  FProcessingState.Free;
  inherited Destroy;
end;

procedure TAIProcessingBatch.Add(ref, src, dst, pairs, comments: string; status: integer);
begin
  FReference.Add(ref);
  FSrcText.Add(src);
  FDstText.Add(dst);
  FPairs.Add(pairs);
  FComments.Add(comments);
  FStatus.Add(IntToStr(status));
  FErrors.Add('');
  FProcessingState.Add('pending');
end;

procedure TAIProcessingBatch.SetState(index: integer; state: string);
begin
  if (index >= 0) and (index < FProcessingState.Count) then
    FProcessingState[index] := state;
end;

procedure TAIProcessingBatch.SetError(index: integer; error: string);
begin
  if (index >= 0) and (index < FErrors.Count) then
    FErrors[index] := error;
end;

function TAIProcessingBatch.Count: integer;
begin
  Result := FReference.Count;
end;

{ TFrmAIProcessing }

constructor TFrmAIProcessing.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  // Initialize batch objects immediately
  FBatch := TAIProcessingBatch.Create;
  FOriginalData := TAIProcessingBatch.Create;
  
  FStatus := apsIdle;
  FIndex := 0;
  FProject := nil;
  
  // Note: child visual controls are created in FormCreate after .lfm is loaded
end;

procedure TFrmAIProcessing.FormCreate(Sender: TObject);
begin
  // Create child controls now that .lfm components exist
  FBibleTreeView := TBibleTreeView.Create(Self);
  FBibleTreeView.Parent := GroupBoxVerses;
  FBibleTreeView.Align := alClient;
  FBibleTreeView.OnVerseChange := @OnVerseChange;

  // Do not create TVersiculo here; create them in FormShow with the project

  FActualCommentsMemo := TCommentsView.Create(Self);
  FActualCommentsMemo.ReadOnly := True;
end;

procedure TFrmAIProcessing.BtnSelectAllClick(Sender: TObject);
begin
  FBibleTreeView.SelectAll;
end;

procedure TFrmAIProcessing.BtnDeselectAllClick(Sender: TObject);
begin
  FBibleTreeView.SelectNone;
end;

procedure TFrmAIProcessing.BtnCancelClick(Sender: TObject);
begin
  if FStatus = apsRunning then
  begin
    if MessageDlg(SConfirmation, SCancelConfirmation, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      FStatus := apsCancelling;
      LabelProgress.Caption := SAIProcessingCancelling;
      // Disable timer to avoid PumpApply running during shutdown
      TimerUpdate.Enabled := False;
      if Assigned(FManager) then
        FManager.CancelAll;
      BtnCancel.Enabled := False;
    end;
  end;
end;

procedure TFrmAIProcessing.BtnApplyClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TFrmAIProcessing.BtnCloseClick(Sender: TObject);
begin
  if FStatus in [apsCompleted, apsCancelled] then
  begin
    if MessageDlg(SConfirmation, SDiscardConfirmation, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;
  end;
  ModalResult := mrCancel;
end;


procedure TFrmAIProcessing.FormDestroy(Sender: TObject);
begin
  // Ensure timer is disabled so PumpProcessing won't run during shutdown
  TimerUpdate.Enabled := False;
  if Assigned(FSrcVerse) then FSrcVerse.Free;
  if Assigned(FDstVerse) then FDstVerse.Free;
  if Assigned(FBibleTreeView) then FBibleTreeView.Free;
  if Assigned(FActualCommentsMemo) then FActualCommentsMemo.Free;
  if Assigned(FBatch) then FBatch.Free;
  if Assigned(FOriginalData) then FOriginalData.Free;
end;

procedure TFrmAIProcessing.FormShow(Sender: TObject);
var
  proj: TProjeto;
begin
  if not Assigned(FProject) then Exit;
  
  // Setup MemoComments replacement (now that form is fully created)
  if Assigned(MemoComments) then
  begin
    FActualCommentsMemo.Parent := MemoComments.Parent;
    FActualCommentsMemo.Align := MemoComments.Align;
    FActualCommentsMemo.TabOrder := MemoComments.TabOrder;
    FActualCommentsMemo.Left := MemoComments.Left;
    FActualCommentsMemo.Top := MemoComments.Top;
    FActualCommentsMemo.Width := MemoComments.Width;
    FActualCommentsMemo.Height := MemoComments.Height;
    MemoComments.Visible := False;
  end;
  
  proj := TProjeto(FProject);
  // Ensure verses are created with the project so their Painel is initialized
  if not Assigned(FSrcVerse) then
    FSrcVerse := TVersiculo.Criar(FProject, ScrollBoxSrcText);
  if not Assigned(FDstVerse) then
    FDstVerse := TVersiculo.Criar(FProject, ScrollBoxDstText);

  FSrcVerse.VersiculoPar := FDstVerse;
  FSrcVerse.ReadOnly := true;
  FDstVerse.ReadOnly := true;

  FSrcVerse.Fonte := proj.ObterFonteTexto(tbOrigem);
  FDstVerse.Fonte := proj.ObterFonteTexto(tbDestino);
  FSrcVerse.PalavrasComStrongEmNegrito := proj.PalavrasComStrongEmNegrito;
  FSrcVerse.StrongsCountMode := proj.MostrarQtdStrongs;
  FDstVerse.StrongsCountMode := proj.MostrarQtdStrongs;
  FSrcVerse.Ativo := true;
  FDstVerse.Ativo := true;
  
  FActualCommentsMemo.FontName := 'default';
  FActualCommentsMemo.FontSize := 10;

  FBibleTreeView.FullExpand;
  if FStatus = apsRunning then
    TimerUpdate.Enabled := True;
end;

procedure TFrmAIProcessing.TabControlPreviewChange(Sender: TObject);
  procedure SetLabelColor(lbl: TLabel; isEqual: boolean);
  begin
    if isEqual then
      lbl.Color := clGreen
    else
      lbl.Color := clRed;
  end;
begin
  if FBatch.Count = 0 then Exit;
  
  case TabControlPreview.TabIndex of
    0: LoadControlsFromBatch;
    1: LoadControlsFromOriginal;
  end;
  
  // Compare batch results with original data
  SetLabelColor(LabelSrcText, FBatch.SourceText[FIndex] = FOriginalData.SourceText[FIndex]);
  SetLabelColor(LabelDstText, FBatch.DestinationText[FIndex] = FOriginalData.DestinationText[FIndex]);
  SetLabelColor(LabelAssociations, FBatch.Pairs[FIndex] = FOriginalData.Pairs[FIndex]);
  SetLabelColor(LabelComments, FBatch.Comments[FIndex] = FOriginalData.Comments[FIndex]);
  SetLabelColor(LabelStatus, FBatch.Status[FIndex] = FOriginalData.Status[FIndex]);
end;

procedure TFrmAIProcessing.TimerUpdateTimer(Sender: TObject);
begin
  PumpProcessing;
end;

procedure TFrmAIProcessing.OnVerseChange(index: integer);
begin
  if not Assigned(TabControlPreview) then Exit;
  if index = FIndex then Exit;
  FIndex := index;
  TabControlPreviewChange(TabControlPreview);
end;

procedure TFrmAIProcessing.EnsureVersesCreated;
begin
  // If verses already created, nothing to do
  if Assigned(FSrcVerse) and Assigned(FDstVerse) then Exit;

  // If we have a project, create verses with project so panels are initialized
  if Assigned(FProject) then
  begin
    if not Assigned(FSrcVerse) then
      FSrcVerse := TVersiculo.Criar(FProject, ScrollBoxSrcText);
    if not Assigned(FDstVerse) then
      FDstVerse := TVersiculo.Criar(FProject, ScrollBoxDstText);
    FSrcVerse.VersiculoPar := FDstVerse;
    FSrcVerse.ReadOnly := true;
    FDstVerse.ReadOnly := true;
  end
  else
  begin
    // Create non-active verses bound to the scrollboxes so FPanel is not nil
    if not Assigned(FSrcVerse) then
      FSrcVerse := TVersiculo.Criar(nil, ScrollBoxSrcText);
    if not Assigned(FDstVerse) then
      FDstVerse := TVersiculo.Criar(nil, ScrollBoxDstText);
    FSrcVerse.VersiculoPar := FDstVerse;
  end;
end;

procedure TFrmAIProcessing.LoadControlsFromBatch;
begin
  EnsureVersesCreated;
  if (FIndex < 0) or (FIndex >= FBatch.Count) then Exit;
  
  // Set text first, then pairs so SetPares can properly select syntagms
  FSrcVerse.Texto := FBatch.SourceText[FIndex];
  FDstVerse.Texto := FBatch.DestinationText[FIndex];
  
  // Set pairs AFTER text so syntagms exist and can be selected
  if FBatch.Pairs[FIndex] <> '' then
  begin
    FSrcVerse.Pares := FBatch.Pairs[FIndex];
    AIDebugLogFmt('[LoadControlsFromBatch] Pairs loaded and rendered for index %d', [FIndex]);
  end
  else
  begin
    // Clear any previous selections if no pairs
    FSrcVerse.LimparSelecao;
    FDstVerse.LimparSelecao;
    AIDebugLogFmt('[LoadControlsFromBatch] No pairs for index %d, cleared selections', [FIndex]);
  end;
  
  FActualCommentsMemo.SetCommentsText(FBatch.Comments[FIndex]);
end;

procedure TFrmAIProcessing.LoadControlsFromOriginal;
begin
  EnsureVersesCreated;
  if (FIndex < 0) or (FIndex >= FOriginalData.Count) then Exit;
  
  FSrcVerse.Texto := FOriginalData.SourceText[FIndex];
  FDstVerse.Texto := FOriginalData.DestinationText[FIndex];
  FSrcVerse.Pares := FOriginalData.Pairs[FIndex];
  FActualCommentsMemo.SetCommentsText(FOriginalData.Comments[FIndex]);
end;

procedure TFrmAIProcessing.UpdateProgress;
var
  total, completed, failed, processed: Integer;
  percent: Integer;
begin
  if not Assigned(FManager) then Exit;
  
  total := FManager.TotalJobs;
  completed := FManager.CompletedJobs;
  failed := FManager.FailedJobs;
  processed := completed + failed;
  
  if total > 0 then
    percent := (processed * 100) div total
  else
    percent := 0;
    
  ProgressBar1.Max := total;
  ProgressBar1.Position := processed;
  
  case FStatus of
    apsRunning:
      LabelProgress.Caption := Format(SAIProcessingInProgress, [processed, total, percent]);
    apsCompleted:
      LabelProgress.Caption := Format(SAIProcessingCompleted, [completed, failed]);
    apsCancelled:
      LabelProgress.Caption := SAIProcessingCancelled;
  end;
  
  if FStatus = apsRunning then
  begin
    if (total > 0) and not FManager.IsIdle then
    begin
      LabelETA.Caption := Format(SEstimatedTimeRemaining, [FManager.GetETAFormatted]);
      LabelETA.Visible := True;
    end
    else
      LabelETA.Visible := False;
  end
  else
    LabelETA.Visible := False;
    
  Application.ProcessMessages;
end;

procedure TFrmAIProcessing.UpdateTreeNodeStates;
var
  i: Integer;
begin
  // Update tree node icons based on processing state
  for i := 0 to FBatch.Count - 1 do
  begin
    // The tree nodes will be updated based on state
    // This requires extending TBibleTreeView with AI state icons
    // For now, we just ensure the ProcessingState entry exists
    // (no UI update performed here yet)
  end;
end;

procedure TFrmAIProcessing.PumpProcessing;
begin
  if not Assigned(FManager) then Exit;
  // Let manager process completed jobs
  try
    FManager.PumpApply;
  except
    on E: Exception do
    begin
      AIDebugLogFmt('[PumpProcessing] Exception from FManager.PumpApply: %s - %s', [E.ClassName, E.Message]);
      // Disable timer to avoid repeated crashes and surface state to user
      TimerUpdate.Enabled := False;
      LabelProgress.Caption := 'Error during AI processing (see logs)';
      BtnCancel.Enabled := False;
      BtnApply.Enabled := True;
      BtnClose.Enabled := True;
      Exit;
    end;
  end;

  UpdateProgress;

  if FManager.IsIdle then
  begin
    if FStatus = apsCancelling then
      FStatus := apsCancelled
    else if FStatus = apsRunning then
      FStatus := apsCompleted;

    TimerUpdate.Enabled := False;
    BtnCancel.Enabled := False;
    BtnApply.Enabled := True;
    BtnClose.Enabled := True;
    
    // Convert AI state icons back to checkboxes for user review
    // Success (green) becomes checked, Error (red) becomes unchecked
    FBibleTreeView.ClearAllAIStates;
    
    UpdateProgress;
  end;

  Application.ProcessMessages; // IMPORTANT for UI responsiveness
end;

procedure TFrmAIProcessing.OnJobStateChanged(Sender: TObject; JobIndex: Integer; 
  NewState: TAIJobState; ErrorMsg: string);
var
  iconIndex: Integer;
  stateName: string;
begin
  if (JobIndex < 0) or (JobIndex >= FBatch.Count) then Exit;
  
  // Map job state to icon index
  case NewState of
    ajsPending: 
      begin
        iconIndex := ImgIndexAIPending;
        stateName := 'pending';
      end;
    ajsFetching: 
      begin
        iconIndex := ImgIndexAIProcessing;
        stateName := 'processing';
      end;
    ajsCompleted: 
      begin
        iconIndex := ImgIndexAISuccess;
        stateName := 'success';
      end;
    ajsFailed: 
      begin
        iconIndex := ImgIndexAIError;
        stateName := 'error';
      end;
  else
    Exit;
  end;
  
  AIDebugLogFmt('[OnJobStateChanged] JobIndex=%d, NewState=%d (%s), IconIndex=%d', 
    [JobIndex, Ord(NewState), stateName, iconIndex]);
  
  // Update tree node icon
  FBibleTreeView.AIStatesEnabled := True;
  FBibleTreeView.SetVerseAIState(JobIndex, iconIndex);
  
  // Update batch state
  FBatch.SetState(JobIndex, stateName);
  
  // Store error message if failed
  if NewState = ajsFailed then
    FBatch.SetError(JobIndex, ErrorMsg);
end;

procedure TFrmAIProcessing.OnJobCompletedEvent(Sender: TObject; JobIndex: Integer;
  Result: TAIFlowResult);
var
  translation: TAITranslationResult;
  association: TAssociationResult;
begin
  if (JobIndex < 0) or (JobIndex >= FBatch.Count) then Exit;
  if not Assigned(Result) then Exit;

  if Result is TAITranslationResult then
  begin
    translation := TAITranslationResult(Result);
    // Use the translated text from the result (already updated in temp verse by ApplyResult)
    FBatch.DestinationText[JobIndex] := translation.TranslatedText;
    AIDebugLogFmt('[OnJobCompletedEvent] Translation result: "%s"', [translation.TranslatedText]);
  end
  else if Result is TAssociationResult then
  begin
    association := TAssociationResult(Result);
    
    // Store comments from the association result
    if association.Comments <> '' then
      FBatch.Comments[JobIndex] := association.Comments;
    
    // Store the PairsXML that was populated by ApplyResult
    // This is the XML representation of associations created during batch processing
    if association.PairsXML <> '' then
    begin
      FBatch.Pairs[JobIndex] := association.PairsXML;
      AIDebugLogFmt('[OnJobCompletedEvent] Association PairsXML stored (%d chars)', [Length(association.PairsXML)]);
    end;
  end;

  // If this is the currently displayed verse, refresh the view
  if JobIndex = FIndex then
    LoadControlsFromBatch;
end;

function TFrmAIProcessing.StartProcessing(Manager: TAIProcessingManager;
  Flow: IAIVerseFlow; Verses: TList; Project: TObject): boolean;
var
  i: Integer;
  verseData: PVerseData;
  ref: string;
  proj: TProjeto;
begin
  Result := False;
  
  FManager := Manager;
  FFlow := Flow;
  FProject := Project;
  FStatus := apsRunning;
  EnsureVersesCreated;
  
  proj := TProjeto(Project); // Cast to use methods
  
  // Initialize batch with verse data
  FBatch.Reference.Clear;
  FBatch.SourceText.Clear;
  FBatch.DestinationText.Clear;
  FBatch.Pairs.Clear;
  FBatch.Comments.Clear;
  FBatch.Status.Clear;
  FBatch.ProcessingState.Clear;
  FBatch.Errors.Clear;
  
  // Load original data and initialize batch
  // Note: verse data is already collected by caller, no dataset navigation needed
  for i := 0 to Verses.Count - 1 do
  begin
    verseData := PVerseData(Verses[i]);
    ref := verseData^.ReferenceID;
    
    // Store original data
    FOriginalData.Add(
      ref,
      verseData^.OriginalText,
      verseData^.TranslatedText,
      verseData^.Associations,
      verseData^.Comments,
      verseData^.Status
    );
    
    // Initialize batch with same data (will be updated as processing completes)
    FBatch.Add(
      ref,
      verseData^.OriginalText,
      verseData^.TranslatedText,
      verseData^.Associations,
      verseData^.Comments,
      verseData^.Status
    );
  end;
  
  // Setup tree view
  FBibleTreeView.VerseList := FBatch.Reference;
  FIndex := 0;
  TabControlPreviewChange(TabControlPreview);
  
  // Setup callbacks BEFORE starting batch processing
  FManager.OnJobStateChange := @OnJobStateChanged;
  FManager.OnJobCompleted := @OnJobCompletedEvent;
  
  // Setup UI
  Caption := SAIProcessingTitle + ' - ' + Flow.Name;
  BtnApply.Enabled := False;
  BtnClose.Enabled := False;
  BtnCancel.Enabled := True;
  ProgressBar1.Max := FBatch.Count;
  ProgressBar1.Position := 0;
  
  // Start the timer for updates
  TimerUpdate.Enabled := True;
  
  // Note: Manager.StartBatch will be called by the caller after StartProcessing
  
  Result := True;
end;

procedure TFrmAIProcessing.Apply;
var
  i, idx: Integer;
  proj: TProjeto;
  patch: TPatchFile;
  selectedIndexes: TIntegerList;
  appliedCount: Integer;
begin
  if not Assigned(FProject) then Exit;
  
  proj := TProjeto(FProject);
  
  // Disable verse change callbacks during Apply to prevent UI refresh while DB is locked
  FBibleTreeView.OnVerseChange := nil;
  
  AIDebugLogFmt('[Apply] Starting batch apply. FBatch.Count=%d', [FBatch.Count]);
  
  // Try to get selected verses from tree view
  selectedIndexes := nil;
  try
    selectedIndexes := FBibleTreeView.IndexesList;
    AIDebugLogFmt('[Apply] Tree selection: %d items', [selectedIndexes.Count]);
  except
    on E: Exception do
    begin
      AIDebugLogFmt('[Apply] Exception getting IndexesList: %s', [E.Message]);
    end;
  end;
  
  // If no selection, use all verses
  if not Assigned(selectedIndexes) or (selectedIndexes.Count = 0) then
  begin
    AIDebugLogFmt('[Apply] No tree selection, applying all %d batch items', [FBatch.Count]);
    FreeAndNil(selectedIndexes);
    selectedIndexes := TIntegerList.Create;
    for i := 0 to FBatch.Count - 1 do
      selectedIndexes.Add(i);
  end
  else
  begin
    AIDebugLogFmt('[Apply] Applying %d selected verses from tree', [selectedIndexes.Count]);
  end;
  
  try
    // Create patch from batch data
    patch := TPatchFile.Create;
    try
      appliedCount := 0;
      
      // Add only changed verses to patch
      for i := 0 to selectedIndexes.Count - 1 do
      begin
        idx := selectedIndexes[i];
        if (idx >= 0) and (idx < FBatch.Count) then
        begin
          // Only include verses that have changed
          if FBatch.Pairs[idx] <> FOriginalData.Pairs[idx] then
          begin
            patch.Add(
              FBatch.Reference[idx],
              FBatch.SourceText[idx],
              FBatch.DestinationText[idx],
              FBatch.Pairs[idx],
              FBatch.Comments[idx],
              StrToInt(FBatch.Status[idx])
            );
            Inc(appliedCount);
            AIDebugLogFmt('[Apply] Added to patch: %s', [FBatch.Reference[idx]]);
          end;
        end;
      end;
      
      AIDebugLogFmt('[Apply] Patch created with %d verses', [appliedCount]);
      
      // Apply the patch using TProjeto.ApplyPatch (handles scrolling session, DB updates, etc.)
      if appliedCount > 0 then
      begin
        // ApplyPatch with nil indexes applies all patch items
        AIDebugLogFmt('[Apply] Calling ApplyPatch with %d verses', [appliedCount]);
        proj.ApplyPatch(patch, nil);
        AIDebugLogFmt('[Apply] ApplyPatch completed successfully', []);
      end;
      
    finally
      patch.Free;
    end;
  finally
    if Assigned(selectedIndexes) then
      selectedIndexes.Free;
      
    // Re-enable verse change callbacks after Apply completes
    // Add a small delay to ensure DB lock is released
    Sleep(100);
    FBibleTreeView.OnVerseChange := @OnVerseChange;
  end;
  
  AIDebugLogFmt('[Apply] Batch apply complete. Applied %d verses', [appliedCount]);
  ShowMessage(Format(SApplySuccessful + ' (%d verses)', [appliedCount]));
end;

initialization

end.
