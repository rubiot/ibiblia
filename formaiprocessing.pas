unit formaiprocessing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, BibleTreeView, Verse, CommentsView,
  ai_processing_manager, ai_flows, ai_processing_batch, ai_batch_work_item;

type
  TAIProcessingStatus = (apsIdle, apsRunning, apsCancelling, apsCompleted, apsCancelled);

  TAIProcessingApplyBatchFunc = function(Batch, OriginalData: TAIProcessingBatch;
    SelectedIndexes: TAIBatchIndexList): Integer of object;

  TAIProcessingPreviewOptions = record
    TokenizerVersion: Integer;
    SourceFont: TFont;
    DestinationFont: TFont;
    BoldStrongs: Boolean;
    StrongsCountMode: TStrongsCountMode;
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
    LabelTokens: TLabel;
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
    FApplyAcceptedBatch: TAIProcessingApplyBatchFunc;
    FPreviewOptions: TAIProcessingPreviewOptions;
    FIndex: integer;
    FSrcVerse: TVerse;
    FDstVerse: TVerse;
    FActualCommentsMemo: TCommentsView;
    FStatus: TAIProcessingStatus;
    FOriginalData: TAIProcessingBatch; // Store original project data for comparison
    FLastTotalTokens: Int64;
    FPanelReview: TPanel;
    FPanelReviewButtons: TPanel;
    FLabelReviewSummary: TLabel;
    FMemoVerseReview: TMemo;
    FBtnAcceptVerse: TButton;
    FBtnRejectVerse: TButton;
    FBtnRetryErrors: TButton;
    FPanelCommentsReview: TPanel;
    FPanelCommentsHeader: TPanel;
    FPanelCommentsDetails: TPanel;
    FBtnToggleComments: TButton;
    FLabelCommentsReviewSummary: TLabel;
    FRadioCommentsDecision: TRadioGroup;
    FGroupCurrentComments: TGroupBox;
    FGroupAppliedComments: TGroupBox;
    FCurrentCommentsView: TCommentsView;
    FAppliedCommentsView: TCommentsView;
    FCommentsExpanded: Boolean;
    
    procedure OnVerseChange(index: integer);
      procedure EnsureVersesCreated;
    procedure CreateReviewControls;
    procedure CreateCommentsReviewControls;
    procedure ApplyLocalizedCaptions;
    procedure LoadControlsFromBatch;
    procedure LoadControlsFromOriginal;
    procedure UpdateProgress;
    procedure UpdateTokenUsageLabel;
    procedure UpdateReviewPanel;
    procedure UpdateReviewButtons;
    procedure UpdateApplySummary;
    procedure UpdateTreeNodeStates;
    procedure PumpProcessing;
    procedure OnJobStateChanged(Sender: TObject; JobIndex: Integer; NewState: TAIJobState; ErrorMsg: string);
    procedure OnJobCompletedEvent(Sender: TObject; JobIndex: Integer; Result: TAIFlowResult);
    procedure OnTreeSelectionChanged(Sender: TObject);
    procedure BtnAcceptVerseClick(Sender: TObject);
    procedure BtnRejectVerseClick(Sender: TObject);
    procedure BtnRetryErrorsClick(Sender: TObject);
    procedure BtnToggleCommentsClick(Sender: TObject);
    procedure RadioCommentsDecisionClick(Sender: TObject);
    procedure SyncBatchAcceptanceFromTree;
    procedure SetControlHint(AControl: TControl; const AHint: string);
    procedure UpdateCommentsReviewPanel;
    procedure UpdateReviewPanelLayout;
    procedure ApplyCurrentCommentsDecision;
    function CountAssociations(const PairsXml: string): Integer;
    function CurrentVerseHasCommentProposal: Boolean;
    function CommentsDecisionToItemIndex(Decision: TAICommentsDecision): Integer;
    function ItemIndexToCommentsDecision(ItemIndex: Integer): TAICommentsDecision;
    function CommentsDecisionDisplayName(Decision: TAICommentsDecision): string;
    function FlowDisplayName(const Flow: IAIVerseFlow): string;
    function BuildSelectedIndexes: TAIBatchIndexList;
    function CountFailedVerses: Integer;
    function BuildApplySummary(out SelectedVerseCount, AcceptedVerseCount,
      AcceptedChangeCount, FailedCount: Integer): string;
  public
    constructor Create(AOwner: TComponent); override;
    function StartProcessing(Manager: TAIProcessingManager; Flow: IAIVerseFlow; 
                             WorkItems: TAIBatchWorkItemList;
                             const PreviewOptions: TAIProcessingPreviewOptions;
                             ApplyAcceptedBatch: TAIProcessingApplyBatchFunc): boolean;
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
  STokensConsumed = 'Tokens consumed: prompt %s, completion %s, total %s';
  STokensConsumedLastIncrement = 'Tokens consumed: prompt %s, completion %s, total %s (last +%s)';
  STokensConsumedUnavailable = 'Tokens consumed: unavailable';
  STokensConsumedNone = 'Tokens consumed: 0';
  SApplySuccessful = 'Changes applied successfully';
  SApplySuccessfulFmt = 'Changes applied successfully (%d verses)';
  SCancelConfirmation = 'Are you sure you want to cancel the AI processing?';
  SConfirmation = 'Confirmation';
  SDiscardConfirmation = 'Are you sure you want to discard the AI results?';
  SAIProcessingErrorSeeLogs = 'Error during AI processing (see logs)';
  SAIReviewGroupCaption = 'Verses to apply';
  SAIReviewAIResultsTab = 'AI results';
  SAIReviewCurrentProjectTab = 'Current project';
  SAIReviewSelectAll = 'Accept all';
  SAIReviewDeselectAll = 'Reject all';
  SAIReviewApplyAccepted = 'Apply accepted';
  SAIReviewDiscard = 'Discard';
  SAIReviewCancel = 'Cancel';
  SAIReviewErrorPrefix = 'Error: %s';
  SAIReviewSummaryFmt = 'Selected: %d verses; ready to apply: %d verses; failed: %d.';
  SAIReviewReadySummaryFmt = 'Ready to apply %d accepted verses.';
  SAIReviewApplyQuestion = 'Apply the accepted AI changes now?';
  SAIReviewNoAcceptedChanges = 'There are no accepted AI changes selected to apply.';
  SAIReviewFailedWarningFmt = '%d verses failed. They will not be applied unless you retry them successfully.';
  SAIReviewAcceptVerse = 'Accept verse';
  SAIReviewRejectVerse = 'Reject verse';
  SAIReviewRetryFailed = 'Retry failed';
  SAIReviewSelectAllHint = 'Mark all completed verses to apply their accepted AI proposals.';
  SAIReviewDeselectAllHint = 'Unmark all verses. Nothing will be applied until verses are accepted again.';
  SAIReviewAcceptVerseHint = 'Mark the current verse for application. Comments follow the selected comment option.';
  SAIReviewRejectVerseHint = 'Unmark the current verse so the project stays unchanged for this verse.';
  SAIReviewRetryFailedHint = 'Run the AI again only for verses that failed.';
  SAIReviewApplyAcceptedHint = 'Apply only the marked verses.';
  SAIReviewDiscardHint = 'Close this review without applying AI results.';
  SAIReviewCancelHint = 'Stop the AI processing that is currently running.';
  SAIReviewNoFailedVerses = 'There are no failed verses to retry.';
  SAIReviewRetryStartedFmt = 'Retrying %d failed verses.';
  SAIReviewVerseAccepted = 'This verse is accepted and will be applied.';
  SAIReviewVerseRejected = 'This verse is rejected and will not be applied.';
  SAIReviewVerseNoChanges = 'The AI did not propose changes for this verse.';
  SAIReviewVersePending = 'Waiting for AI result.';
  SAIReviewVerseProcessing = 'Processing this verse...';
  SAIReviewAssociationSummaryFmt = 'Associations proposed: %d (current project: %d).';
  SAIReviewAssociationProgressFmt = 'Association coverage in displayed preview: %.0f%%.';
  SAIReviewTranslationSummary = 'Review the proposed destination text above.';
  SAIReviewCommentsAvailable = 'AI comments are shown below.';
  SAIReviewNoComments = 'No AI comments for this verse.';
  SAIReviewCommentsChangedFmt = 'AI proposed comments. Current choice: %s.';
  SAIReviewCommentsSectionSummaryFmt = 'Comments: %s';
  SAIReviewShowComments = 'Review comments';
  SAIReviewHideComments = 'Hide comments';
  SAIReviewCurrentComments = 'Current comment';
  SAIReviewAppliedComments = 'After applying';
  SAIReviewCommentsDecisionCaption = 'Comment choice';
  SAIReviewKeepCurrentComments = 'Keep current comment';
  SAIReviewUseAIComments = 'Use AI comment';
  SAIReviewMergeComments = 'Keep both comments';
  SAIFlowAssociation = 'Association';
  SAIFlowTranslation = 'Translation';

implementation

{$R *.lfm}

uses ai_debug;

{ TFrmAIProcessing }

constructor TFrmAIProcessing.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  // Initialize batch objects immediately
  FBatch := TAIProcessingBatch.Create;
  FOriginalData := TAIProcessingBatch.Create;
  
  FStatus := apsIdle;
  FIndex := 0;
  FApplyAcceptedBatch := nil;
  FillChar(FPreviewOptions, SizeOf(FPreviewOptions), 0);
  FLastTotalTokens := 0;
  
  // Note: child visual controls are created in FormCreate after .lfm is loaded
end;

procedure TFrmAIProcessing.FormCreate(Sender: TObject);
begin
  // Create child controls now that .lfm components exist
  FBibleTreeView := TBibleTreeView.Create(Self);
  FBibleTreeView.Parent := GroupBoxVerses;
  FBibleTreeView.Align := alClient;
  FBibleTreeView.OnVerseChange := @OnVerseChange;
  FBibleTreeView.OnSelectionChanged := @OnTreeSelectionChanged;

  ScrollBoxSrcText.Constraints.MinHeight := 80;
  ScrollBoxDstText.Constraints.MinHeight := 80;
  MemoComments.Constraints.MinHeight := 45;

  // Do not create TVerse here; create them in FormShow with the project

  FActualCommentsMemo := TCommentsView.Create(Self);
  FActualCommentsMemo.ReadOnly := True;

  CreateReviewControls;
  ApplyLocalizedCaptions;
  LabelSrcText.Visible := False;
  LabelDstText.Visible := False;
  LabelAssociations.Visible := False;
  LabelComments.Visible := False;
  LabelStatus.Visible := False;
  BtnApply.Width := 130;
  BtnClose.Width := 115;
  BtnClose.Left := PanelBottom.ClientWidth - BtnClose.Width - 8;
  BtnApply.Left := BtnClose.Left - BtnApply.Width - 10;
  BtnApply.ModalResult := mrNone;
end;

procedure TFrmAIProcessing.BtnSelectAllClick(Sender: TObject);
var
  I: Integer;
begin
  FBibleTreeView.SelectAll;
  for I := 0 to FBatch.Count - 1 do
    FBatch.AcceptVerseProposal(FOriginalData, I);
  UpdateReviewPanel;
end;

procedure TFrmAIProcessing.BtnDeselectAllClick(Sender: TObject);
var
  I: Integer;
begin
  FBibleTreeView.SelectNone;
  for I := 0 to FBatch.Count - 1 do
    FBatch.RejectVerseProposal(I);
  UpdateReviewPanel;
end;

procedure TFrmAIProcessing.CreateReviewControls;
var
  VerseButtonsPanel: TPanel;
begin
  FPanelReview := TPanel.Create(Self);
  FPanelReview.Parent := TabControlPreview;
  FPanelReview.Align := alBottom;
  FPanelReview.Height := 145;
  FPanelReview.BevelOuter := bvNone;

  FPanelReviewButtons := TPanel.Create(Self);
  FPanelReviewButtons.Parent := FPanelReview;
  FPanelReviewButtons.Align := alTop;
  FPanelReviewButtons.Height := 64;
  FPanelReviewButtons.BevelOuter := bvNone;

  FLabelReviewSummary := TLabel.Create(Self);
  FLabelReviewSummary.Parent := FPanelReviewButtons;
  FLabelReviewSummary.Align := alTop;
  FLabelReviewSummary.Height := 28;
  FLabelReviewSummary.Layout := tlCenter;
  FLabelReviewSummary.AutoSize := False;
  FLabelReviewSummary.WordWrap := False;

  VerseButtonsPanel := TPanel.Create(Self);
  VerseButtonsPanel.Parent := FPanelReviewButtons;
  VerseButtonsPanel.Align := alClient;
  VerseButtonsPanel.BevelOuter := bvNone;

  FBtnRetryErrors := TButton.Create(Self);
  FBtnRetryErrors.Parent := VerseButtonsPanel;
  FBtnRetryErrors.Align := alRight;
  FBtnRetryErrors.Width := 165;
  FBtnRetryErrors.OnClick := @BtnRetryErrorsClick;

  FBtnRejectVerse := TButton.Create(Self);
  FBtnRejectVerse.Parent := VerseButtonsPanel;
  FBtnRejectVerse.Align := alRight;
  FBtnRejectVerse.Width := 145;
  FBtnRejectVerse.OnClick := @BtnRejectVerseClick;

  FBtnAcceptVerse := TButton.Create(Self);
  FBtnAcceptVerse.Parent := VerseButtonsPanel;
  FBtnAcceptVerse.Align := alRight;
  FBtnAcceptVerse.Width := 145;
  FBtnAcceptVerse.OnClick := @BtnAcceptVerseClick;

  CreateCommentsReviewControls;

  FMemoVerseReview := TMemo.Create(Self);
  FMemoVerseReview.Parent := FPanelReview;
  FMemoVerseReview.Align := alClient;
  FMemoVerseReview.ReadOnly := True;
  FMemoVerseReview.ScrollBars := ssAutoVertical;
  FMemoVerseReview.BorderStyle := bsSingle;
end;

procedure TFrmAIProcessing.CreateCommentsReviewControls;
var
  PreviewPanel: TPanel;
begin
  FCommentsExpanded := False;

  FPanelCommentsReview := TPanel.Create(Self);
  FPanelCommentsReview.Parent := FPanelReview;
  FPanelCommentsReview.Align := alBottom;
  FPanelCommentsReview.Height := 34;
  FPanelCommentsReview.BevelOuter := bvNone;
  FPanelCommentsReview.Visible := False;

  FPanelCommentsHeader := TPanel.Create(Self);
  FPanelCommentsHeader.Parent := FPanelCommentsReview;
  FPanelCommentsHeader.Align := alTop;
  FPanelCommentsHeader.Height := 34;
  FPanelCommentsHeader.BevelOuter := bvNone;

  FBtnToggleComments := TButton.Create(Self);
  FBtnToggleComments.Parent := FPanelCommentsHeader;
  FBtnToggleComments.Align := alLeft;
  FBtnToggleComments.Width := 145;
  FBtnToggleComments.OnClick := @BtnToggleCommentsClick;

  FLabelCommentsReviewSummary := TLabel.Create(Self);
  FLabelCommentsReviewSummary.Parent := FPanelCommentsHeader;
  FLabelCommentsReviewSummary.Align := alClient;
  FLabelCommentsReviewSummary.AutoSize := False;
  FLabelCommentsReviewSummary.Layout := tlCenter;
  FLabelCommentsReviewSummary.WordWrap := False;

  FPanelCommentsDetails := TPanel.Create(Self);
  FPanelCommentsDetails.Parent := FPanelCommentsReview;
  FPanelCommentsDetails.Align := alClient;
  FPanelCommentsDetails.BevelOuter := bvNone;
  FPanelCommentsDetails.Visible := False;

  FRadioCommentsDecision := TRadioGroup.Create(Self);
  FRadioCommentsDecision.Parent := FPanelCommentsDetails;
  FRadioCommentsDecision.Align := alTop;
  FRadioCommentsDecision.Height := 56;
  FRadioCommentsDecision.Columns := 3;
  FRadioCommentsDecision.OnClick := @RadioCommentsDecisionClick;

  PreviewPanel := TPanel.Create(Self);
  PreviewPanel.Parent := FPanelCommentsDetails;
  PreviewPanel.Align := alClient;
  PreviewPanel.BevelOuter := bvNone;

  FGroupCurrentComments := TGroupBox.Create(Self);
  FGroupCurrentComments.Parent := PreviewPanel;
  FGroupCurrentComments.Align := alLeft;
  FGroupCurrentComments.Width := 260;

  FCurrentCommentsView := TCommentsView.Create(Self);
  FCurrentCommentsView.Parent := FGroupCurrentComments;
  FCurrentCommentsView.Align := alClient;
  FCurrentCommentsView.ReadOnly := True;
  FCurrentCommentsView.FontName := 'default';
  FCurrentCommentsView.FontSize := 10;

  FGroupAppliedComments := TGroupBox.Create(Self);
  FGroupAppliedComments.Parent := PreviewPanel;
  FGroupAppliedComments.Align := alClient;

  FAppliedCommentsView := TCommentsView.Create(Self);
  FAppliedCommentsView.Parent := FGroupAppliedComments;
  FAppliedCommentsView.Align := alClient;
  FAppliedCommentsView.ReadOnly := True;
  FAppliedCommentsView.FontName := 'default';
  FAppliedCommentsView.FontSize := 10;
end;

procedure TFrmAIProcessing.ApplyLocalizedCaptions;
begin
  Caption := SAIProcessingTitle;
  GroupBoxVerses.Caption := SAIReviewGroupCaption;
  BtnSelectAll.Caption := SAIReviewSelectAll;
  BtnDeselectAll.Caption := SAIReviewDeselectAll;
  BtnCancel.Caption := SAIReviewCancel;
  BtnApply.Caption := SAIReviewApplyAccepted;
  BtnClose.Caption := SAIReviewDiscard;
  TabControlPreview.Tabs[0] := SAIReviewAIResultsTab;
  TabControlPreview.Tabs[1] := SAIReviewCurrentProjectTab;

  FBtnAcceptVerse.Caption := SAIReviewAcceptVerse;
  FBtnRejectVerse.Caption := SAIReviewRejectVerse;
  FBtnRetryErrors.Caption := SAIReviewRetryFailed;
  FBtnToggleComments.Caption := SAIReviewShowComments;
  FGroupCurrentComments.Caption := SAIReviewCurrentComments;
  FGroupAppliedComments.Caption := SAIReviewAppliedComments;
  FRadioCommentsDecision.Caption := SAIReviewCommentsDecisionCaption;
  FRadioCommentsDecision.Items.Clear;
  FRadioCommentsDecision.Items.Add(SAIReviewKeepCurrentComments);
  FRadioCommentsDecision.Items.Add(SAIReviewUseAIComments);
  FRadioCommentsDecision.Items.Add(SAIReviewMergeComments);

  SetControlHint(BtnSelectAll, SAIReviewSelectAllHint);
  SetControlHint(BtnDeselectAll, SAIReviewDeselectAllHint);
  SetControlHint(FBtnAcceptVerse, SAIReviewAcceptVerseHint);
  SetControlHint(FBtnRejectVerse, SAIReviewRejectVerseHint);
  SetControlHint(FBtnRetryErrors, SAIReviewRetryFailedHint);
  SetControlHint(BtnApply, SAIReviewApplyAcceptedHint);
  SetControlHint(BtnClose, SAIReviewDiscardHint);
  SetControlHint(BtnCancel, SAIReviewCancelHint);
end;

procedure TFrmAIProcessing.SetControlHint(AControl: TControl; const AHint: string);
begin
  if not Assigned(AControl) then
    Exit;

  AControl.Hint := AHint;
  AControl.ShowHint := True;
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
var
  selectedVerseCount, acceptedVerseCount, acceptedChangeCount, failedCount: Integer;
  msg: string;
begin
  msg := BuildApplySummary(selectedVerseCount, acceptedVerseCount,
    acceptedChangeCount, failedCount);
  if acceptedVerseCount = 0 then
  begin
    MessageDlg(SConfirmation, SAIReviewNoAcceptedChanges, mtInformation, [mbOK], 0);
    ModalResult := mrNone;
    Exit;
  end;

  msg := msg + LineEnding + LineEnding + Format(SAIReviewReadySummaryFmt,
    [acceptedVerseCount]);
  if failedCount > 0 then
    msg := msg + LineEnding + Format(SAIReviewFailedWarningFmt, [failedCount]);
  msg := msg + LineEnding + LineEnding + SAIReviewApplyQuestion;

  if MessageDlg(SConfirmation, msg, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
  begin
    ModalResult := mrNone;
    Exit;
  end;

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
begin
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

  EnsureVersesCreated;
  
  FActualCommentsMemo.FontName := 'default';
  FActualCommentsMemo.FontSize := 10;

  FBibleTreeView.FullExpand;
  if FStatus = apsRunning then
    TimerUpdate.Enabled := True;
end;

procedure TFrmAIProcessing.TabControlPreviewChange(Sender: TObject);
begin
  if FBatch.Count = 0 then Exit;
  
  case TabControlPreview.TabIndex of
    0: LoadControlsFromBatch;
    1: LoadControlsFromOriginal;
  end;
  
  UpdateReviewPanel;
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
  FCommentsExpanded := False;
  TabControlPreviewChange(TabControlPreview);
end;

procedure TFrmAIProcessing.OnTreeSelectionChanged(Sender: TObject);
begin
  if FStatus <> apsRunning then
    SyncBatchAcceptanceFromTree;
  UpdateReviewPanel;
end;

procedure TFrmAIProcessing.BtnAcceptVerseClick(Sender: TObject);
begin
  if (FIndex < 0) or (FIndex >= FBatch.Count) then
    Exit;

  FBatch.AcceptVerseProposal(FOriginalData, FIndex);
  FBibleTreeView.SetVerseChecked(FIndex, True);
  UpdateReviewPanel;
end;

procedure TFrmAIProcessing.BtnRejectVerseClick(Sender: TObject);
begin
  if (FIndex < 0) or (FIndex >= FBatch.Count) then
    Exit;

  FBatch.RejectVerseProposal(FIndex);
  FBibleTreeView.SetVerseChecked(FIndex, False);
  UpdateReviewPanel;
end;

procedure TFrmAIProcessing.BtnRetryErrorsClick(Sender: TObject);
var
  Retried: Integer;
begin
  if not Assigned(FManager) then
    Exit;

  Retried := FManager.RetryFailedJobs;
  if Retried = 0 then
  begin
    MessageDlg(SConfirmation, SAIReviewNoFailedVerses, mtInformation, [mbOK], 0);
    Exit;
  end;

  FStatus := apsRunning;
  TimerUpdate.Enabled := True;
  BtnCancel.Enabled := True;
  BtnApply.Enabled := False;
  BtnClose.Enabled := False;
  MessageDlg(SConfirmation, Format(SAIReviewRetryStartedFmt, [Retried]), mtInformation, [mbOK], 0);
  UpdateProgress;
  UpdateReviewPanel;
end;

procedure TFrmAIProcessing.BtnToggleCommentsClick(Sender: TObject);
begin
  FCommentsExpanded := not FCommentsExpanded;
  UpdateCommentsReviewPanel;
end;

procedure TFrmAIProcessing.RadioCommentsDecisionClick(Sender: TObject);
begin
  ApplyCurrentCommentsDecision;
  UpdateReviewPanel;
end;

procedure TFrmAIProcessing.ApplyCurrentCommentsDecision;
begin
  if (FIndex < 0) or (FIndex >= FBatch.Count) then
    Exit;

  FBatch.SetCommentsDecision(FIndex,
    ItemIndexToCommentsDecision(FRadioCommentsDecision.ItemIndex));

  if FBibleTreeView.IsVerseChecked(FIndex) then
    FBatch.AcceptVerseProposal(FOriginalData, FIndex);
end;

procedure TFrmAIProcessing.SyncBatchAcceptanceFromTree;
var
  I: Integer;
begin
  if not Assigned(FBibleTreeView) then
    Exit;

  for I := 0 to FBatch.Count - 1 do
    if FBibleTreeView.IsVerseChecked(I) then
      FBatch.AcceptVerseProposal(FOriginalData, I)
    else
      FBatch.RejectVerseProposal(I);
end;

procedure TFrmAIProcessing.UpdateReviewPanelLayout;
begin
  if not Assigned(FPanelReview) then
    Exit;

  if Assigned(FPanelCommentsDetails) then
    FPanelCommentsDetails.Visible := FCommentsExpanded and
      Assigned(FPanelCommentsReview) and FPanelCommentsReview.Visible;

  if Assigned(FBtnToggleComments) then
  begin
    if FCommentsExpanded then
      FBtnToggleComments.Caption := SAIReviewHideComments
    else
      FBtnToggleComments.Caption := SAIReviewShowComments;
  end;

  if not Assigned(FPanelCommentsReview) or not FPanelCommentsReview.Visible then
    FPanelReview.Height := 145
  else if FCommentsExpanded then
  begin
    FPanelCommentsReview.Height := 220;
    FPanelReview.Height := 360;
  end
  else
  begin
    FPanelCommentsReview.Height := 34;
    FPanelReview.Height := 185;
  end;
end;

function TFrmAIProcessing.CurrentVerseHasCommentProposal: Boolean;
begin
  Result := (FIndex >= 0) and (FIndex < FBatch.Count) and
    (FIndex < FOriginalData.Count) and
    (FBatch.Errors[FIndex] = '') and
    not SameText(FBatch.ProcessingState[FIndex], 'pending') and
    not SameText(FBatch.ProcessingState[FIndex], 'processing') and
    (Trim(FBatch.Comments[FIndex]) <> '') and
    (FBatch.Comments[FIndex] <> FOriginalData.Comments[FIndex]);
end;

function TFrmAIProcessing.CommentsDecisionToItemIndex(
  Decision: TAICommentsDecision): Integer;
begin
  case Decision of
    acdUseAI:
      Result := 1;
    acdMerge:
      Result := 2;
  else
    Result := 0;
  end;
end;

function TFrmAIProcessing.ItemIndexToCommentsDecision(
  ItemIndex: Integer): TAICommentsDecision;
begin
  case ItemIndex of
    1:
      Result := acdUseAI;
    2:
      Result := acdMerge;
  else
    Result := acdKeepCurrent;
  end;
end;

function TFrmAIProcessing.CommentsDecisionDisplayName(
  Decision: TAICommentsDecision): string;
begin
  case Decision of
    acdUseAI:
      Result := SAIReviewUseAIComments;
    acdMerge:
      Result := SAIReviewMergeComments;
  else
    Result := SAIReviewKeepCurrentComments;
  end;
end;

function TFrmAIProcessing.CountAssociations(const PairsXml: string): Integer;
var
  SearchStart: SizeInt;
  FoundAt: SizeInt;
  PairText: string;
begin
  Result := 0;
  PairText := LowerCase(PairsXml);
  SearchStart := 1;
  repeat
    FoundAt := PosEx('<par ', PairText, SearchStart);
    if FoundAt <= 0 then
      Break;
    Inc(Result);
    SearchStart := FoundAt + 5;
  until SearchStart > Length(PairText);
end;

function TFrmAIProcessing.FlowDisplayName(const Flow: IAIVerseFlow): string;
var
  FlowName: string;
begin
  if Flow = nil then
    Exit('');

  FlowName := LowerCase(Flow.Name);
  if FlowName = 'association' then
    Result := SAIFlowAssociation
  else if FlowName = 'translation' then
    Result := SAIFlowTranslation
  else
    Result := Flow.Name;
end;

function TFrmAIProcessing.BuildSelectedIndexes: TAIBatchIndexList;
var
  TreeIndexes: BibleTreeView.TIntegerList;
  I: Integer;
begin
  Result := TAIBatchIndexList.Create;
  TreeIndexes := nil;
  try
    TreeIndexes := FBibleTreeView.IndexesList;
    for I := 0 to TreeIndexes.Count - 1 do
      Result.Add(IntToStr(TreeIndexes[I]));
  finally
    if Assigned(TreeIndexes) then
      TreeIndexes.Free;
  end;
end;

function TFrmAIProcessing.CountFailedVerses: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FBatch.Count - 1 do
    if SameText(FBatch.ProcessingState[I], 'error') or (FBatch.Errors[I] <> '') then
      Inc(Result);
end;

function TFrmAIProcessing.BuildApplySummary(out SelectedVerseCount,
  AcceptedVerseCount, AcceptedChangeCount, FailedCount: Integer): string;
var
  SelectedIndexes: TAIBatchIndexList;
  I, Index: Integer;
begin
  SelectedVerseCount := 0;
  AcceptedVerseCount := 0;
  AcceptedChangeCount := 0;
  FailedCount := CountFailedVerses;

  SelectedIndexes := BuildSelectedIndexes;
  try
    SelectedVerseCount := SelectedIndexes.Count;
    for I := 0 to SelectedIndexes.Count - 1 do
    begin
      Index := StrToIntDef(SelectedIndexes[I], -1);
      if (Index < 0) or (Index >= FBatch.Count) then
        Continue;

      if FBatch.HasAcceptedChanges(FOriginalData, Index) then
      begin
        Inc(AcceptedVerseCount);
        Inc(AcceptedChangeCount, FBatch.AcceptedFieldChanges(FOriginalData, Index));
      end;
    end;
  finally
    SelectedIndexes.Free;
  end;

  Result := Format(SAIReviewSummaryFmt, [
    SelectedVerseCount,
    AcceptedVerseCount,
    FailedCount
  ]);
end;

procedure TFrmAIProcessing.EnsureVersesCreated;
begin
  // If verses already created, nothing to do
  if Assigned(FSrcVerse) and Assigned(FDstVerse) then Exit;

  if not Assigned(FSrcVerse) then
    FSrcVerse := TVerse.Create(ScrollBoxSrcText);
  if not Assigned(FDstVerse) then
    FDstVerse := TVerse.Create(ScrollBoxDstText);

  FSrcVerse.PairedVerse := FDstVerse;
  FSrcVerse.ReadOnly := true;
  FDstVerse.ReadOnly := true;
  FSrcVerse.TokenizerVersion := FPreviewOptions.TokenizerVersion;
  FDstVerse.TokenizerVersion := FPreviewOptions.TokenizerVersion;

  if Assigned(FPreviewOptions.SourceFont) then
    FSrcVerse.Font := FPreviewOptions.SourceFont;
  if Assigned(FPreviewOptions.DestinationFont) then
    FDstVerse.Font := FPreviewOptions.DestinationFont;

  FSrcVerse.BoldWordsWithStrong := FPreviewOptions.BoldStrongs;
  FSrcVerse.StrongsCountMode := FPreviewOptions.StrongsCountMode;
  FDstVerse.StrongsCountMode := FPreviewOptions.StrongsCountMode;
  FSrcVerse.Active := true;
  FDstVerse.Active := true;
end;

procedure TFrmAIProcessing.LoadControlsFromBatch;
begin
  EnsureVersesCreated;
  if (FIndex < 0) or (FIndex >= FBatch.Count) then Exit;
  
  // Set text first, then pairs so SetPares can properly select syntagms
  FSrcVerse.Text := FBatch.SourceText[FIndex];
  FDstVerse.Text := FBatch.DestinationText[FIndex];
  
  // Set pairs AFTER text so syntagms exist and can be selected
  if FBatch.Pairs[FIndex] <> '' then
  begin
    FSrcVerse.Pairs := FBatch.Pairs[FIndex];
    AIDebugLogFmt('[LoadControlsFromBatch] Pairs loaded and rendered for index %d', [FIndex]);
  end
  else
  begin
    // Clear any previous selections if no pairs
    FSrcVerse.ClearSelection;
    FDstVerse.ClearSelection;
    AIDebugLogFmt('[LoadControlsFromBatch] No pairs for index %d, cleared selections', [FIndex]);
  end;
  
  FActualCommentsMemo.SetCommentsText(FBatch.EffectiveComments(FOriginalData, FIndex));
end;

procedure TFrmAIProcessing.LoadControlsFromOriginal;
begin
  EnsureVersesCreated;
  if (FIndex < 0) or (FIndex >= FOriginalData.Count) then Exit;
  
  FSrcVerse.Text := FOriginalData.SourceText[FIndex];
  FDstVerse.Text := FOriginalData.DestinationText[FIndex];
  FSrcVerse.Pairs := FOriginalData.Pairs[FIndex];
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

  UpdateTokenUsageLabel;
    
  Application.ProcessMessages;
end;

procedure TFrmAIProcessing.UpdateTokenUsageLabel;
var
  promptTokens, completionTokens, totalTokens: Int64;
  processed: Integer;
  lastIncrement: Int64;
begin
  if not Assigned(LabelTokens) then Exit;

  if not Assigned(FManager) then
  begin
    LabelTokens.Caption := STokensConsumedNone;
    Exit;
  end;

  promptTokens := FManager.PromptTokens;
  completionTokens := FManager.CompletionTokens;
  totalTokens := FManager.TotalTokens;
  processed := FManager.CompletedJobs + FManager.FailedJobs;
  lastIncrement := totalTokens - FLastTotalTokens;

  if (promptTokens > 0) or (completionTokens > 0) or (totalTokens > 0) then
  begin
    if lastIncrement > 0 then
      LabelTokens.Caption := Format(STokensConsumedLastIncrement, [
        IntToStr(promptTokens),
        IntToStr(completionTokens),
        IntToStr(totalTokens),
        IntToStr(lastIncrement)
      ])
    else
      LabelTokens.Caption := Format(STokensConsumed, [
        IntToStr(promptTokens),
        IntToStr(completionTokens),
        IntToStr(totalTokens)
      ]);
  end
  else if processed > 0 then
    LabelTokens.Caption := STokensConsumedUnavailable
  else
    LabelTokens.Caption := STokensConsumedNone;

  FLastTotalTokens := totalTokens;
end;

procedure TFrmAIProcessing.UpdateReviewButtons;
var
  CanReview: Boolean;
  HasProposal: Boolean;
begin
  CanReview := (FStatus <> apsRunning) and (FIndex >= 0) and (FIndex < FBatch.Count);
  HasProposal := CanReview and FBatch.HasProposalChanges(FOriginalData, FIndex) and
    (FBatch.Errors[FIndex] = '');

  FBtnAcceptVerse.Enabled := HasProposal;
  FBtnRejectVerse.Enabled := HasProposal;
  FBtnRetryErrors.Enabled := (FStatus <> apsRunning) and (CountFailedVerses > 0);
end;

procedure TFrmAIProcessing.UpdateApplySummary;
var
  selectedVerseCount, acceptedVerseCount, acceptedChangeCount, failedCount: Integer;
begin
  if not Assigned(FLabelReviewSummary) then
    Exit;

  FLabelReviewSummary.Caption := BuildApplySummary(selectedVerseCount,
    acceptedVerseCount, acceptedChangeCount, failedCount);
end;

procedure TFrmAIProcessing.UpdateCommentsReviewPanel;
var
  Decision: TAICommentsDecision;
  Effective: string;
begin
  if not Assigned(FPanelCommentsReview) then
    Exit;

  FPanelCommentsReview.Visible := CurrentVerseHasCommentProposal;
  if not FPanelCommentsReview.Visible then
  begin
    FCommentsExpanded := False;
    if Assigned(FCurrentCommentsView) then
      FCurrentCommentsView.SetCommentsText('');
    if Assigned(FAppliedCommentsView) then
      FAppliedCommentsView.SetCommentsText('');
    UpdateReviewPanelLayout;
    Exit;
  end;

  Effective := FBatch.EffectiveComments(FOriginalData, FIndex);
  Decision := FBatch.CommentsDecision[FIndex];

  FLabelCommentsReviewSummary.Caption := Format(SAIReviewCommentsSectionSummaryFmt,
    [CommentsDecisionDisplayName(Decision)]);

  FRadioCommentsDecision.OnClick := nil;
  try
    FRadioCommentsDecision.ItemIndex := CommentsDecisionToItemIndex(Decision);
    FRadioCommentsDecision.Enabled := FStatus <> apsRunning;
  finally
    FRadioCommentsDecision.OnClick := @RadioCommentsDecisionClick;
  end;

  FCurrentCommentsView.SetCommentsText(FOriginalData.Comments[FIndex]);
  FAppliedCommentsView.SetCommentsText(Effective);
  UpdateReviewPanelLayout;
end;

procedure TFrmAIProcessing.UpdateReviewPanel;
var
  Lines: TStringList;
  AcceptedChanges: Integer;
  ProposedAssociations: Integer;
  CurrentAssociations: Integer;
  FlowName: string;
begin
  if not Assigned(FMemoVerseReview) then
    Exit;

  Lines := TStringList.Create;
  try
    if (FIndex < 0) or (FIndex >= FBatch.Count) then
    begin
      FMemoVerseReview.Clear;
      UpdateApplySummary;
      UpdateReviewButtons;
      UpdateCommentsReviewPanel;
      Exit;
    end;

    if FBatch.Errors[FIndex] <> '' then
      Lines.Add(Format(SAIReviewErrorPrefix, [FBatch.Errors[FIndex]]))
    else if SameText(FBatch.ProcessingState[FIndex], 'pending') then
      Lines.Add(SAIReviewVersePending)
    else if SameText(FBatch.ProcessingState[FIndex], 'processing') then
      Lines.Add(SAIReviewVerseProcessing)
    else
    begin
      AcceptedChanges := FBatch.AcceptedFieldChanges(FOriginalData, FIndex);
      if not FBatch.HasProposalChanges(FOriginalData, FIndex) then
        Lines.Add(SAIReviewVerseNoChanges)
      else if (AcceptedChanges > 0) and FBibleTreeView.IsVerseChecked(FIndex) then
        Lines.Add(SAIReviewVerseAccepted)
      else
        Lines.Add(SAIReviewVerseRejected);

      FlowName := '';
      if Assigned(FFlow) then
        FlowName := LowerCase(FFlow.Name);

      if FlowName = 'association' then
      begin
        ProposedAssociations := CountAssociations(FBatch.Pairs[FIndex]);
        CurrentAssociations := CountAssociations(FOriginalData.Pairs[FIndex]);
        Lines.Add(Format(SAIReviewAssociationSummaryFmt,
          [ProposedAssociations, CurrentAssociations]));
        Lines.Add(Format(SAIReviewAssociationProgressFmt,
          [FSrcVerse.AssociationProgress * 100]));
      end
      else if FlowName = 'translation' then
        Lines.Add(SAIReviewTranslationSummary);

      if CurrentVerseHasCommentProposal then
      begin
        FBatch.EffectiveComments(FOriginalData, FIndex);
        Lines.Add(Format(SAIReviewCommentsChangedFmt,
          [CommentsDecisionDisplayName(FBatch.CommentsDecision[FIndex])]));
      end
      else if Trim(FBatch.Comments[FIndex]) <> '' then
        Lines.Add(SAIReviewCommentsAvailable)
      else
        Lines.Add(SAIReviewNoComments);
    end;

    FMemoVerseReview.Lines.Assign(Lines);
  finally
    Lines.Free;
  end;

  UpdateApplySummary;
  UpdateReviewButtons;
  UpdateCommentsReviewPanel;
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
      LabelProgress.Caption := SAIProcessingErrorSeeLogs;
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
    UpdateReviewPanel;
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
  if NewState in [ajsPending, ajsFetching] then
  begin
    FBatch.SetError(JobIndex, '');
    FBatch.RejectVerseProposal(JobIndex);
  end;

  if JobIndex = FIndex then
    UpdateReviewPanel
  else
    UpdateApplySummary;
end;

procedure TFrmAIProcessing.OnJobCompletedEvent(Sender: TObject; JobIndex: Integer;
  Result: TAIFlowResult);
begin
  if (JobIndex < 0) or (JobIndex >= FBatch.Count) then Exit;
  if not Assigned(Result) then Exit;

  FBatch.ApplyAcceptedResult(JobIndex, Result);
  FBatch.AcceptVerseProposal(FOriginalData, JobIndex);
  FBatch.SetError(JobIndex, '');

  UpdateTokenUsageLabel;

  // If this is the currently displayed verse, refresh the view
  if JobIndex = FIndex then
  begin
    LoadControlsFromBatch;
    UpdateReviewPanel;
  end
  else
    UpdateApplySummary;
end;

function TFrmAIProcessing.StartProcessing(Manager: TAIProcessingManager;
  Flow: IAIVerseFlow; WorkItems: TAIBatchWorkItemList;
  const PreviewOptions: TAIProcessingPreviewOptions;
  ApplyAcceptedBatch: TAIProcessingApplyBatchFunc): boolean;
var
  i: Integer;
  workItem: TAIBatchWorkItem;
  verseData: TVerseData;
  ref: string;
  refList: TStringList;
begin
  Result := False;
  
  FManager := Manager;
  FFlow := Flow;
  FPreviewOptions := PreviewOptions;
  FApplyAcceptedBatch := ApplyAcceptedBatch;
  FStatus := apsRunning;
  FLastTotalTokens := 0;
  EnsureVersesCreated;
  
  // Initialize batch with verse data
  FBatch.Clear;
  FOriginalData.Clear;
  
  // Load original data and initialize batch
  // Note: verse data is already collected by caller, no dataset navigation needed
  for i := 0 to WorkItems.Count - 1 do
  begin
    workItem := WorkItems[i];
    verseData := workItem.VerseData;
    ref := verseData.ReferenceID;
    
    // Store original data
    FOriginalData.Add(
      ref,
      verseData.OriginalText,
      verseData.TranslatedText,
      verseData.Associations,
      verseData.Comments,
      verseData.Status
    );
    
    // Initialize batch with same data (will be updated as processing completes)
    FBatch.Add(
      ref,
      verseData.OriginalText,
      verseData.TranslatedText,
      verseData.Associations,
      verseData.Comments,
      verseData.Status
    );
  end;
  
  // Setup tree view
  refList := FBatch.BuildReferenceList;
  try
    FBibleTreeView.VerseList := refList;
  finally
    refList.Free;
  end;
  FIndex := 0;
  TabControlPreview.TabIndex := 0;
  TabControlPreviewChange(TabControlPreview);
  
  // Setup callbacks BEFORE starting batch processing
  FManager.OnJobStateChange := @OnJobStateChanged;
  FManager.OnJobCompleted := @OnJobCompletedEvent;
  
  // Setup UI
  Caption := SAIProcessingTitle + ' - ' + FlowDisplayName(Flow);
  BtnApply.Enabled := False;
  BtnClose.Enabled := False;
  BtnCancel.Enabled := True;
  ProgressBar1.Max := FBatch.Count;
  ProgressBar1.Position := 0;
  UpdateTokenUsageLabel;
  UpdateReviewPanel;
  
  // Start the timer for updates
  TimerUpdate.Enabled := True;
  
  // Note: Manager.StartBatch will be called by the caller after StartProcessing
  
  Result := True;
end;

procedure TFrmAIProcessing.Apply;
var
  selectedIndexes: TAIBatchIndexList;
  appliedCount: Integer;
begin
  if not Assigned(FApplyAcceptedBatch) then Exit;
  
  // Disable verse change callbacks during Apply to prevent UI refresh while DB is locked
  FBibleTreeView.OnVerseChange := nil;
  
  AIDebugLogFmt('[Apply] Starting batch apply. FBatch.Count=%d', [FBatch.Count]);
  
  // Try to get selected verses from tree view
  selectedIndexes := BuildSelectedIndexes;
  AIDebugLogFmt('[Apply] Applying %d selected verses from tree', [selectedIndexes.Count]);
  
  try
    appliedCount := FApplyAcceptedBatch(FBatch, FOriginalData, selectedIndexes);
    AIDebugLogFmt('[Apply] ApplyAcceptedBatch completed successfully: %d verses', [appliedCount]);
  finally
    if Assigned(selectedIndexes) then
      selectedIndexes.Free;
      
    // Re-enable verse change callbacks after Apply completes
    // Add a small delay to ensure DB lock is released
    Sleep(100);
    FBibleTreeView.OnVerseChange := @OnVerseChange;
  end;
  
  AIDebugLogFmt('[Apply] Batch apply complete. Applied %d verses', [appliedCount]);
  ShowMessage(Format(SApplySuccessfulFmt, [appliedCount]));
end;

initialization

end.
