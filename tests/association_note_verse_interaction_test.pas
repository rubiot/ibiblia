unit association_note_verse_interaction_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  fpcunit, testregistry;

type
  TAssociationNoteVerseInteractionTests = class(TTestCase)
  published
    procedure NoteHitsBypassSyntagmsRouteClicksAndEndOnLifecycleChanges;
  end;

implementation

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Syntagm, Verse,
  directwrite_syntagm_view, verse_view_intf, verse_visual_document,
  text_edit_provenance, theword_wysiwyg_session_v2;

type
  TAssociationNoteInteractionProbe = class
  private
    FClickCount: Integer;
    FEnterCount: Integer;
    FLastId: QWord;
    FLastRevision: QWord;
    FLeaveCount: Integer;
  public
    procedure NoteClick(Sender: TVerse; ANoteId, ARevision: QWord);
    procedure SyntagmEnter(Sender: TSyntagm);
    procedure SyntagmLeave(Sender: TSyntagm);
  end;

procedure TAssociationNoteInteractionProbe.NoteClick(Sender: TVerse;
  ANoteId, ARevision: QWord);
begin
  Inc(FClickCount);
  FLastId := ANoteId;
  FLastRevision := ARevision;
end;

procedure TAssociationNoteInteractionProbe.SyntagmEnter(Sender: TSyntagm);
begin
  Inc(FEnterCount);
end;

procedure TAssociationNoteInteractionProbe.SyntagmLeave(Sender: TSyntagm);
begin
  Inc(FLeaveCount);
end;

function FindAssociationView(AParent: TWinControl):
  TDirectWriteSyntagmView;
var
  Index: Integer;
begin
  Result := nil;
  if not Assigned(AParent) then
    Exit;
  for Index := 0 to AParent.ControlCount - 1 do
  begin
    if AParent.Controls[Index] is TDirectWriteSyntagmView then
      Exit(TDirectWriteSyntagmView(AParent.Controls[Index]));
    if AParent.Controls[Index] is TWinControl then
    begin
      Result := FindAssociationView(TWinControl(AParent.Controls[Index]));
      if Assigned(Result) then
        Exit;
    end;
  end;
end;

function FindSpan(const ADocument: IVerseVisualDocument;
  ARole: TVisualSpanRole; AInteractive: Boolean;
  out ASpan: TVisualSpan): Boolean;
var
  Index: Integer;
begin
  ASpan := Default(TVisualSpan);
  Result := False;
  if not Assigned(ADocument) then
    Exit;
  for Index := 0 to ADocument.SpanCount - 1 do
    if (ADocument.Spans[Index].Role = ARole) and
       (ADocument.Spans[Index].Interactive = AInteractive) then
    begin
      ASpan := ADocument.Spans[Index];
      Exit(True);
    end;
end;

function HitForSpan(const ADocument: IVerseVisualDocument;
  const ASpan: TVisualSpan): TVerseHit;
begin
  Result := Default(TVerseHit);
  Result.LogicalIndex := ASpan.LogicalIndex;
  Result.UTF16Offset := ASpan.StartUTF16;
  Result.Revision := ADocument.Revision;
  Result.Role := ASpan.Role;
  Result.ObjectId := ASpan.ObjectId;
end;

procedure TAssociationNoteVerseInteractionTests.
  NoteHitsBypassSyntagmsRouteClicksAndEndOnLifecycleChanges;
var
  ContentHit, NoteHit, StaleHit: TVerseHit;
  ContentSpan, NoteSpan: TVisualSpan;
  Container: TScrollBox;
  Document: IVerseVisualDocument;
  Host: TForm;
  Impact: TAssociationEditImpact;
  Probe: TAssociationNoteInteractionProbe;
  Session: TTheWordWysiwygSessionV2;
  Trace: TVerseEditTrace;
  VerseModel: TVerse;
  View: TDirectWriteSyntagmView;
begin
  Forms.Application.Initialize;
  Host := TForm.CreateNew(nil);
  Probe := TAssociationNoteInteractionProbe.Create;
  Container := TScrollBox.Create(Host);
  Container.Parent := Host;
  VerseModel := TVerse.Create(Container);
  try
    VerseModel.OnTranslatorNoteClick := @Probe.NoteClick;
    VerseModel.OnMouseEnter := @Probe.SyntagmEnter;
    VerseModel.OnMouseLeave := @Probe.SyntagmLeave;
    VerseModel.Text := 'word<RF>formatted <b>note</b><Rf> tail';
    View := FindAssociationView(Container);
    AssertNotNull('association DirectWrite view is available', View);
    Document := View.Document;
    AssertTrue(FindSpan(Document, vsrContent, True, ContentSpan));
    AssertTrue(FindSpan(Document, vsrTranslatorNote, False, NoteSpan));
    ContentHit := HitForSpan(Document, ContentSpan);
    NoteHit := HitForSpan(Document, NoteSpan);

    View.OnVerseHover(View, ContentHit);
    AssertEquals(1, Probe.FEnterCount);
    View.OnVerseHover(View, NoteHit);
    AssertEquals('enter is never fired for note metadata', 1,
      Probe.FEnterCount);
    AssertEquals('leaving the prior word remains balanced', 1,
      Probe.FLeaveCount);
    AssertTrue(VerseModel.TranslatorNotePreviewActive);

    View.OnVerseMouseDown(View, mbRight, [], NoteHit);
    AssertEquals('right click is consumed without editing', 0,
      Probe.FClickCount);
    AssertEquals('note metadata is not selected', 0,
      VerseModel.Selection.Count);
    AssertFalse(VerseModel.TranslatorNotePreviewActive);

    View.OnVerseHover(View, NoteHit);
    AssertTrue(VerseModel.TranslatorNotePreviewActive);
    View.OnVerseMouseDown(View, mbLeft, [], NoteHit);
    AssertEquals(1, Probe.FClickCount);
    AssertEquals(NoteSpan.ObjectId, Probe.FLastId);
    AssertEquals(Document.Revision, Probe.FLastRevision);
    AssertFalse('opening the editor ends the preview',
      VerseModel.TranslatorNotePreviewActive);

    StaleHit := NoteHit;
    Inc(StaleHit.Revision);
    View.OnVerseMouseDown(View, mbLeft, [], StaleHit);
    AssertEquals('stale note hits are routed for an explicit editor error', 2,
      Probe.FClickCount);
    AssertEquals(NoteSpan.ObjectId, Probe.FLastId);
    AssertEquals(StaleHit.Revision, Probe.FLastRevision);
    AssertEquals('stale note metadata remains non-associable', 0,
      VerseModel.Selection.Count);

    View.OnVerseHover(View, NoteHit);
    AssertTrue(VerseModel.TranslatorNotePreviewActive);
    Session := TTheWordWysiwygSessionV2.Create(
      UTF8Decode(VerseModel.XML), VerseModel.TokenizerVersion);
    Trace := nil;
    try
      AssertTrue(Session.ReplaceText(0, 4, 'WORD'));
      Trace := Session.CloneEditTrace;
      AssertTrue('a valid traced edit is committed',
        VerseModel.TryApplyTracedEdit(UTF8Encode(Session.RawText),
          Trace, Impact));
    finally
      Trace.Free;
      Session.Free;
    end;
    AssertFalse('a traced edit immediately closes the old preview',
      VerseModel.TranslatorNotePreviewActive);
    View.OnVerseHover(View, NoteHit);
    AssertFalse('the old revision cannot revive the preview',
      VerseModel.TranslatorNotePreviewActive);
    Document := View.Document;
    AssertTrue(FindSpan(Document, vsrTranslatorNote, False, NoteSpan));
    NoteHit := HitForSpan(Document, NoteSpan);
    View.OnVerseHover(View, NoteHit);
    AssertTrue('the refreshed note map supports the new revision',
      VerseModel.TranslatorNotePreviewActive);

    VerseModel.Active := False;
    AssertFalse('deactivation cancels pending hover',
      VerseModel.TranslatorNotePreviewActive);

    VerseModel.Active := True;
    VerseModel.Render;
    Document := View.Document;
    AssertTrue(FindSpan(Document, vsrTranslatorNote, False, NoteSpan));
    NoteHit := HitForSpan(Document, NoteSpan);
    View.OnVerseHover(View, NoteHit);
    VerseModel.AutomaticNoteStart := 4;
    AssertFalse('renumbering invalidates pending hover',
      VerseModel.TranslatorNotePreviewActive);

    Document := View.Document;
    AssertTrue(FindSpan(Document, vsrTranslatorNote, False, NoteSpan));
    NoteHit := HitForSpan(Document, NoteSpan);
    View.OnVerseHover(View, NoteHit);
    VerseModel.Text := 'replacement without a note';
    AssertFalse('reloading text cancels pending hover',
      VerseModel.TranslatorNotePreviewActive);
  finally
    VerseModel.Free;
    Host.Free;
    Probe.Free;
  end;
end;

initialization
  RegisterTest(TAssociationNoteVerseInteractionTests);

end.
