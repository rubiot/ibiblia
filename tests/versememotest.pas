unit versememotest;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry;

type
  TVerseMemoTests = class(TTestCase)
  published
    procedure ActivateEmptyVerseKeepsEditorAttachedToWindow;
  end;

implementation

uses
  Forms, Controls, ExtCtrls, Verse, VerseMemo;

procedure TVerseMemoTests.ActivateEmptyVerseKeepsEditorAttachedToWindow;
var
  Container: TScrollBox;
  Editor: TVerseMemo;
  Host: TForm;
  VerseView: TVerse;
begin
  Forms.Application.Initialize;
  Host := TForm.CreateNew(nil);
  Container := TScrollBox.Create(Host);
  Container.Parent := Host;
  Container.Align := alClient;
  VerseView := TVerse.Create(Container);
  Editor := TVerseMemo.Create;
  try
    Host.Show;
    Application.ProcessMessages;
    VerseView.Text := '';

    Editor.Activate(Container, VerseView);

    AssertSame(VerseView, Editor.Verse);
    Editor.DiscardEdit;
  finally
    Editor.Free;
    VerseView.Free;
    Host.Free;
  end;
end;

initialization
  RegisterTest(TVerseMemoTests);

end.
