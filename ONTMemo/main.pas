unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ONTMemo;

type

  { TForm1 }

  TForm1 = class(TForm)
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    FMemo: TONTMemo;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FMemo := TONTMemo.Create(Panel1);
  FMemo.Align := alClient;
  FMemo.Parent := Panel1;

  {
  FMemo.AddTextBlock('Normal text ');
  FMemo.AddFIBlock('added words');
  FMemo.AddTextBlock(' normal text: ');
  FMemo.AddFRBlock('"nono nono nono" ');
  FMemo.AddFOBlock('nono nono nono');
  FMemo.AddCMBlock;
  }
  FMemo.ONT := '<v="Mt 1:6">E Jessé gerou Davi, o rei.<CM>E (daquela <FI>que tinha sido a esposa<Fi> de Urias) o rei Davi gerou Salomão;';
end;

end.

