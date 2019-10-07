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

    procedure LoadSample;
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

  LoadSample;
  {
  FMemo.AddTextBlock('Normal text ');
  FMemo.AddFIBlock('added words');
  FMemo.AddTextBlock(' normal text: ');
  FMemo.AddFRBlock('"nono nono nono" ');
  FMemo.AddFOBlock('nono nono nono');
  FMemo.AddCMBlock;
  }
  //FMemo.ONT := '<v="Mt 1:6">E Jessé gerou Davi, o rei. E (daquela <FI>que tinha sido a esposa<Fi> de Urias) o rei Davi gerou Salomão;<CM><CM>' +
  //             //'<TS>The Creation<Ts><Q><wg>εν<WG1722><E> In<e><q> <Q><wg>αρχή<WG746><E> <FI>the<Fi> beginning<e><q> <Q><wg>εποίησεν ο θεός<WG4160><WG3588><WG2316><E> God made<e><q> <Q><wg>τον<WG3588><E> the<e><q> <Q><wg>ουρανόν<WG3772><E> heaven<e><q> <Q><wg>και<WG2532><E> and<e><q> <Q><wg>την<WG3588><E> the<e><q> <Q><wg>γην<WG1093><E> earth.<e><q>';
  //             '<TS>The Creation<Ts><Q><wg>εν<WG1722><E> In<e><q> <Q><wg>αρχή<WG746><E>beginning<e><q> <Q><wg>εποίησεν ο θεός<WG4160><WG3588><WG2316><E> God made<e><q> <Q><wg>τον<WG3588><E> the<e><q> <Q><wg>ουρανόν<WG3772><E> heaven<e><q> <Q><wg>και<WG2532><E> and<e><q> <Q><wg>την<WG3588><E> the<e><q> <Q><wg>γην<WG1093><E> earth.<e><q>';
end;

procedure TForm1.LoadSample;
var
  lines: TStringList;
begin
  lines := TStringList.Create;
  try
    lines.LoadFromFile('sample.ont');
    FMemo.ONT:= lines.Text;
  finally
    lines.Free;
  end;
end;

end.

