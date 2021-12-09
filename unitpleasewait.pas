unit UnitPleaseWait;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls;

type

  { TFormPleaseWait }

  TFormPleaseWait = class(TForm)
    Label1: TLabel;
    ProgressBar1: TProgressBar;
  private

  public

  end;

var
  FormPleaseWait: TFormPleaseWait;

implementation

initialization
  {$I unitpleasewait.lrs}

end.

