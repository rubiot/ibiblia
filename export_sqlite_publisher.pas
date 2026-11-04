unit export_sqlite_publisher;

{$mode objfpc}{$H+}

interface

uses SysUtils;

type
  ESQLiteExportPublishError = class(Exception)
  private
    FSQLiteCode: Integer;
    FDestinationInvalid: Boolean;
  public
    constructor Create(const MessageText: string; ACode: Integer);
    property SQLiteCode: Integer read FSQLiteCode;
    property DestinationInvalid: Boolean read FDestinationInvalid;
  end;

// Both paths must exist. The caller retains ownership of the staging file.
procedure PublishExistingSQLiteExport(const StagingFileName,
  FinalFileName: string; BusyTimeoutMS: Integer = 250);

implementation

uses sqlite_database_copy, sqlite3dyn;

resourcestring
  SSQLiteExportBusy = 'The exported module is locked by another application. Its previous contents were preserved. Close the active module view and retry: %s';

constructor ESQLiteExportPublishError.Create(const MessageText: string;
  ACode: Integer);
begin
  inherited Create(MessageText);
  FSQLiteCode := ACode;
end;

procedure PublishExistingSQLiteExport(const StagingFileName,
  FinalFileName: string; BusyTimeoutMS: Integer);
var
  ExportError: ESQLiteExportPublishError;
  MessageText: string;
begin
  try
    CopySQLiteDatabase(StagingFileName, FinalFileName, True, BusyTimeoutMS);
  except
    on E: ESQLiteDatabaseCopyError do
    begin
      MessageText := E.Message;
      if (E.SQLiteCode and $FF) in [SQLITE_BUSY, SQLITE_LOCKED] then
        MessageText := Format(SSQLiteExportBusy, [ExpandFileName(FinalFileName)]);
      ExportError := ESQLiteExportPublishError.Create(MessageText, E.SQLiteCode);
      ExportError.FDestinationInvalid := E.DestinationInvalid;
      raise ExportError;
    end;
  end;
end;

end.