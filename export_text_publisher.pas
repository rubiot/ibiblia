unit export_text_publisher;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

type
  TExportCopyPhase = (ecpBackup, ecpReplacement, ecpRestore);

  TExportTextPublisher = class
  private
    FBackupFileName: string;
  protected
    procedure CopyContents(Source, Destination: TStream;
      Phase: TExportCopyPhase); virtual;
  public
    procedure Publish(const StagingFileName, FinalFileName: string);
    property BackupFileName: string read FBackupFileName;
  end;

implementation

{$IFDEF WINDOWS}
uses Windows;
{$ELSE}
uses BaseUnix, Unix;
{$ENDIF}

resourcestring
  STextExportRestoreFailed = 'Could not update %s or restore its previous contents. The previous module is preserved at %s. Write error: %s. Restore error: %s';

procedure TExportTextPublisher.CopyContents(Source, Destination: TStream;
  Phase: TExportCopyPhase);
begin
  Source.Position := 0;
  Destination.Position := 0;
  Destination.CopyFrom(Source, Source.Size);
  if Destination.Size <> Source.Size then
    Destination.Size := Source.Size;
end;

procedure TExportTextPublisher.Publish(const StagingFileName, FinalFileName: string);
{$IFDEF WINDOWS}
var
  SourceHandle, DestinationHandle, BackupHandle: THandle;
  Source, Destination, Backup: THandleStream;
  Guid: TGUID;
  RemoveBackup: Boolean;
  WriteError: string;

  procedure Flush(Handle: THandle);
  begin
    if not FlushFileBuffers(Handle) then RaiseLastOSError;
  end;

begin
  FBackupFileName := '';
  SourceHandle := INVALID_HANDLE_VALUE;
  DestinationHandle := INVALID_HANDLE_VALUE;
  BackupHandle := INVALID_HANDLE_VALUE;
  Source := nil;
  Destination := nil;
  Backup := nil;
  RemoveBackup := False;
  try
    SourceHandle := CreateFileW(PWideChar(UTF8Decode(StagingFileName)),
      GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if SourceHandle = INVALID_HANDLE_VALUE then RaiseLastOSError;
    Source := THandleStream.Create(SourceHandle);

    // Keep the original file identity used by theWord. Denying write/delete
    // sharing also prevents another writer changing the backup or rollback.
    DestinationHandle := CreateFileW(PWideChar(UTF8Decode(FinalFileName)),
      GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ, nil, OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL, 0);
    if DestinationHandle = INVALID_HANDLE_VALUE then RaiseLastOSError;
    Destination := THandleStream.Create(DestinationHandle);

    if CreateGUID(Guid) <> 0 then
      raise Exception.Create('Could not create an export backup name');
    FBackupFileName := FinalFileName + '.export-backup-' +
      Copy(GUIDToString(Guid), 2, 36);
    BackupHandle := CreateFileW(PWideChar(UTF8Decode(FBackupFileName)),
      GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ, nil, CREATE_NEW,
      FILE_ATTRIBUTE_NORMAL, 0);
    if BackupHandle = INVALID_HANDLE_VALUE then
    begin
      FBackupFileName := '';
      RaiseLastOSError;
    end;
    Backup := THandleStream.Create(BackupHandle);
    RemoveBackup := True;
    CopyContents(Destination, Backup, ecpBackup);
    Flush(BackupHandle);

    // From this point a crash must leave a durable recovery copy on disk.
    RemoveBackup := False;
    try
      CopyContents(Source, Destination, ecpReplacement);
      Flush(DestinationHandle);
      RemoveBackup := True;
    except
      on E: Exception do
      begin
        WriteError := E.Message;
        try
          CopyContents(Backup, Destination, ecpRestore);
          Flush(DestinationHandle);
          RemoveBackup := True;
        except
          on RestoreError: Exception do
            raise Exception.CreateFmt(STextExportRestoreFailed,
              [FinalFileName, FBackupFileName, WriteError, RestoreError.Message]);
        end;
        raise;
      end;
    end;
  finally
    Backup.Free;
    Destination.Free;
    Source.Free;
    if BackupHandle <> INVALID_HANDLE_VALUE then CloseHandle(BackupHandle);
    if DestinationHandle <> INVALID_HANDLE_VALUE then CloseHandle(DestinationHandle);
    if SourceHandle <> INVALID_HANDLE_VALUE then CloseHandle(SourceHandle);
    if RemoveBackup and (FBackupFileName <> '') and SysUtils.DeleteFile(FBackupFileName) then
      FBackupFileName := '';
  end;
end;
{$ELSE}
var
  Source, Destination, Backup: TFileStream;
  Guid: TGUID;
  RemoveBackup: Boolean;
  WriteError: string;
begin
  FBackupFileName := '';
  Source := nil;
  Destination := nil;
  Backup := nil;
  RemoveBackup := False;
  try
    Source := TFileStream.Create(StagingFileName, fmOpenRead or fmShareDenyNone);
    Destination := TFileStream.Create(FinalFileName, fmOpenReadWrite or fmShareDenyWrite);
    if CreateGUID(Guid) <> 0 then
      raise Exception.Create('Could not create an export backup name');
    FBackupFileName := FinalFileName + '.export-backup-' +
      Copy(GUIDToString(Guid), 2, 36);
    Backup := TFileStream.Create(FBackupFileName, fmCreate or fmShareExclusive);
    RemoveBackup := True;
    CopyContents(Destination, Backup, ecpBackup);
    fpfsync(Backup.Handle);

    RemoveBackup := False;
    try
      CopyContents(Source, Destination, ecpReplacement);
      fpfsync(Destination.Handle);
      RemoveBackup := True;
    except
      on E: Exception do
      begin
        WriteError := E.Message;
        try
          CopyContents(Backup, Destination, ecpRestore);
          fpfsync(Destination.Handle);
          RemoveBackup := True;
        except
          on RestoreError: Exception do
            raise Exception.CreateFmt(STextExportRestoreFailed,
              [FinalFileName, FBackupFileName, WriteError, RestoreError.Message]);
        end;
        raise;
      end;
    end;
  finally
    Backup.Free;
    Destination.Free;
    Source.Free;
    if RemoveBackup and (FBackupFileName <> '') and SysUtils.DeleteFile(FBackupFileName) then
      FBackupFileName := '';
  end;
end;
{$ENDIF}

end.
