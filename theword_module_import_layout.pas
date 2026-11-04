unit theword_module_import_layout;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, BibleScope;

const
  THEWORD_STAGED_IMPORT_BATCH_SIZE = 200;

type
  TTheWordModuleImportLayoutResolution = (
    twmilrResolved,
    twmilrInvalidModuleType,
    twmilrModuleDoesNotIncludeScope,
    twmilrInvalidProjectScope
  );

  TTheWordModuleImportLayout = record
    ModuleScope: TEscopoTexto;
    ProjectScope: TEscopoTexto;
    ModuleVerseCount: Integer;
    ExpectedVerseCount: Integer;
    ModuleLineOffset: Integer;
    ProjectVerseOffset: Integer;
  end;

function CreateTheWordModuleImportLayout(ModuleScope,
  ProjectScope: TEscopoTexto; ModuleVerseCount, ExpectedVerseCount,
  ModuleLineOffset, ProjectVerseOffset: Integer): TTheWordModuleImportLayout;
function IsValidTheWordModuleImportLayout(
  const Layout: TTheWordModuleImportLayout): Boolean;
function ResolveTheWordModuleImportLayout(const ModuleExtension: string;
  ProjectScope: TEscopoTexto; out Layout: TTheWordModuleImportLayout):
  TTheWordModuleImportLayoutResolution;
function FirstImportedModuleLineIndex(
  const Layout: TTheWordModuleImportLayout): Integer;
function LastImportedModuleLineIndex(
  const Layout: TTheWordModuleImportLayout): Integer;
function FirstModulePropertyLineIndex(
  const Layout: TTheWordModuleImportLayout): Integer;
function IsImportedVerseLine(const Layout: TTheWordModuleImportLayout;
  LineIndex: Integer): Boolean;

implementation

{$INCLUDE include/bookdefs.inc}

function CreateTheWordModuleImportLayout(ModuleScope,
  ProjectScope: TEscopoTexto; ModuleVerseCount, ExpectedVerseCount,
  ModuleLineOffset, ProjectVerseOffset: Integer): TTheWordModuleImportLayout;
begin
  Result := Default(TTheWordModuleImportLayout);
  Result.ModuleScope := ModuleScope;
  Result.ProjectScope := ProjectScope;
  Result.ModuleVerseCount := ModuleVerseCount;
  Result.ExpectedVerseCount := ExpectedVerseCount;
  Result.ModuleLineOffset := ModuleLineOffset;
  Result.ProjectVerseOffset := ProjectVerseOffset;
end;

function IsValidTheWordModuleImportLayout(
  const Layout: TTheWordModuleImportLayout): Boolean;
begin
  if (Layout.ModuleScope = etNone) or
     (Layout.ProjectScope = etNone) or
     (Layout.ModuleVerseCount <= 0) or
     (Layout.ExpectedVerseCount <= 0) or
     (Layout.ModuleLineOffset < 0) or
     (Layout.ProjectVerseOffset < 0) then
    Exit(False);

  Result :=
    (Layout.ModuleVerseCount <= QLinhas[Layout.ModuleScope]) and
    (Layout.ModuleLineOffset + Layout.ExpectedVerseCount <=
      Layout.ModuleVerseCount) and
    (Layout.ProjectVerseOffset + Layout.ExpectedVerseCount <=
      QLinhas[Layout.ProjectScope]);
end;

function ResolveTheWordModuleImportLayout(const ModuleExtension: string;
  ProjectScope: TEscopoTexto; out Layout: TTheWordModuleImportLayout):
  TTheWordModuleImportLayoutResolution;
var
  Extension: string;
  ModuleScope: TEscopoTexto;
  ExpectedVerseCount: Integer;
  ModuleLineOffset: Integer;
  ProjectVerseOffset: Integer;
begin
  Layout := Default(TTheWordModuleImportLayout);
  if ProjectScope = etNone then
    Exit(twmilrInvalidProjectScope);

  Extension := LowerCase(ModuleExtension);
  if Extension = '.ont' then
    ModuleScope := etONT
  else if Extension = '.ot' then
    ModuleScope := etOT
  else if Extension = '.nt' then
    ModuleScope := etNT
  else
    Exit(twmilrInvalidModuleType);

  if ((ModuleScope = etOT) and (ProjectScope = etNT)) or
     ((ModuleScope = etNT) and (ProjectScope = etOT)) then
    Exit(twmilrModuleDoesNotIncludeScope);

  ModuleLineOffset := 0;
  ProjectVerseOffset := 0;
  if (ModuleScope = etONT) and (ProjectScope = etOT) then
    ExpectedVerseCount := QLinhas[etOT]
  else if (ModuleScope = etONT) and (ProjectScope = etNT) then
  begin
    ExpectedVerseCount := QLinhas[etNT];
    ModuleLineOffset := QLinhas[etOT];
  end
  else
  begin
    ExpectedVerseCount := QLinhas[ModuleScope];
    if (ModuleScope = etNT) and (ProjectScope = etONT) then
      ProjectVerseOffset := QLinhas[etOT];
  end;

  Layout := CreateTheWordModuleImportLayout(ModuleScope, ProjectScope,
    QLinhas[ModuleScope], ExpectedVerseCount, ModuleLineOffset,
    ProjectVerseOffset);
  Result := twmilrResolved;
end;

function FirstImportedModuleLineIndex(
  const Layout: TTheWordModuleImportLayout): Integer;
begin
  Result := Layout.ModuleLineOffset;
end;

function LastImportedModuleLineIndex(
  const Layout: TTheWordModuleImportLayout): Integer;
begin
  Result := Layout.ModuleLineOffset + Layout.ExpectedVerseCount - 1;
end;

function FirstModulePropertyLineIndex(
  const Layout: TTheWordModuleImportLayout): Integer;
begin
  Result := Layout.ModuleVerseCount;
end;

function IsImportedVerseLine(const Layout: TTheWordModuleImportLayout;
  LineIndex: Integer): Boolean;
begin
  Result := (LineIndex >= FirstImportedModuleLineIndex(Layout)) and
    (LineIndex <= LastImportedModuleLineIndex(Layout));
end;

end.
