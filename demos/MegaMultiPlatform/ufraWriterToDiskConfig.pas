unit ufraWriterToDiskConfig;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ufraWriterConfig, FMX.ListBox, FMX.Controls.Presentation, FMX.Layouts,
  ujachLogMgr, ujachLogToDisk, FMX.Edit;

type
  TfraWriterToDiskConfig = class(TfraWriterConfig)
    pnlPath: TPanel;
    pnlPrefix: TPanel;
    pnlSuffix: TPanel;
    pnlMaxFileSize: TPanel;
    lblPath: TLabel;
    lblPrefix: TLabel;
    lblSufix: TLabel;
    lblMaxFileSize: TLabel;
    edtPath: TEdit;
    edtPrefix: TEdit;
    edtSuffix: TEdit;
    edtMaxFileSize: TEdit;
    pnlMaxLineSize: TPanel;
    lblMaxLineSize: TLabel;
    edtMaxLineSize: TEdit;
    pnlFileCountToKeepInRotation: TPanel;
    lblFileCountToKeepInRotation: TLabel;
    edtFileCountToKeepInRotation: TEdit;
    btnPathVisit: TButton;
    procedure btnPathVisitClick(Sender: TObject);
    procedure edtPathValidate(Sender: TObject; var Text: string);
    procedure edtPrefixValidate(Sender: TObject; var Text: string);
    procedure edtSuffixValidate(Sender: TObject; var Text: string);
    procedure edtMaxFileSizeValidate(Sender: TObject; var Text: string);
    procedure edtMaxLineSizeValidate(Sender: TObject; var Text: string);
    procedure edtFileCountToKeepInRotationValidate(Sender: TObject;
      var Text: string);
  private
    FWriter2: TjachLogToDisk;
    function BytesToString(ABytes: UInt64): string;
    function StringToBytes(AStrBytes: string): UInt64;
  public
    constructor Create(AOwner: TComponent; AWriter: TjachLogWriter); override;
  end;

implementation

{$ifdef MSWindows}
uses
  Winapi.ShellAPI, Winapi.Windows, System.StrUtils, System.Character,
  System.IOUtils;
{$endif}

{$R *.fmx}

{ TfraWriterToDiskConfig }

procedure TfraWriterToDiskConfig.btnPathVisitClick(Sender: TObject);
begin
  inherited;
  {$ifdef MSWindows}
  Winapi.ShellAPI.ShellExecute(0, 'open', PChar(edtPath.Text), nil, nil, SW_SHOW);
  {$endif}
end;

function TfraWriterToDiskConfig.BytesToString(ABytes: UInt64): string;
var
  Idx: Integer;
  AValue: UInt64;
const
  Suffixes = ' KMGP';
begin
  AValue := ABytes;
  Idx := 0;
  while     (AValue > 0)
        and (AValue mod 1024 = 0)
        and (Idx < length(Suffixes))
  do
  begin
    Inc(Idx);
    AValue := AValue div 1024;
  end;

  Result := Trim(AValue.ToString + Suffixes.Chars[Idx]);
end;

constructor TfraWriterToDiskConfig.Create(AOwner: TComponent;
  AWriter: TjachLogWriter);
begin
  inherited;
  FWriter2 := AWriter as TjachLogToDisk;
  edtPath.Text := FWriter2.BasePath;
  edtPrefix.Text := FWriter2.FileNamePrefix;
  edtSuffix.Text := FWriter2.FileNameSuffix;
  edtMaxFileSize.Text := BytesToString(FWriter2.MaxFileSize);
  edtMaxLineSize.Text := FWriter2.MaxLineSize.ToString;
  edtFileCountToKeepInRotation.Text := FWriter2.FileCountToKeepInRotation.ToString;
  {$ifdef MSWindows}
  btnPathVisit.Visible := True;
  {$else}
  btnPathVisit.Visible := False;
  edtPath.Width := edtPath.Width + btnPathVisit.Width;
  {$endif}
end;

procedure TfraWriterToDiskConfig.edtFileCountToKeepInRotationValidate(
  Sender: TObject; var Text: string);
var
  Count: UInt64;
begin
  inherited;
  Count := StrToIntDef(Text.Trim, 0);
  if Count > 0 then
    FWriter2.FileCountToKeepInRotation := Count;
  Text := FWriter2.FileCountToKeepInRotation.ToString;
end;

procedure TfraWriterToDiskConfig.edtMaxFileSizeValidate(Sender: TObject;
  var Text: string);
var
  Size: UInt64;
begin
  inherited;
  Size := StringToBytes(Text.Trim);
  if Size > 0 then
    FWriter2.MaxFileSize := Size;
  Text := BytesToString(FWriter2.MaxFileSize);
end;

procedure TfraWriterToDiskConfig.edtMaxLineSizeValidate(Sender: TObject;
  var Text: string);
var
  Size: Integer;
begin
  inherited;
  Size := StrToIntDef(Text.Trim, 0);
  if Size > 0 then
    FWriter2.MaxLineSize := Size;
  Text := FWriter2.MaxLineSize.ToString;
end;

procedure TfraWriterToDiskConfig.edtPathValidate(Sender: TObject;
  var Text: string);
var
  Dir: string;
begin
  inherited;
  Dir := TPath.GetFullPath(Trim(Text));
  if TPath.HasValidPathChars(Dir, False) then
  begin
    if not TDirectory.Exists(Dir) then
      TDirectory.CreateDirectory(Dir);
    if TDirectory.Exists(Dir) then
      FWriter2.BasePath := Dir;
  end;
  Text := FWriter2.BasePath;
end;

procedure TfraWriterToDiskConfig.edtPrefixValidate(Sender: TObject;
  var Text: string);
var
  Pfx: string;
begin
  inherited;
  Pfx := Text.Trim;
  if TPath.HasValidFileNameChars(Pfx, False) then
    FWriter2.FileNamePrefix := Pfx;
  Text := FWriter2.FileNamePrefix;
end;

procedure TfraWriterToDiskConfig.edtSuffixValidate(Sender: TObject;
  var Text: string);
var
  Sfx: string;
begin
  inherited;
  Sfx := Text.Trim;
  if TPath.HasValidFileNameChars(Sfx, False) then
    FWriter2.FileNameSuffix := Sfx;
  Text := FWriter2.FileNameSuffix;
end;

function TfraWriterToDiskConfig.StringToBytes(AStrBytes: string): UInt64;
var
  Suffix: Char;
  Str: string;
  I: Integer;
  Position: Integer;
const
  Suffixes = 'KMGP';
begin
  {$ifdef nextgen}
    {$message warn 'this code is not yet tested under NEXTGEN compiler, please test and adjust before use'}
  {$endif}
  Str := '';
  for I := 0 to AStrBytes.Length - 1 do
    if not AStrBytes.Chars[I].IsWhiteSpace then
      Str := Str + AStrBytes.Chars[I];
  if Str.Length = 0 then
    Exit(0);
  Suffix := Str.Chars[Str.Length - 1];
  if Suffix.IsDigit then
  begin
    Result := StrToUInt64Def(Str, 0);
  end
  else
  begin
    Delete(Str, Length(Str), 1);
    Result := StrToUInt64Def(Str, 0);
    Position := Pos(Suffix, Suffixes);
    case Position of
      1: Result := Result * 1024;
      2: Result := Result * 1024*1024;
      3: Result := Result * 1024*1024*1024;
      4: Result := Result * 1024*1024*1024;
      5: Result := Result * 1024*1024*1024*1024;
    end;
  end;
end;

end.
