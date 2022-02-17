{ ******************************************************** }
{ **                                                    ** }
{ ** Basic multithreaded Log support for Delphi         ** }
{ **                                                    ** }
{ ** Author:                                            ** }
{ **     Juan Antonio Castillo H. (jachguate)           ** }
{ **                                                    ** }
{ ** Copyright (c) 2007-2021                            ** }
{ **                                                    ** }
{ ** https://github.com/jachguate/jachLogMgr            ** }
{ **                                                    ** }
{ ** Available under MIT License                        ** }
{ ******************************************************** }

{
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
}
unit ujachLogToDisk;

interface

{$define AutoRegisterjachLogToDisk}

uses
  UjachLogMgr, System.SyncObjs, ujachLogClasses;

type
  TjachLogToDisk = class(TjachLogWriter)
  private
    FBasePath: string;
    FFileNamePrefix: string;
    FFileNameSuffix: string;

    FLogFileName: string;
    FLogFile: TextFile;

    FIsOpen: Boolean;
    FLock: TCriticalSection;
    FMaxFileSize: UInt64;
    FIsMirroredToConsole: Boolean;
    FMaxLineSize: UInt16;
    procedure SetFileNamePrefix(const Value: string);
    procedure SetFileNameSuffix(const Value: string);
    procedure SetBasePath(const Value: string);
    procedure UpdateLogFileName;
    procedure SetMaxFileSize(const Value: UInt64);
    procedure SetIsMirroredToConsole(const Value: Boolean);
    procedure SetMaxLineSize(const Value: UInt16);
  public
    procedure OpenLogChannel; override;
    procedure CloseLogChannel; override;
    procedure Write(ATopic: TjachLogTopicIndex; ASeverity: TLogSeverity;
      const S, AIndentSpaces: string; const AThreadID: TThreadID;
      const ATimeStamp: TDateTime); override;
    procedure RotateLogs;
    function GetLock: TCriticalSection; override;
  public
    constructor Create;
    destructor Destroy; override;
    property BasePath: string read FBasePath write SetBasePath;
    property FileNamePrefix: string read FFileNamePrefix write SetFileNamePrefix;
    property FileNameSuffix: string read FFileNameSuffix write SetFileNameSuffix;
    property MaxFileSize: UInt64 read FMaxFileSize write SetMaxFileSize;
    property MaxLineSize: UInt16 read FMaxLineSize write SetMaxLineSize;
    property IsMirroredToConsole: Boolean read FIsMirroredToConsole write SetIsMirroredToConsole;
  end;

implementation

uses
    System.IOUtils
  {$ifdef MSWindows}
  , Windows
  {$endif}
  , System.SysUtils
  , System.Types;

function GetDefaultBasePath: string;
begin
  Result := TPath.GetPublicPath() + PathDelim + 'log';
end;

{$ifdef MSWindows}
function MyGetFileSize(const aFilename: String): Int64;
var
  info: TWin32FileAttributeData;
begin
  result := -1;

  if NOT GetFileAttributesEx(PWideChar(aFileName), GetFileExInfoStandard, @info) then
    EXIT;

  result := Int64(info.nFileSizeLow) or Int64(info.nFileSizeHigh shl 32);
end;
{$endif}

{ TjachLogToDisk }

procedure TjachLogToDisk.CloseLogChannel;
begin
  if not FIsOpen then
    Exit;

  CloseFile(FLogFile);
  FIsOpen := False;
end;

constructor TjachLogToDisk.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FIsMirroredToConsole := IsConsole;
  FMaxFileSize := 20 * 1024 * 1024; //20MB
  FMaxLineSize := 255;
  FBasePath := GetDefaultBasePath;
  UpdateLogFileName;
end;

destructor TjachLogToDisk.Destroy;
begin
  FLock.Free;
  if FIsOpen then
    CloseLogChannel;
  inherited;
end;

function TjachLogToDisk.GetLock: TCriticalSection;
begin
  Result := FLock;
end;

procedure TjachLogToDisk.OpenLogChannel;
begin
  if FIsOpen then
    Exit;
  AssignFile(FLogFile, FLogFileName);
  if FileExists(FLogFileName) then
  begin
    {$ifdef MSWindows}
    if MyGetFileSize(FLogFileName) >= FMaxFileSize then
    begin
      RotateLogs;
      ReWrite(FLogFile);
    end
    else
      Append(FLogFile);
    {$else}
    Append(FLogFile);
    {$endif}
  end
  else begin
    ForceDirectories(ExtractFilePath(FLogFileName));
    ReWrite(FLogFile);
  end;
  FIsOpen := True;
end;

procedure TjachLogToDisk.RotateLogs;
var
  Path, FN, Ext: string;

  function CalcFileName(Idx: Integer): string;
  begin
    Result := TPath.Combine(Path, FN + '.' + IntToStr(Idx) + Ext);
  end;

var
  FileNames: array[0..5] of string;
  I: Integer;
begin
  Path := ExtractFilePath(FLogFileName);
  FN := ExtractFileName(FLogFileName);
  Ext := ExtractFileExt(FLogFileName);
  Delete(FN, Length(FN) - Length(Ext) + 1, Length(Ext));
  FileNames[0] := FLogFileName;
  for I := Low(FileNames) + 1 to High(FileNames) do
    FileNames[I] := CalcFileName(I);
  if TFile.Exists(FileNames[High(FileNames)], False) then
    TFile.Delete(FileNames[High(FileNames)]);
  for I := High(FileNames) - 1 downto Low(FileNames) do
    if TFile.Exists(FileNames[I]) then
      TFile.Move(FileNames[I], FileNames[I + 1]);
end;

procedure TjachLogToDisk.SetBasePath(const Value: string);
begin
  FBasePath := Value;
  UpdateLogFileName;
end;

procedure TjachLogToDisk.SetFileNamePrefix(const Value: string);
begin
  FFileNamePrefix := Value;
  UpdateLogFileName;
end;

procedure TjachLogToDisk.SetFileNameSuffix(const Value: string);
begin
  FFileNameSuffix := Value;
  UpdateLogFileName;
end;

procedure TjachLogToDisk.SetIsMirroredToConsole(const Value: Boolean);
begin
  FIsMirroredToConsole := Value;
end;

procedure TjachLogToDisk.SetMaxFileSize(const Value: UInt64);
begin
  FMaxFileSize := Value;
end;

procedure TjachLogToDisk.SetMaxLineSize(const Value: UInt16);
begin
  FMaxLineSize := Value;
end;

procedure TjachLogToDisk.UpdateLogFileName;
var
  Temp: string;
begin
  Temp := TPath.ChangeExtension(TPath.GetFileName(ParamStr(0)), '.log');
  Temp := FFileNamePrefix + Copy(Temp, 1, Length(Temp) - 4) + FFileNameSuffix + '.log';
  FLogFileName := TPath.Combine(FBasePath, Temp);
end;

procedure TjachLogToDisk.Write(ATopic: TjachLogTopicIndex;
  ASeverity: TLogSeverity; const S, AIndentSpaces: string;
  const AThreadID: TThreadID; const ATimeStamp: TDateTime);
var
  DT: string;
  Margin: string;
  Msgs: TStringDynArray;
  I: Integer;
begin
  DT := Format('%s %.8x %-5s', [FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', ATimeStamp)
    , AThreadID
    , LogSeverityToStr(ASeverity)]);
  Margin := StringOfChar(' ', Length(DT));
  Msgs := WordWrap(S, FMaxLineSize);
  Writeln(FLogFile, DT + ' ' + AIndentSpaces + Msgs[0]);
  if IsConsole and FIsMirroredToConsole then
    Writeln(DT + ' ' + AIndentSpaces + Msgs[0]);
  for I := 1 to High(Msgs) do
  begin
    Writeln(FLogFile, Margin + ' ' + AIndentSpaces + Msgs[I]);
    if IsConsole and FIsMirroredToConsole then
      Writeln(Margin + ' ' + AIndentSpaces + Msgs[I]);
  end;
end;

initialization
finalization
end.
