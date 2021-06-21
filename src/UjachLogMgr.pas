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


{$define AutoRegisterDiskLogger}

unit UjachLogMgr;

interface
uses Classes, SysUtils, System.Types, SyncObjs, System.Generics.Collections;

type
  TObjectReferenceToLoggerProc = record
    AObject: TObject;
    ALoggerProcIndex: Integer;
  end;

  TLogType = (ltDebug, ltInfo, ltMessage, ltWarn, ltError, ltFatalError);

  //TLoggerProcedure = TProc<TLogType, string>;
  TLoggerProcedure = reference to procedure (ALogType: TLogType; const S: string);

  TjachLog = record
  private
    class var FCS: TCriticalSection;
    class var FRegisteredLoggers: TList<TLoggerProcedure>;
    class var FRegisteredObjects: TList<TObjectReferenceToLoggerProc>;
    class var FFileSuffix: string;
    class var IndentSpaces: string;
    class procedure RotateLogs(LogFileName: string); static;
    class function GetExceptionStr(E: Exception): string; static;
    class procedure DiskLogger(ALogType: TLogType; const S: string); static;
  public
    class var IsActive: Boolean;
    class var MaxFileSize: Integer;

    class procedure IncIndent; static;
    class procedure DecIndent; static;
    class function RegisterLoggerProc(AProc: TLoggerProcedure): Integer; static;
    class procedure RegisterDiskLoggerProc; static;
    class function GetIndentSpaces: string; static; inline;
    class procedure RegisterObjectLoggerProc(AProc: TLoggerProcedure; AObject: TObject); static;
    class procedure UnregisterObjectLoggerProc(AObject: TObject); static;
    class procedure SetFileSuffix(AFileSuffix: string); static;

    class procedure Log(ALogType: TLogType; const S: string); static;
    class procedure LogDebug(const S: string); overload; static;
    class procedure LogDebug(const S: string; const Args: array of const); overload; static;
    class procedure LogInfo(const S: string); overload; static;
    class procedure LogInfo(const S: string; const Args: array of const); overload; static;
    class procedure LogMessage(const S: string); overload; static;
    class procedure LogMessage(const S: string; const Args: array of const); overload; static;
    class procedure LogWarn(const S: string); overload; static;
    class procedure LogWarn(const S: string; const Args: array of const); overload; static;

    class procedure LogError(const S: string); overload; static;
    class procedure LogError(E: Exception; const ExtraMsg: string); overload; static;
    class procedure LogError(E: Exception; const S: string; const Args: array of const); overload; static;
    class procedure LogError(E: Exception); overload; static;

    class procedure LogFatalError(const S: string); overload; static;
    class procedure LogFatalError(E: Exception; const ExtraMsg: string); overload; static;
    class procedure LogFatalError(E: Exception; const S: string; const Args: array of const); overload; static;
    class procedure LogFatalError(E: Exception); overload; static;
    class procedure ForceLog(Proc: TProc); static;
    class procedure ForceLogMessage(const S: string); overload; static;
    class procedure ForceLogMessage(const S: string; const Args: array of const); overload; static;
    class procedure ForceLogError(E: Exception); overload; static;
    class procedure ForceLogError(E: Exception; const ExtraMsg: string); overload; static;
    class procedure ForceLogError(E: Exception; const S: string; const Args: array of const); overload; static;
  end;

function LogTypeToStr(ALogType: TLogType): string;

const
  WWMAX_LEN = 255;

  function WordWrap(const S: string; MaxLen: Integer = WWMAX_LEN): TStringDynArray;

implementation
uses StrUtils,
{$ifdef MSWindows}
Windows,
{$endif}
{$ifdef HAS_EXCEPTION_STACKTRACE}
JclDebug,
{$endif HAS_EXCEPTION_STACKTRACE}
System.IOUtils;


{ TjachLog }

function WordWrap(const S: string; MaxLen: Integer = WWMAX_LEN): TStringDynArray;
const
  CR = #13;
  LF = #10;
var
  Start, Idx, PosSpace, CharsToIgnore: Integer;
  CrLf: string;
begin
  CrLf := CR + LF;
  SetLength(Result, 1);
  if     (Length(S) <= MaxLen)
     and (Pos(CrLf, S) = 0)
     and (Pos(CR, S) = 0)
     and (Pos(LF, S) = 0) then
    Result[0] := S
  else begin
    Start := 1;
    Idx := 0;
    repeat
      CharsToIgnore := 0;
      SetLength(Result, Idx + 1);
      Result[Idx] := Copy(S, Start, MaxLen);
      if (Start + Length(Result[Idx])) < Length(S) then
      begin
        PosSpace := LastDelimiter(' ', Result[Idx]);
        if PosSpace > 0 then
        begin
          Delete(Result[Idx], PosSpace, MaxInt);
          CharsToIgnore := 1;
        end;
      end;
      PosSpace := Pos(CRLF, Result[Idx]);
      if PosSpace > 0 then
      begin
        Delete(Result[Idx], PosSpace, MaxInt);
        CharsToIgnore := 2;
      end;

      PosSpace := Pos(CR, Result[Idx]);
      if PosSpace > 0 then
      begin
        Delete(Result[Idx], PosSpace, MaxInt);
        CharsToIgnore := 1;
      end;
      PosSpace := Pos(LF, Result[Idx]);
      if PosSpace > 0 then
      begin
        Delete(Result[Idx], PosSpace, MaxInt);
        CharsToIgnore := 1;
      end;

      Start := Start + Length(Result[Idx]) + CharsToIgnore;
      Inc(Idx);
    until Start >= Length(S);
  end;
end;

function LogTypeToStr(ALogType: TLogType): string;
begin
  case ALogType of
    ltDebug: Result := 'Dbg';
    ltInfo: Result := 'Info';
    ltMessage: Result := 'Msg';
    ltWarn: Result := 'Warn';
    ltError: Result := 'Err';
    ltFatalError: Result := 'Fatal';
  end;
end;

function PathBase: string;
  {$if defined(debug) or defined(dbglog)}
  const ThePathBase: string = 'c:\log';
  {$endif}
begin
  {$if defined(debug) or defined(dbglog)}
    Result := ThePathBase;
  {$else}
    Result := ExtractFilePath(ExpandFileName(ParamStr(0))) + PathDelim + 'log';
  {$endif}
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

class procedure TjachLog.RegisterDiskLoggerProc;
begin
  FRegisteredLoggers.Add(DiskLogger);
end;

class procedure TjachLog.DecIndent;
begin
  Delete(IndentSpaces, 1, 2);
end;

class procedure TjachLog.DiskLogger(ALogType: TLogType; const S: string);
var
  LogFileName: string;
  Log: TextFile;
  DT: string;
  Margen: string;
  Msgs: TStringDynArray;
  I: Integer;
  Retries: Integer;
begin
  if not IsActive then
    Exit;
  try
    DT := Format('%s %.8x %-5s', [FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now)
      , GetCurrentThreadID
      , LogTypeToStr(ALogType)]);
    Margen := StringOfChar(' ', Length(DT));
    LogFileName := TPath.ChangeExtension(TPath.GetFileName(ParamStr(0)), '.log');
    LogFileName := TPath.Combine(PathBase, LogFileName);
    if FFileSuffix <> '' then
      LogFileName := Copy(LogFileName, 1, Length(LogFileName) - 4) + FFileSuffix + '.log';
    AssignFile(Log, LogFileName);
    Msgs := WordWrap(S);
    Retries := 0;
    while true do
    begin
      if FCS.TryEnter then
        try
          if FileExists(LogFileName) then
          begin
            {$ifdef MSWindows}
            if MyGetFileSize(LogFileName) >= MaxFileSize then
            begin
              RotateLogs(LogFileName);
              ReWrite(Log);
            end
            else
              Append(Log);
            {$else}
            Append(Log);
            {$endif}
          end
          else begin
            ForceDirectories(ExtractFilePath(LogFileName));
            ReWrite(Log);
          end;
          try
            Writeln(Log, DT + ' ' + GetIndentSpaces() + Msgs[0]);
            if IsConsole then
              Writeln(DT + ' ' + GetIndentSpaces() + Msgs[0]);
            for I := 1 to High(Msgs) do
            begin
              Writeln(Log, Margen + ' ' + GetIndentSpaces() + Msgs[I]);
              if IsConsole then
                Writeln(Margen + ' ' + GetIndentSpaces() + Msgs[I]);
            end;
      //      Writeln(Log, '');
          finally
            CloseFile(Log);
          end;
          Exit;
        finally
          FCS.Leave;
        end
      else
      begin
        Inc(Retries);
        if Retries = 10 then
          Exit;
        Sleep(10);
      end;
    end;
  except
    //no exceptions!
    ;
  end;
end;

class procedure TjachLog.ForceLog(Proc: TProc);
var
  WasActive: Boolean;
begin
  WasActive := IsActive;
  try
    IsActive := True;
    Proc();
  finally
    IsActive := WasActive;
  end;
end;

class procedure TjachLog.ForceLogError(E: Exception);
var
  WasActive: Boolean;
begin
  WasActive := IsActive;
  try
    IsActive := True;
    LogError(E);
  finally
    IsActive := WasActive;
  end;
end;

class procedure TjachLog.ForceLogError(E: Exception; const ExtraMsg: string);
var
  WasActive: Boolean;
begin
  WasActive := IsActive;
  try
    IsActive := True;
    LogError(E, ExtraMsg);
  finally
    IsActive := WasActive;
  end;
end;

class procedure TjachLog.ForceLogError(E: Exception; const S: string;
  const Args: array of const);
var
  WasActive: Boolean;
begin
  WasActive := IsActive;
  try
    IsActive := True;
    LogError(E, S, Args);
  finally
    IsActive := WasActive;
  end;
end;

class procedure TjachLog.ForceLogMessage(const S: string;
  const Args: array of const);
var
  WasActive: Boolean;
begin
  WasActive := IsActive;
  try
    IsActive := True;
    LogMessage(S, Args);
  finally
    IsActive := WasActive;
  end;
end;

class procedure TjachLog.ForceLogMessage(const S: string);
var
  WasActive: Boolean;
begin
  WasActive := IsActive;
  try
    IsActive := True;
    LogMessage(S);
  finally
    IsActive := WasActive;
  end;
end;

class function TjachLog.GetExceptionStr(E: Exception): string;
begin
  Result := Format('%s'#13'%s', [E.ClassName, E.Message]);
  {$ifdef HAS_EXCEPTION_STACKTRACE}
  Result := Result + #13 + E.StackTrace;
  {$endif HAS_EXCEPTION_STACKTRACE}
end;

class function TjachLog.GetIndentSpaces: string;
begin
  Result := IndentSpaces;
end;

class procedure TjachLog.IncIndent;
begin
  IndentSpaces := IndentSpaces + '  ';
end;

class procedure TjachLog.Log(ALogType: TLogType; const S: string);
var
  LP: TLoggerProcedure;
begin
  if not IsActive then
    Exit;
  for LP in FRegisteredLoggers do
    try
      LP(ALogType, S);
    except
      ;
    end;
end;

class procedure TjachLog.LogError(const S: string);
begin
  Log(ltError, S);
end;

class procedure TjachLog.LogError(E: Exception; const ExtraMsg: string);
begin
  Log(ltError, Format('%s'#13'%s', [ExtraMsg, GetExceptionStr(E)]));
end;

class procedure TjachLog.LogDebug(const S: string);
begin
  {$if defined(debug) or defined(dbglog)}
  Log(ltDebug, S);
  {$endif}
end;

class procedure TjachLog.LogDebug(const S: string;
  const Args: array of const);
begin
  {$if defined(debug) or defined(dbglog)}
  LogDebug(Format(S, Args));
  {$endif}
end;

class procedure TjachLog.LogError(E: Exception);
begin
  Log(ltError, GetExceptionStr(E));
end;

class procedure TjachLog.LogError(E: Exception; const S: string;
  const Args: array of const);
begin
  LogError(E, Format(S, Args));
end;

class procedure TjachLog.LogFatalError(const S: string);
begin
  Log(ltFatalError, S);
end;

class procedure TjachLog.LogFatalError(E: Exception; const ExtraMsg: string);
begin
  Log(ltFatalError, Format('%s'#13'%s', [ExtraMsg, GetExceptionStr(E)]));
end;

class procedure TjachLog.LogFatalError(E: Exception);
begin
  Log(ltFatalError, GetExceptionStr(E));
end;

class procedure TjachLog.LogFatalError(E: Exception; const S: string;
  const Args: array of const);
begin
  LogFatalError(E, Format(S, Args));
end;

class procedure TjachLog.LogInfo(const S: string;
  const Args: array of const);
begin
  Log(ltInfo, Format(S, Args));
end;

class procedure TjachLog.LogInfo(const S: string);
begin
  Log(ltInfo, S);
end;

class procedure TjachLog.LogMessage(const S: string);
begin
  Log(ltMessage, S);
end;

class procedure TjachLog.LogMessage(const S: string;
  const Args: array of const);
begin
  Log(ltMessage, Format(S, Args));
end;

class procedure TjachLog.LogWarn(const S: string);
begin
  Log(ltWarn, S);
end;

class procedure TjachLog.LogWarn(const S: string;
  const Args: array of const);
begin
  Log(ltWarn, Format(S, Args));
end;

class function TjachLog.RegisterLoggerProc(AProc: TLoggerProcedure): Integer;
begin
  Result := FRegisteredLoggers.Add(AProc);
end;

class procedure TjachLog.RegisterObjectLoggerProc(AProc: TLoggerProcedure;
  AObject: TObject);
var
  Ref: TObjectReferenceToLoggerProc;
begin
  Ref.AObject := AObject;
  Ref.ALoggerProcIndex := RegisterLoggerProc(AProc);
  FRegisteredObjects.Add(Ref);
end;

class procedure TjachLog.RotateLogs(LogFileName: string);
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
  Path := ExtractFilePath(LogFileName);
  FN := ExtractFileName(LogFileName);
  Ext := ExtractFileExt(LogFileName);
  Delete(FN, Length(FN) - Length(Ext) + 1, Length(Ext));
  FileNames[0] := LogFileName;
  for I := Low(FileNames) + 1 to High(FileNames) do
    FileNames[I] := CalcFileName(I);
  if TFile.Exists(FileNames[High(FileNames)], False) then
    TFile.Delete(FileNames[High(FileNames)]);
  for I := High(FileNames) - 1 downto Low(FileNames) do
    if TFile.Exists(FileNames[I]) then
      TFile.Move(FileNames[I], FileNames[I + 1]);
end;

class procedure TjachLog.SetFileSuffix(AFileSuffix: string);
begin
  FFileSuffix := AFileSuffix;
end;

class procedure TjachLog.UnregisterObjectLoggerProc(AObject: TObject);
  procedure UpdateIndex(IndexToDelete: Integer);
  var
    Ref: TObjectReferenceToLoggerProc;
    I: Integer;
  begin
    for I := 0 to FRegisteredObjects.Count - 1 do
    begin
      Ref := FRegisteredObjects[I];
      if Ref.ALoggerProcIndex > IndexToDelete then
      begin
        Ref.ALoggerProcIndex := Ref.ALoggerProcIndex - 1;
        FRegisteredObjects[I] := Ref;
      end;
    end;
  end;
var
  I: Integer;
begin
  for I := FRegisteredObjects.Count - 1 downto 0 do
  begin
    if FRegisteredObjects[I].AObject = AObject then
    begin
      FRegisteredLoggers.Delete(FRegisteredObjects[I].ALoggerProcIndex);
      UpdateIndex(FRegisteredObjects[I].ALoggerProcIndex);
      FRegisteredObjects.Delete(I);
    end;
  end;
end;

initialization
  TjachLog.MaxFileSize := 20 * 1024 * 1024; //20MB
  TjachLog.FCS := TCriticalSection.Create;
  TjachLog.FRegisteredLoggers := TList<TLoggerProcedure>.Create();
  TjachLog.FRegisteredObjects := TList<TObjectReferenceToLoggerProc>.Create();

  {$ifdef AutoRegisterDiskLogger}
  TjachLog.RegisterDiskLoggerProc;
  {$endif}
finalization
  TjachLog.FCS.Free;
  TjachLog.FRegisteredLoggers.Free;
  TjachLog.FRegisteredObjects.Free;
end.
