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

unit UjachLogMgr;

interface
uses Classes, SysUtils, System.Types, SyncObjs, System.Generics.Collections;

type
  TLogType = (ltDebug, ltInfo, ltMessage, ltWarn, ltError, ltFatalError);

  TjachLogWriter = class
  protected
    procedure OpenLogChannel; virtual; abstract;
    procedure CloseLogChannel; virtual; abstract;
    procedure Write(ALogType: TLogType; const S, AIndentSpaces: string;
      const AThreadID: TThreadID; const ADateTime: TDateTime); virtual; abstract;
    function GetLock: TCriticalSection; virtual; abstract;
  public
    property Lock: TCriticalSection read GetLock;
  end;


  TjachLog = record
  private
    class var FCS: TCriticalSection;
    class var FRegisteredLoggers: TObjectList<TjachLogWriter>;
    class var FIndentSpaces: string;
    class var FCacheCS: TCriticalSection;
    class function GetExceptionStr(E: Exception): string; static;
  public
    class var IsActive: Boolean;
    class procedure IncIndent; static;
    class procedure DecIndent; static;

    class procedure RegisterLogger(ALogger: TjachLogWriter); static;
    class function GetIndentSpaces: string; static; inline;

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

    class procedure CacheLog(ALogType: TLogType; const S: string); static;

    class procedure CacheLogDebug(const S: string); overload; static;
    class procedure CacheLogDebug(const S: string; const Args: array of const); overload; static;
    class procedure CacheLogInfo(const S: string); overload; static;
    class procedure CacheLogInfo(const S: string; const Args: array of const); overload; static;
    class procedure CacheLogMessage(const S: string); overload; static;
    class procedure CacheLogMessage(const S: string; const Args: array of const); overload; static;
    class procedure CacheLogWarn(const S: string); overload; static;
    class procedure CacheLogWarn(const S: string; const Args: array of const); overload; static;

    class procedure CacheLogError(const S: string); overload; static;
    class procedure CacheLogError(E: Exception; const ExtraMsg: string); overload; static;
    class procedure CacheLogError(E: Exception; const S: string; const Args: array of const); overload; static;
    class procedure CacheLogError(E: Exception); overload; static;

    class procedure CacheLogFatalError(const S: string); overload; static;
    class procedure CacheLogFatalError(E: Exception; const ExtraMsg: string); overload; static;
    class procedure CacheLogFatalError(E: Exception; const S: string; const Args: array of const); overload; static;
    class procedure CacheLogFatalError(E: Exception); overload; static;

    class procedure CacheClear; static;
    class procedure WriteCachedLog; static;
    class procedure ForceWriteCachedLog; static;

    class procedure ForceLog(Proc: TProc); static;
    class procedure ForceLogMessage(const S: string); overload; static;
    class procedure ForceLogMessage(const S: string; const Args: array of const); overload; static;
    class procedure ForceLogError(const S: string); overload; static;
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

type
  TLogEntry = class
  private
    FLogString: string;
    FIndent: string;
    FThreadID: TThreadID;
    FTimeStamp: TDateTime;
    FLogType: TLogType;
  end;

  TLogEntryList = class(TObjectList<TLogEntry>)
  end;

  TLogCache = class
  private
    FEntryList: TLogEntryList;
  public
    constructor Create;
    destructor Destroy; override;
    property EntryList: TLogEntryList read FEntryList;
  end;

var
  GlobalLogCache: TLogCache;

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

class procedure TjachLog.CacheClear;
begin
  FCacheCS.Enter;
  try
    GlobalLogCache.FEntryList.Clear;
  finally
    FCacheCS.Leave;
  end;
end;

class procedure TjachLog.CacheLog(ALogType: TLogType; const S: string);
var
  Entry: TLogEntry;
begin
  Entry := TLogEntry.Create;
  try
    Entry.FTimeStamp := Now;
    Entry.FIndent := FIndentSpaces;
    Entry.FLogString := S;
    Entry.FLogType := ALogType;
    Entry.FThreadID := GetCurrentThreadId;
    FCacheCS.Enter;
    try
      GlobalLogCache.EntryList.Add(Entry);
    finally
      FCacheCS.Leave;
    end;
  except
    Entry.Free;
  end;
end;

class procedure TjachLog.CacheLogDebug(const S: string);
begin
  {$if defined(debug) or defined(dbglog)}
  CacheLog(ltDebug, S);
  {$endif}
end;

class procedure TjachLog.CacheLogDebug(const S: string;
  const Args: array of const);
begin
  {$if defined(debug) or defined(dbglog)}
  CacheLogDebug(Format(S, Args));
  {$endif}
end;

class procedure TjachLog.CacheLogError(E: Exception; const S: string;
  const Args: array of const);
begin
  CacheLogError(E, Format(S, Args));
end;

class procedure TjachLog.CacheLogError(E: Exception);
begin
  CacheLog(ltError, GetExceptionStr(E));
end;

class procedure TjachLog.CacheLogError(E: Exception; const ExtraMsg: string);
begin
  CacheLog(ltError, Format('%s'#13'%s', [ExtraMsg, GetExceptionStr(E)]));
end;

class procedure TjachLog.CacheLogError(const S: string);
begin
  CacheLog(ltError, S);
end;

class procedure TjachLog.CacheLogFatalError(E: Exception);
begin
  CacheLog(ltFatalError, GetExceptionStr(E));
end;

class procedure TjachLog.CacheLogFatalError(E: Exception; const S: string;
  const Args: array of const);
begin
  CacheLogFatalError(E, Format(S, Args));
end;

class procedure TjachLog.CacheLogFatalError(E: Exception;
  const ExtraMsg: string);
begin
  CacheLog(ltFatalError, Format('%s'#13'%s', [ExtraMsg, GetExceptionStr(E)]));
end;

class procedure TjachLog.CacheLogFatalError(const S: string);
begin
  CacheLog(ltFatalError, S);
end;

class procedure TjachLog.CacheLogInfo(const S: string);
begin
  CacheLog(ltInfo, S);
end;

class procedure TjachLog.CacheLogInfo(const S: string;
  const Args: array of const);
begin
  CacheLog(ltInfo, Format(S, Args));
end;

class procedure TjachLog.CacheLogMessage(const S: string;
  const Args: array of const);
begin
  CacheLog(ltMessage, Format(S, Args));
end;

class procedure TjachLog.CacheLogMessage(const S: string);
begin
  CacheLog(ltMessage, S);
end;

class procedure TjachLog.CacheLogWarn(const S: string);
begin
  CacheLog(ltWarn, S);
end;

class procedure TjachLog.CacheLogWarn(const S: string;
  const Args: array of const);
begin
  CacheLog(ltWarn, Format(S, Args));
end;

class procedure TjachLog.DecIndent;
begin
  Delete(FIndentSpaces, 1, 2);
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

class procedure TjachLog.ForceLogError(const S: string);
var
  WasActive: Boolean;
begin
  WasActive := IsActive;
  try
    IsActive := True;
    LogError(S);
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

class procedure TjachLog.ForceWriteCachedLog;
var
  WasActive: Boolean;
begin
  WasActive := IsActive;
  try
    IsActive := True;
    WriteCachedLog;
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
  Result := FIndentSpaces;
end;

class procedure TjachLog.IncIndent;
begin
  FIndentSpaces := FIndentSpaces + '  ';
end;

class procedure TjachLog.Log(ALogType: TLogType; const S: string);
var
  Writer: TjachLogWriter;
begin
  if not IsActive then
    Exit;
  for Writer in FRegisteredLoggers do
    try
      Writer.Lock.Enter;
      try
        Writer.OpenLogChannel;
        try
          Writer.Write(ALogType, S, FIndentSpaces, GetCurrentThreadID, Now);
        finally
          Writer.CloseLogChannel;
        end;
      finally
        Writer.Lock.Leave;
      end;
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

class procedure TjachLog.RegisterLogger(ALogger: TjachLogWriter);
begin
  FRegisteredLoggers.Add(ALogger);
end;

class procedure TjachLog.WriteCachedLog;
var
  SavedIndentSpaces: string;
  LogEntry: TLogEntry;
  Writer: TjachLogWriter;
begin
  if not IsActive then
  begin
    FCacheCS.Enter;
    try
      GlobalLogCache.EntryList.Clear;
    finally
      FCacheCS.Leave;
    end;
    Exit;
  end;
  SavedIndentSpaces := FIndentSpaces;
  try
    FIndentSpaces := '';
    LogMessage('Cached LOG Write BEGIN ********************');
    try
      FCacheCS.Enter;
      try
        for Writer in FRegisteredLoggers do
          try
            Writer.OpenLogChannel;
            try
              for LogEntry in GlobalLogCache.EntryList do
                Writer.Write(LogEntry.FLogType, LogEntry.FLogString, LogEntry.FIndent, LogEntry.FThreadID, LogEntry.FTimeStamp);
            finally
              Writer.CloseLogChannel;
            end;
          except
            ;
          end;
      finally
        FCacheCS.Leave;
      end;
      LogMessage('Cached LOG Write END **********************');
    except
      on E:Exception do
      begin
        FIndentSpaces := '';
        LogError(E, 'Cached LOG Write Error');
      end;
    end;
  finally
    FIndentSpaces := SavedIndentSpaces;
    GlobalLogCache.EntryList.Clear;
  end;
end;

{ TLogCache }

constructor TLogCache.Create;
begin
  inherited Create;
  FEntryList := TLogEntryList.Create(True);
end;

destructor TLogCache.Destroy;
begin
  FEntryList.Free;
  inherited;
end;

{ TDiskLogger }

initialization
  TjachLog.FCS := TCriticalSection.Create;
  TjachLog.FCacheCS := TCriticalSection.Create;
  TjachLog.FRegisteredLoggers := TObjectList<TjachLogWriter>.Create();

  GlobalLogCache := TLogCache.Create;
finalization
  TjachLog.FCS.Free;
  TjachLog.FCacheCS.Free;
  TjachLog.FRegisteredLoggers.Free;
  GlobalLogCache.Free;
end.
