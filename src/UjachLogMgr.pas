{ ******************************************************** }
{ **                                                    ** }
{ ** Multithreaded Log for Delphi                       ** }
{ **                                                    ** }
{ ** Author:                                            ** }
{ **     Juan Antonio Castillo H. (jachguate)           ** }
{ **                                                    ** }
{ ** Copyright (c) 2007-2022                            ** }
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

unit ujachLogMgr;

interface

uses Classes, System.SysUtils, System.Types, System.SyncObjs,
  System.Generics.Collections;

type
  TjachLogTopicIndex = 0..63;

  TLogSeverity = (          lsEmergency{0}, lsAlert{1}, lsCritical{2}, lsError{3}, lsWarning{4}, lsNotice{5}, lsInfo{6}, lsDebug{7});
  TLogLevel    = (llOff{0}, llEmergency{1}, llAlert{2}, llCritical{3}, llError{4}, llWarning{5}, llNotice{6}, llInfo{7}, llDebug{8}, llAll{9});

  IjachLogEntry = interface
  ['{D142AB1F-FFB4-4CFD-B5C1-571950541DCA}']
    function GetLogString: string;
    function GetIndent: string;
    function GetThreadID: TThreadID;
    function GetTimeStamp: TDateTime;
    function GetSeverity: TLogSeverity;
    function GetTopic: TjachLogTopicIndex;
    property LogString: string read GetLogString;
    property Indent: string read GetIndent;
    property ThreadID: TThreadID read GetThreadID;
    property TimeStamp: TDateTime read GetTimeStamp;
    property Severity: TLogSeverity read GetSeverity;
    property Topic: TjachLogTopicIndex read GetTopic;
  end;

  TjachLogEntryList = class(TList<IjachLogEntry>)
  end;

  TjachLogWriter = class
  private
    FLock: TCriticalSection;
    FThread: TThread;
    FIsActive: Boolean;
    procedure SetIsActive(const Value: Boolean);
  protected
    const WWMAX_LEN = 255;
    function WordWrap(const S: string; MaxLen: UInt16 = WWMAX_LEN): TStringDynArray; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OpenLogChannel; virtual;
    procedure CloseLogChannel; virtual;
    procedure Write(ATopic: TjachLogTopicIndex; ASeverity: TLogSeverity;
      const S, AIndentSpaces: string; const AThreadID: TThreadID;
      const ATimeStamp: TDateTime); virtual; abstract;
    procedure WriteEntry(AEntry: IjachLogEntry); virtual;
    function GetLock: TCriticalSection; virtual;
    procedure SetWriterThread(AThread: TThread);
    property IsActive: Boolean read FIsActive write SetIsActive;
    property Lock: TCriticalSection read GetLock;
    property Thread: TThread read FThread;
  end;

  TjachLogWriterClass = class of TjachLogWriter;

  TLogCache = class
  private
    FEntryList: TjachLogEntryList;
  public
    constructor Create;
    destructor Destroy; override;
    property EntryList: TjachLogEntryList read FEntryList;
  end;

type
  TjachLog = class
  private
    FCS: TCriticalSection;
    FRegisteredLogWriters: TThreadList<TjachLogWriter>;
    FIndentSpaces: string;
    FCacheCS: TCriticalSection;
    FIsActive: Boolean;
    FDefaultTopic: TjachLogTopicIndex;
    FIsCached: Boolean;
    FLogLevel: array[TjachLogTopicIndex] of TLogLevel;
    FTopicName: array[TjachLogTopicIndex] of string;
    FCache: TLogCache;
    FIncludeTopicName: Boolean;
    FUseSeparateThreadToWrite: Boolean;
    FWriteThread: TThread;
    function GetExceptionStr(E: Exception): string;
    procedure CacheLog(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity; const S: string); inline;
    procedure SetIsCached(const Value: Boolean);
    function GetLogLevel(Index: TjachLogTopicIndex): TLogLevel;
    procedure SetLogLevel(Index: TjachLogTopicIndex; const Value: TLogLevel);
    function GetTopicName(Index: TjachLogTopicIndex): string;
    procedure SetTopicName(Index: TjachLogTopicIndex; const Value: string);
    procedure SetIncludeTopicName(const Value: Boolean);
    procedure SetUseSeparateThreadToWrite(const Value: Boolean);
    procedure FreeRegisteredLogWriters;
    procedure TerminateCoordinatorThread;
  public
    constructor Create(ADefaultTopicLevel: TLogLevel = llInfo; ADefaultTopic: TjachLogTopicIndex = 0);
    destructor Destroy; override;

    property IsCached: Boolean read FIsCached write SetIsCached;
    property LogLevel[Index: TjachLogTopicIndex]: TLogLevel read GetLogLevel write SetLogLevel;
    property TopicName[Index: TjachLogTopicIndex]: string read GetTopicName write SetTopicName;
    property DefaultTopic: TjachLogTopicIndex read FDefaultTopic write FDefaultTopic;
    property IncludeTopicName: Boolean read FIncludeTopicName write SetIncludeTopicName;

    property UseSeparateThreadToWrite: Boolean read FUseSeparateThreadToWrite write SetUseSeparateThreadToWrite;

    procedure IncIndent;
    procedure DecIndent;

    procedure RegisterLogWriter(ALogWriter: TjachLogWriter);
    procedure UnRegisterLogWriter(ALogWriter: TjachLogWriter);
    function FindLogWriterByClass(ALogWriterClass: TjachLogWriterClass): TjachLogWriter;
    function GetLogWriterByClass(ALogWriterClass: TjachLogWriterClass): TjachLogWriter;

    function GetIndentSpaces: string; inline;

    procedure Log(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity; const S: string); overload;
    procedure Log(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity; const S: string; const Args: array of const); overload;
    procedure Log(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity; E: Exception); overload; inline;
    procedure Log(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity; const ExtraMsg: string; E: Exception); overload; inline;
    procedure Log(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity; const S: string; const Args: array of const; E: Exception); overload;

    procedure Log(ALogSeverity: TLogSeverity; const S: string); overload; inline;
    procedure Log(ALogSeverity: TLogSeverity; const S: string; const Args: array of const); overload;
    procedure Log(ALogSeverity: TLogSeverity; E: Exception); overload; inline;
    procedure Log(ALogSeverity: TLogSeverity; const ExtraMsg: string; E: Exception); overload; inline;
    procedure Log(ALogSeverity: TLogSeverity; const S: string; const Args: array of const; E: Exception); overload;

    procedure LogEmergency(ATopic: TjachLogTopicIndex; const S: string); overload; inline;
    procedure LogEmergency(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const); overload;
    procedure LogEmergency(ATopic: TjachLogTopicIndex; E: Exception); overload; inline;
    procedure LogEmergency(ATopic: TjachLogTopicIndex; const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogEmergency(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const; E: Exception); overload;
    procedure LogEmergency(const S: string); overload; inline;
    procedure LogEmergency(const S: string; const Args: array of const); overload;
    procedure LogEmergency(E: Exception); overload; inline;
    procedure LogEmergency(const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogEmergency(const S: string; const Args: array of const; E: Exception); overload;

    procedure LogAlert(ATopic: TjachLogTopicIndex; const S: string); overload; inline;
    procedure LogAlert(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const); overload;
    procedure LogAlert(ATopic: TjachLogTopicIndex; E: Exception); overload; inline;
    procedure LogAlert(ATopic: TjachLogTopicIndex; const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogAlert(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const; E: Exception); overload;
    procedure LogAlert(const S: string); overload; inline;
    procedure LogAlert(const S: string; const Args: array of const); overload;
    procedure LogAlert(E: Exception); overload; inline;
    procedure LogAlert(const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogAlert(const S: string; const Args: array of const; E: Exception); overload;

    procedure LogCritical(ATopic: TjachLogTopicIndex; const S: string); overload; inline;
    procedure LogCritical(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const); overload;
    procedure LogCritical(ATopic: TjachLogTopicIndex; E: Exception); overload; inline;
    procedure LogCritical(ATopic: TjachLogTopicIndex; const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogCritical(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const; E: Exception); overload;
    procedure LogCritical(const S: string); overload; inline;
    procedure LogCritical(const S: string; const Args: array of const); overload;
    procedure LogCritical(E: Exception); overload; inline;
    procedure LogCritical(const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogCritical(const S: string; const Args: array of const; E: Exception); overload;

    procedure LogError(ATopic: TjachLogTopicIndex; const S: string); overload; inline;
    procedure LogError(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const); overload;
    procedure LogError(ATopic: TjachLogTopicIndex; E: Exception); overload; inline;
    procedure LogError(ATopic: TjachLogTopicIndex; const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogError(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const; E: Exception); overload;
    procedure LogError(const S: string); overload; inline;
    procedure LogError(const S: string; const Args: array of const); overload;
    procedure LogError(E: Exception); overload; inline;
    procedure LogError(const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogError(const S: string; const Args: array of const; E: Exception); overload;

    procedure LogWarning(ATopic: TjachLogTopicIndex; const S: string); overload; inline;
    procedure LogWarning(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const); overload;
    procedure LogWarning(ATopic: TjachLogTopicIndex; E: Exception); overload; inline;
    procedure LogWarning(ATopic: TjachLogTopicIndex; const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogWarning(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const; E: Exception); overload;
    procedure LogWarning(const S: string); overload; inline;
    procedure LogWarning(const S: string; const Args: array of const); overload;
    procedure LogWarning(E: Exception); overload; inline;
    procedure LogWarning(const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogWarning(const S: string; const Args: array of const; E: Exception); overload;

    procedure LogNotice(ATopic: TjachLogTopicIndex; const S: string); overload; inline;
    procedure LogNotice(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const); overload;
    procedure LogNotice(ATopic: TjachLogTopicIndex; E: Exception); overload; inline;
    procedure LogNotice(ATopic: TjachLogTopicIndex; const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogNotice(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const; E: Exception); overload;
    procedure LogNotice(const S: string); overload; inline;
    procedure LogNotice(const S: string; const Args: array of const); overload;
    procedure LogNotice(E: Exception); overload; inline;
    procedure LogNotice(const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogNotice(const S: string; const Args: array of const; E: Exception); overload;

    procedure LogInfo(ATopic: TjachLogTopicIndex; const S: string); overload; inline;
    procedure LogInfo(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const); overload;
    procedure LogInfo(ATopic: TjachLogTopicIndex; E: Exception); overload; inline;
    procedure LogInfo(ATopic: TjachLogTopicIndex; const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogInfo(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const; E: Exception); overload;
    procedure LogInfo(const S: string); overload; inline;
    procedure LogInfo(const S: string; const Args: array of const); overload;
    procedure LogInfo(E: Exception); overload; inline;
    procedure LogInfo(const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogInfo(const S: string; const Args: array of const; E: Exception); overload;

    procedure LogDebug(ATopic: TjachLogTopicIndex; const S: string); overload; inline;
    procedure LogDebug(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const); overload;
    procedure LogDebug(ATopic: TjachLogTopicIndex; E: Exception); overload; inline;
    procedure LogDebug(ATopic: TjachLogTopicIndex; const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogDebug(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const; E: Exception); overload;
    procedure LogDebug(const S: string); overload; inline;
    procedure LogDebug(const S: string; const Args: array of const); overload;
    procedure LogDebug(E: Exception); overload; inline;
    procedure LogDebug(const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogDebug(const S: string; const Args: array of const; E: Exception); overload;

    procedure LogDebugVerbose(ATopic: TjachLogTopicIndex; const S: string); overload; inline;
    procedure LogDebugVerbose(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const); overload;
    procedure LogDebugVerbose(ATopic: TjachLogTopicIndex; E: Exception); overload; inline;
    procedure LogDebugVerbose(ATopic: TjachLogTopicIndex; const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogDebugVerbose(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const; E: Exception); overload;
    procedure LogDebugVerbose(const S: string); overload; inline;
    procedure LogDebugVerbose(const S: string; const Args: array of const); overload;
    procedure LogDebugVerbose(E: Exception); overload; inline;
    procedure LogDebugVerbose(const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogDebugVerbose(const S: string; const Args: array of const; E: Exception); overload;

    procedure CacheClear;
    procedure WriteCachedLog;
  end;

  EjachLogError = class(Exception)
  end;

  EjachLogNoWriterRegistered = class(EjachLogError)
  end;

function LogSeverityToStr(ALogSeverity: TLogSeverity): string;
function CreateLogEntry(ATopic: TjachLogTopicIndex; ASeverity: TLogSeverity; AIndent, ALogString: string): IjachLogEntry;

var
  jachLog: TjachLog;

implementation
uses StrUtils,
     {$ifdef MSWindows}
     Winapi.Windows,
     {$endif}
     {$ifdef HAS_EXCEPTION_STACKTRACE}
     JclDebug,
     {$endif HAS_EXCEPTION_STACKTRACE}
     System.IOUtils;

type
  TLogEntry = class(TInterfacedObject, IjachLogEntry)
  private
    FLogString: string;
    FIndent: string;
    FThreadID: TThreadID;
    FTimeStamp: TDateTime;
    FSeverity: TLogSeverity;
    FTopic: TjachLogTopicIndex;
    function GetLogString: string;
    function GetIndent: string;
    function GetThreadID: TThreadID;
    function GetTimeStamp: TDateTime;
    function GetSeverity: TLogSeverity;
    function GetTopic: TjachLogTopicIndex;
  public
    constructor Create(ATopic: TjachLogTopicIndex; ASeverity: TLogSeverity; AIndent, ALogString: string);
  end;

  TjachLogWriteCoordinatorThread = class(TThread)
  private
    FLog: TjachLog;
    FEntryQueue: TThreadedQueue<IjachLogEntry>;
    procedure CreateWriterThreads;
    procedure FreeWriterThreads;
  protected
    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    constructor Create(ALog: TjachLog);
    destructor Destroy; override;
  end;

  TjachLogWriterThread = class(TThread)
  private
    FLog: TjachLog;
    FWriter: TjachLogWriter;
    FEntryQueue: TThreadedQueue<IjachLogEntry>;
    FTerminatedEvent: TEvent;
  protected
    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    constructor Create(ALog: TjachLog; AWriter: TjachLogWriter);
    destructor Destroy; override;
  end;

function LogSeverityToStr(ALogSeverity: TLogSeverity): string;
begin
  case ALogSeverity of
    lsEmergency: Result := 'Emer';
    lsAlert: Result := 'Aler';
    lsCritical: Result := 'Crit';
    lsError: Result := 'Err ';
    lsWarning: Result := 'Warn';
    lsNotice: Result := 'Noti';
    lsInfo: Result := 'Info';
    lsDebug: Result := 'Dbg ';
  end;
end;

function CreateLogEntry(ATopic: TjachLogTopicIndex; ASeverity: TLogSeverity; AIndent, ALogString: string): IjachLogEntry;
begin
  Result := TLogEntry.Create(ATopic, ASeverity, AIndent, ALogString);
end;

{ TLogEntry }

constructor TLogEntry.Create(ATopic: TjachLogTopicIndex;
  ASeverity: TLogSeverity; AIndent, ALogString: string);
begin
  inherited Create;
  FTimeStamp := Now;
  FIndent := AIndent;
  FLogString := ALogString;
  FSeverity := ASeverity;
  FThreadID := GetCurrentThreadId;
  FTopic := ATopic;
end;

function TLogEntry.GetIndent: string;
begin
  Result := FIndent;
end;

function TLogEntry.GetLogString: string;
begin
  Result := FLogString;
end;

function TLogEntry.GetSeverity: TLogSeverity;
begin
  Result := FSeverity;
end;

function TLogEntry.GetThreadID: TThreadID;
begin
  Result := FThreadID;
end;

function TLogEntry.GetTimeStamp: TDateTime;
begin
  Result := FTimeStamp;
end;

function TLogEntry.GetTopic: TjachLogTopicIndex;
begin
  Result := FTopic;
end;

{ TjachLogWriter }

procedure TjachLogWriter.CloseLogChannel;
begin

end;

constructor TjachLogWriter.Create;
begin
  inherited Create;
  FIsActive := True;
  FLock := TCriticalSection.Create;
end;

destructor TjachLogWriter.Destroy;
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FThread.Free;
  end;
  FLock.Free;
  inherited;
end;

function TjachLogWriter.GetLock: TCriticalSection;
begin
  Result := FLock;
end;

procedure TjachLogWriter.OpenLogChannel;
begin

end;

procedure TjachLogWriter.SetIsActive(const Value: Boolean);
begin
  FIsActive := Value;
end;

procedure TjachLogWriter.SetWriterThread(AThread: TThread);
begin
  FThread := AThread;
end;

function TjachLogWriter.WordWrap(const S: string;
  MaxLen: UInt16): TStringDynArray;
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

procedure TjachLogWriter.WriteEntry(AEntry: IjachLogEntry);
begin
  if Assigned(AEntry) then
    Write(AEntry.Topic, AEntry.Severity, AEntry.LogString, AEntry.Indent, AEntry.ThreadID, AEntry.TimeStamp);
end;

{ TLogCache }

constructor TLogCache.Create;
begin
  inherited Create;
  FEntryList := TjachLogEntryList.Create;
end;

destructor TLogCache.Destroy;
begin
  FEntryList.Free;
  inherited;
end;

{ TjachLog }

procedure TjachLog.CacheClear;
begin
  FCacheCS.Enter;
  try
    FCache.EntryList.Clear;
  finally
    FCacheCS.Leave;
  end;
end;

procedure TjachLog.CacheLog(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity; const S: string);
begin
  FCacheCS.Enter;
  try
    FCache.EntryList.Add(CreateLogEntry (ATopic, ALogSeverity, FIndentSpaces, S));
  finally
    FCacheCS.Leave;
  end;
end;

constructor TjachLog.Create(ADefaultTopicLevel: TLogLevel = llInfo; ADefaultTopic: TjachLogTopicIndex = 0);
var
  I: Integer;
begin
  inherited Create;
  FCS := TCriticalSection.Create;
  FRegisteredLogWriters := TThreadList<TjachLogWriter>.Create();
  FCacheCS := TCriticalSection.Create;
  FIsActive := True;
  FDefaultTopic := ADefaultTopic;
  FIsCached := False;
  FCache := TLogCache.Create;
  for I := Low(FLogLevel) to High(FLogLevel) do
    FLogLevel[I] := ADefaultTopicLevel;
end;

procedure TjachLog.DecIndent;
begin
  Delete(FIndentSpaces, 1, 2);
end;

destructor TjachLog.Destroy;
begin
  FIsActive := False;
  if FUseSeparateThreadToWrite then
    TerminateCoordinatorThread;
  FCacheCS.Free;
  FCS.Free;
  FreeRegisteredLogWriters;
  FRegisteredLogWriters.Free;
  FCache.Free;
  inherited;
end;

procedure TjachLog.FreeRegisteredLogWriters;
var
  lRegisteredWriters: TList<TjachLogWriter>;
  I: Integer;
begin
  lRegisteredWriters := FRegisteredLogWriters.LockList;
  try
    for I := 0 to lRegisteredWriters.Count - 1 do
    begin
      lRegisteredWriters[I].Free;
    end;
    lRegisteredWriters.Clear;
  finally
    FRegisteredLogWriters.UnlockList;
  end;
end;

function TjachLog.GetExceptionStr(E: Exception): string;
begin
  Result := Format('%s'#13'%s', [E.ClassName, E.Message]);
  {$ifdef HAS_EXCEPTION_STACKTRACE}
  Result := Result + #13 + E.StackTrace;
  {$endif HAS_EXCEPTION_STACKTRACE}
end;

function TjachLog.GetIndentSpaces: string;
begin
  Result := FIndentSpaces;
end;

function TjachLog.GetLogLevel(Index: TjachLogTopicIndex): TLogLevel;
begin
  Result := FLogLevel[Index];
end;

function TjachLog.GetLogWriterByClass(
  ALogWriterClass: TjachLogWriterClass): TjachLogWriter;
begin
  Result := FindLogWriterByClass(ALogWriterClass);
  if not Assigned(Result) then
    raise EjachLogNoWriterRegistered.CreateFmt('There''s no writer of %s class registered.', [ALogWriterClass.ClassName]);
end;

function TjachLog.FindLogWriterByClass(
  ALogWriterClass: TjachLogWriterClass): TjachLogWriter;
var
  Writers: TList<TjachLogWriter>;
  I: Integer;
begin
  Result := nil;
  FCS.Enter;
  try
    Writers := FRegisteredLogWriters.LockList;
    try
      for I := 0 to Writers.Count - 1 do
        if Writers[I] is ALogWriterClass then
        begin
          Result := Writers[I];
          Break;
        end;
    finally
      FRegisteredLogWriters.UnlockList;
    end;
  finally
    FCS.Leave;
  end;
end;

function TjachLog.GetTopicName(Index: TjachLogTopicIndex): string;
begin
  Result := FTopicName[Index];
end;

procedure TjachLog.IncIndent;
begin
  FIndentSpaces := FIndentSpaces + '  ';
end;

procedure TjachLog.Log(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity; const S: string);
var
  lRegisteredLogWriters: TList<TjachLogWriter>;
  Writer: TjachLogWriter;
begin
  if not FIsActive then
    Exit;

  if Byte(FLogLevel[ATopic]) > Byte(ALogSeverity) then
    if FUseSeparateThreadToWrite then
      if FIncludeTopicName then
        TjachLogWriteCoordinatorThread(FWriteThread).FEntryQueue.PushItem(CreateLogEntry (ATopic, ALogSeverity, FIndentSpaces, '[' + FTopicName[ATopic] + ']' + S))
      else
        TjachLogWriteCoordinatorThread(FWriteThread).FEntryQueue.PushItem(CreateLogEntry (ATopic, ALogSeverity, FIndentSpaces, S))
    else if FIsCached then
      if FIncludeTopicName then
        CacheLog(ATopic, ALogSeverity, '[' + FTopicName[ATopic] + ']' + S)
      else
        CacheLog(ATopic, ALogSeverity, S)
    else
    begin
      lRegisteredLogWriters := FRegisteredLogWriters.LockList;
      try
        for Writer in lRegisteredLogWriters do
          try
            Writer.Lock.Enter;
            try
              Writer.OpenLogChannel;
              try
                if FIncludeTopicName then
                  Writer.Write(ATopic, ALogSeverity, '[' + FTopicName[ATopic] + ']' + S, FIndentSpaces, GetCurrentThreadID, Now)
                else
                  Writer.Write(ATopic, ALogSeverity, S, FIndentSpaces, GetCurrentThreadID, Now);
              finally
                Writer.CloseLogChannel;
              end;
            finally
              Writer.Lock.Leave;
            end;
          except
            ;
          end;
      finally
        FRegisteredLogWriters.UnlockList;
      end;
    end;
end;

procedure TjachLog.Log(ALogSeverity: TLogSeverity; const S: string);
begin
  Log(FDefaultTopic, ALogSeverity, S);
end;

procedure TjachLog.LogEmergency(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const; E: Exception);
begin
  Log(ATopic, lsEmergency, Format(S, Args), E);
end;

procedure TjachLog.LogEmergency(ATopic: TjachLogTopicIndex;
  const ExtraMsg: string; E: Exception);
begin
  Log(ATopic, lsEmergency, ExtraMsg, E);
end;

procedure TjachLog.LogEmergency(ATopic: TjachLogTopicIndex; E: Exception);
begin
  Log(ATopic, lsEmergency, E);
end;

procedure TjachLog.LogEmergency(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const);
begin
  Log(ATopic, lsEmergency, Format(S, Args));
end;

procedure TjachLog.LogEmergency(ATopic: TjachLogTopicIndex; const S: string);
begin
  Log(ATopic, lsEmergency, S);
end;

procedure TjachLog.LogEmergency(const S: string; const Args: array of const;
  E: Exception);
begin
  Log(lsEmergency, Format(S, Args), E);
end;

procedure TjachLog.LogEmergency(const ExtraMsg: string; E: Exception);
begin
  Log(lsEmergency, ExtraMsg, E);
end;

procedure TjachLog.LogEmergency(E: Exception);
begin
  Log(lsEmergency, E);
end;

procedure TjachLog.LogEmergency(const S: string);
begin
  Log(lsEmergency, S);
end;

procedure TjachLog.LogEmergency(const S: string; const Args: array of const);
begin
  Log(lsEmergency, Format(S, Args));
end;

procedure TjachLog.LogAlert(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const; E: Exception);
begin
  Log(ATopic, lsAlert, Format(S, Args), E);
end;

procedure TjachLog.LogAlert(ATopic: TjachLogTopicIndex;
  const ExtraMsg: string; E: Exception);
begin
  Log(ATopic, lsAlert, ExtraMsg, E);
end;

procedure TjachLog.LogAlert(ATopic: TjachLogTopicIndex; E: Exception);
begin
  Log(ATopic, lsAlert, E);
end;

procedure TjachLog.LogAlert(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const);
begin
  Log(ATopic, lsAlert, Format(S, Args));
end;

procedure TjachLog.LogAlert(ATopic: TjachLogTopicIndex; const S: string);
begin
  Log(ATopic, lsAlert, S);
end;

procedure TjachLog.LogAlert(const S: string; const Args: array of const;
  E: Exception);
begin
  Log(lsAlert, Format(S, Args), E);
end;

procedure TjachLog.LogAlert(const ExtraMsg: string; E: Exception);
begin
  Log(lsAlert, ExtraMsg, E);
end;

procedure TjachLog.LogAlert(E: Exception);
begin
  Log(lsAlert, E);
end;

procedure TjachLog.LogAlert(const S: string);
begin
  Log(lsAlert, S);
end;

procedure TjachLog.LogAlert(const S: string; const Args: array of const);
begin
  Log(lsAlert, Format(S, Args));
end;

procedure TjachLog.LogCritical(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const; E: Exception);
begin
  Log(ATopic, lsCritical, Format(S, Args), E);
end;

procedure TjachLog.LogCritical(ATopic: TjachLogTopicIndex;
  const ExtraMsg: string; E: Exception);
begin
  Log(ATopic, lsCritical, ExtraMsg, E);
end;

procedure TjachLog.LogCritical(ATopic: TjachLogTopicIndex; E: Exception);
begin
  Log(ATopic, lsCritical, E);
end;

procedure TjachLog.LogCritical(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const);
begin
  Log(ATopic, lsCritical, Format(S, Args));
end;

procedure TjachLog.LogCritical(ATopic: TjachLogTopicIndex; const S: string);
begin
  Log(ATopic, lsCritical, S);
end;

procedure TjachLog.LogCritical(const S: string; const Args: array of const;
  E: Exception);
begin
  Log(lsCritical, Format(S, Args), E);
end;

procedure TjachLog.LogCritical(const ExtraMsg: string; E: Exception);
begin
  Log(lsCritical, ExtraMsg, E);
end;

procedure TjachLog.LogCritical(E: Exception);
begin
  Log(lsCritical, E);
end;

procedure TjachLog.LogCritical(const S: string);
begin
  Log(lsCritical, S);
end;

procedure TjachLog.LogCritical(const S: string; const Args: array of const);
begin
  Log(lsCritical, Format(S, Args));
end;

procedure TjachLog.LogError(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const; E: Exception);
begin
  Log(ATopic, lsError, Format(S, Args), E);
end;

procedure TjachLog.LogError(ATopic: TjachLogTopicIndex;
  const ExtraMsg: string; E: Exception);
begin
  Log(ATopic, lsError, ExtraMsg, E);
end;

procedure TjachLog.LogError(ATopic: TjachLogTopicIndex; E: Exception);
begin
  Log(ATopic, lsError, E);
end;

procedure TjachLog.LogError(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const);
begin
  Log(ATopic, lsError, Format(S, Args));
end;

procedure TjachLog.LogError(ATopic: TjachLogTopicIndex; const S: string);
begin
  Log(ATopic, lsError, S);
end;

procedure TjachLog.LogError(const S: string; const Args: array of const;
  E: Exception);
begin
  Log(lsError, Format(S, Args), E);
end;

procedure TjachLog.LogError(const ExtraMsg: string; E: Exception);
begin
  Log(lsError, ExtraMsg, E);
end;

procedure TjachLog.LogError(E: Exception);
begin
  Log(lsError, E);
end;

procedure TjachLog.LogError(const S: string);
begin
  Log(lsError, S);
end;

procedure TjachLog.LogError(const S: string; const Args: array of const);
begin
  Log(lsError, Format(S, Args));
end;

procedure TjachLog.LogWarning(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const; E: Exception);
begin
  Log(ATopic, lsWarning, Format(S, Args), E);
end;

procedure TjachLog.LogWarning(ATopic: TjachLogTopicIndex;
  const ExtraMsg: string; E: Exception);
begin
  Log(ATopic, lsWarning, ExtraMsg, E);
end;

procedure TjachLog.LogWarning(ATopic: TjachLogTopicIndex; E: Exception);
begin
  Log(ATopic, lsWarning, E);
end;

procedure TjachLog.LogWarning(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const);
begin
  Log(ATopic, lsWarning, Format(S, Args));
end;

procedure TjachLog.LogWarning(ATopic: TjachLogTopicIndex; const S: string);
begin
  Log(ATopic, lsWarning, S);
end;

procedure TjachLog.LogWarning(const S: string; const Args: array of const;
  E: Exception);
begin
  Log(lsWarning, Format(S, Args), E);
end;

procedure TjachLog.LogWarning(const ExtraMsg: string; E: Exception);
begin
  Log(lsWarning, ExtraMsg, E);
end;

procedure TjachLog.LogWarning(E: Exception);
begin
  Log(lsWarning, E);
end;

procedure TjachLog.LogWarning(const S: string);
begin
  Log(lsWarning, S);
end;

procedure TjachLog.LogWarning(const S: string; const Args: array of const);
begin
  Log(lsWarning, Format(S, Args));
end;

procedure TjachLog.LogNotice(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const; E: Exception);
begin
  Log(ATopic, lsNotice, Format(S, Args), E);
end;

procedure TjachLog.LogNotice(ATopic: TjachLogTopicIndex;
  const ExtraMsg: string; E: Exception);
begin
  Log(ATopic, lsNotice, ExtraMsg, E);
end;

procedure TjachLog.LogNotice(ATopic: TjachLogTopicIndex; E: Exception);
begin
  Log(ATopic, lsNotice, E);
end;

procedure TjachLog.LogNotice(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const);
begin
  Log(ATopic, lsNotice, Format(S, Args));
end;

procedure TjachLog.LogNotice(ATopic: TjachLogTopicIndex; const S: string);
begin
  Log(ATopic, lsNotice, S);
end;

procedure TjachLog.LogNotice(const S: string; const Args: array of const;
  E: Exception);
begin
  Log(lsNotice, Format(S, Args), E);
end;

procedure TjachLog.LogNotice(const ExtraMsg: string; E: Exception);
begin
  Log(lsNotice, ExtraMsg, E);
end;

procedure TjachLog.LogNotice(E: Exception);
begin
  Log(lsNotice, E);
end;

procedure TjachLog.LogNotice(const S: string);
begin
  Log(lsNotice, S);
end;

procedure TjachLog.LogNotice(const S: string; const Args: array of const);
begin
  Log(lsNotice, Format(S, Args));
end;

procedure TjachLog.LogInfo(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const; E: Exception);
begin
  Log(ATopic, lsInfo, Format(S, Args), E);
end;

procedure TjachLog.LogInfo(ATopic: TjachLogTopicIndex;
  const ExtraMsg: string; E: Exception);
begin
  Log(ATopic, lsInfo, ExtraMsg, E);
end;

procedure TjachLog.LogInfo(ATopic: TjachLogTopicIndex; E: Exception);
begin
  Log(ATopic, lsInfo, E);
end;

procedure TjachLog.LogInfo(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const);
begin
  Log(ATopic, lsInfo, Format(S, Args));
end;

procedure TjachLog.LogInfo(ATopic: TjachLogTopicIndex; const S: string);
begin
  Log(ATopic, lsInfo, S);
end;

procedure TjachLog.LogInfo(const S: string; const Args: array of const;
  E: Exception);
begin
  Log(lsInfo, Format(S, Args), E);
end;

procedure TjachLog.LogInfo(const ExtraMsg: string; E: Exception);
begin
  Log(lsInfo, ExtraMsg, E);
end;

procedure TjachLog.LogInfo(E: Exception);
begin
  Log(lsInfo, E);
end;

procedure TjachLog.LogInfo(const S: string);
begin
  Log(lsInfo, S);
end;

procedure TjachLog.LogInfo(const S: string; const Args: array of const);
begin
  Log(lsInfo, Format(S, Args));
end;

procedure TjachLog.LogDebug(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const; E: Exception);
begin
  Log(ATopic, lsDebug, Format(S, Args), E);
end;

procedure TjachLog.LogDebug(ATopic: TjachLogTopicIndex;
  const ExtraMsg: string; E: Exception);
begin
  Log(ATopic, lsDebug, ExtraMsg, E);
end;

procedure TjachLog.LogDebug(ATopic: TjachLogTopicIndex; E: Exception);
begin
  Log(ATopic, lsDebug, E);
end;

procedure TjachLog.LogDebug(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const);
begin
  Log(ATopic, lsDebug, Format(S, Args));
end;

procedure TjachLog.LogDebug(ATopic: TjachLogTopicIndex; const S: string);
begin
  Log(ATopic, lsDebug, S);
end;

procedure TjachLog.LogDebug(const S: string; const Args: array of const;
  E: Exception);
begin
  Log(lsDebug, Format(S, Args), E);
end;

procedure TjachLog.LogDebug(const ExtraMsg: string; E: Exception);
begin
  Log(lsDebug, ExtraMsg, E);
end;

procedure TjachLog.LogDebug(E: Exception);
begin
  Log(lsDebug, E);
end;

procedure TjachLog.LogDebug(const S: string);
begin
  Log(lsDebug, S);
end;

procedure TjachLog.LogDebug(const S: string; const Args: array of const);
begin
  Log(lsDebug, Format(S, Args));
end;

procedure TjachLog.LogDebugVerbose(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const; E: Exception);
begin
  Log(ATopic, lsDebug, Format(S, Args), E);
end;

procedure TjachLog.LogDebugVerbose(ATopic: TjachLogTopicIndex;
  const ExtraMsg: string; E: Exception);
begin
  Log(ATopic, lsDebug, ExtraMsg, E);
end;

procedure TjachLog.LogDebugVerbose(ATopic: TjachLogTopicIndex; E: Exception);
begin
  Log(ATopic, lsDebug, E);
end;

procedure TjachLog.LogDebugVerbose(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const);
begin
  Log(ATopic, lsDebug, Format(S, Args));
end;

procedure TjachLog.LogDebugVerbose(ATopic: TjachLogTopicIndex; const S: string);
begin
  Log(ATopic, lsDebug, S);
end;

procedure TjachLog.LogDebugVerbose(const S: string; const Args: array of const;
  E: Exception);
begin
  Log(lsDebug, Format(S, Args), E);
end;

procedure TjachLog.LogDebugVerbose(const ExtraMsg: string; E: Exception);
begin
  Log(lsDebug, ExtraMsg, E);
end;

procedure TjachLog.LogDebugVerbose(E: Exception);
begin
  Log(lsDebug, E);
end;

procedure TjachLog.LogDebugVerbose(const S: string);
begin
  Log(lsDebug, S);
end;

procedure TjachLog.LogDebugVerbose(const S: string; const Args: array of const);
begin
  Log(lsDebug, Format(S, Args));
end;

procedure TjachLog.Log(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity;
  const ExtraMsg: string; E: Exception);
begin
  Log(ATopic, ALogSeverity,  Format('%s'#13'%s', [ExtraMsg, GetExceptionStr(E)]));
end;

procedure TjachLog.Log(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity;
  const S: string; const Args: array of const; E: Exception);
begin
  Log(ATopic, ALogSeverity, Format(S, Args), E);
end;

procedure TjachLog.Log(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity;
  E: Exception);
begin
  Log(ATopic, ALogSeverity, GetExceptionStr(E));
end;

procedure TjachLog.Log(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity;
  const S: string; const Args: array of const);
begin
  Log(ATopic, ALogSeverity, Format(S, Args));
end;

procedure TjachLog.Log(ALogSeverity: TLogSeverity; const S: string;
  const Args: array of const; E: Exception);
begin
  Log(FDefaultTopic, ALogSeverity, Format(S, Args), E);
end;

procedure TjachLog.Log(ALogSeverity: TLogSeverity; const ExtraMsg: string;
  E: Exception);
begin
  Log(FDefaultTopic, ALogSeverity, ExtraMsg, E);
end;

procedure TjachLog.Log(ALogSeverity: TLogSeverity; E: Exception);
begin
  Log(FDefaultTopic, ALogSeverity, E);
end;

procedure TjachLog.Log(ALogSeverity: TLogSeverity; const S: string;
  const Args: array of const);
begin
  Log(FDefaultTopic, ALogSeverity, Format(S, Args));
end;

procedure TjachLog.RegisterLogWriter(ALogWriter: TjachLogWriter);
begin
  FCS.Enter;
  try
    FRegisteredLogWriters.Add(ALogWriter);
    if FUseSeparateThreadToWrite then
      ALogWriter.SetWriterThread(TjachLogWriterThread.Create(Self, ALogWriter));
  finally
    FCS.Leave;
  end;
end;

procedure TjachLog.SetIncludeTopicName(const Value: Boolean);
begin
  FIncludeTopicName := Value;
end;

procedure TjachLog.SetIsCached(const Value: Boolean);
begin
  FCS.Enter;
  try
    if FIsCached <> Value then
      FIsCached := Value;
  finally
    FCS.Leave;
  end;
end;

procedure TjachLog.SetLogLevel(Index: TjachLogTopicIndex;
  const Value: TLogLevel);
begin
  FLogLevel[Index] := Value;
end;

procedure TjachLog.SetTopicName(Index: TjachLogTopicIndex; const Value: string);
begin
  FTopicName[Index] := Value;
end;

procedure TjachLog.SetUseSeparateThreadToWrite(const Value: Boolean);
begin
  FCS.Enter;
  try
    if Value <> FUseSeparateThreadToWrite then
    begin
      if Value then
        FWriteThread := TjachLogWriteCoordinatorThread.Create(Self);
      FUseSeparateThreadToWrite := Value;
      if not Value then
        TerminateCoordinatorThread;
    end;
  finally
    FCS.Leave;
  end;
end;

procedure TjachLog.TerminateCoordinatorThread;
begin
  FWriteThread.Terminate;
  FWriteThread.WaitFor;
  FWriteThread.Free;
  FWriteThread := nil;
end;

procedure TjachLog.UnRegisterLogWriter(ALogWriter: TjachLogWriter);
var
  Writers: TList<TjachLogWriter>;
begin
  FCS.Enter;
  try
    Writers := FRegisteredLogWriters.LockList;
    try
      Writers.Remove(ALogWriter);
      if FUseSeparateThreadToWrite then
      begin
        ALogWriter.FThread.Terminate;
        ALogWriter.FThread.WaitFor; //todo: check if this is correct
      end;
    finally
      FRegisteredLogWriters.UnlockList;
    end;
  finally
    FCS.Leave;
  end;
end;

procedure TjachLog.WriteCachedLog;
var
  SavedIndentSpaces: string;
  LogEntry: IjachLogEntry;
  lRegisteredLogWriters: TList<TjachLogWriter>;
  Writer: TjachLogWriter;
begin
  if not FIsActive then
  begin
    FCacheCS.Enter;
    try
      FCache.EntryList.Clear;
    finally
      FCacheCS.Leave;
    end;
    Exit;
  end;
  SavedIndentSpaces := FIndentSpaces;
  try
    FIndentSpaces := '';
    try
      FCacheCS.Enter;
      try
        lRegisteredLogWriters := FRegisteredLogWriters.LockList;
        try
          for Writer in lRegisteredLogWriters do
          begin
            Writer.Lock.Enter;
            try
              try
                Writer.OpenLogChannel;
                try
                  Writer.Write(0, lsInfo, 'Cached LOG Write BEGIN ********************', '', GetCurrentThreadId, Now);
                  for LogEntry in FCache.EntryList do
                    Writer.WriteEntry(LogEntry);
                  Writer.Write(0, lsInfo, 'Cached LOG Write END **********************', '', GetCurrentThreadId, Now);
                finally
                  Writer.CloseLogChannel;
                end;
              except
                ;
              end;
            finally
              Writer.Lock.Leave;
            end;
          end;
        finally
          FRegisteredLogWriters.UnlockList;
        end;
      finally
        FCacheCS.Leave;
      end;
    except
      on E:Exception do
      begin
        FIndentSpaces := '';
        LogError('Cached LOG Write Error', E);
      end;
    end;
  finally
    FIndentSpaces := SavedIndentSpaces;
    FCache.EntryList.Clear;
  end;
end;

{ TjachLogWriteThread }

constructor TjachLogWriteCoordinatorThread.Create(ALog: TjachLog);
begin
  FLog := ALog;
  FEntryQueue := TThreadedQueue<IjachLogEntry>.Create(16384, 0, INFINITE);
  CreateWriterThreads;
  inherited Create(False);
end;

procedure TjachLogWriteCoordinatorThread.CreateWriterThreads;
var
  lWriterList: TList<TjachLogWriter>;
  lWriter: TjachLogWriter;
begin
  lWriterList := FLog.FRegisteredLogWriters.LockList;
  try
    for lWriter in lWriterList do
    begin
      lWriter.SetWriterThread(TjachLogWriterThread.Create(FLog, lWriter));
    end;
  finally
    FLog.FRegisteredLogWriters.UnlockList;
  end;
end;

destructor TjachLogWriteCoordinatorThread.Destroy;
begin
  FEntryQueue.Free;
  FreeWriterThreads;
  inherited;
end;

procedure TjachLogWriteCoordinatorThread.Execute;
var
  AEntry: IjachLogEntry;
  WriterList: TList<TjachLogWriter>;
  Writer: TjachLogWriter;
begin
  while not Terminated do
  begin
    case FEntryQueue.PopItem(AEntry) of
      wrSignaled:
        begin
          WriterList := FLog.FRegisteredLogWriters.LockList;
          try
            for Writer in WriterList do
              try
                TjachLogWriterThread(Writer.Thread).FEntryQueue.PushItem(AEntry);
              except
                //todo: fire error event
                ;
              end;
          finally
            FLog.FRegisteredLogWriters.UnlockList;
          end;
        end;
      wrTimeout: ;
      wrAbandoned: ;
      wrError: ;
      wrIOCompletion: ;
    end;
  end;
end;

procedure TjachLogWriteCoordinatorThread.FreeWriterThreads;
var
  lWriterList: TList<TjachLogWriter>;
  lWriter: TjachLogWriter;
begin
  lWriterList := FLog.FRegisteredLogWriters.LockList;
  try
    for lWriter in lWriterList do
    begin
      if Assigned(lWriter.Thread) then
      begin
        lWriter.Thread.Terminate;
        lWriter.Thread.WaitFor;
        lWriter.Thread.Free;
        lWriter.SetWriterThread(nil);
      end;
    end;
  finally
    FLog.FRegisteredLogWriters.UnlockList;
  end;
end;

procedure TjachLogWriteCoordinatorThread.TerminatedSet;
begin
  inherited;
  FEntryQueue.DoShutDown;
end;

{ TjachLogWriterThread }

constructor TjachLogWriterThread.Create(ALog: TjachLog;
  AWriter: TjachLogWriter);
begin
  FLog := ALog;
  FWriter := AWriter;
  FEntryQueue := TThreadedQueue<IjachLogEntry>.Create(16384, 0, INFINITE);
  FTerminatedEvent := TEvent.Create();
  inherited Create(False);
end;

destructor TjachLogWriterThread.Destroy;
begin
  FEntryQueue.Free;
  FTerminatedEvent.Free;
  inherited;
end;

procedure TjachLogWriterThread.Execute;
begin
  inherited;
  while not Terminated do
  begin
    if     (not FEntryQueue.ShutDown)
       and (FEntryQueue.TotalItemsPushed > FEntryQueue.TotalItemsPopped) then
    begin
      FWriter.GetLock.Enter;
      try
        FWriter.OpenLogChannel;
        try
          while     (not Terminated)
                and (not FEntryQueue.ShutDown)
                and (FEntryQueue.TotalItemsPushed > FEntryQueue.TotalItemsPopped) do
          begin
            FWriter.WriteEntry(FEntryQueue.PopItem);
          end;
        finally
          FWriter.CloseLogChannel;
        end;
      finally
        FWriter.GetLock.Leave;
      end;
    end
    else
      WaitForSingleObject(FTerminatedEvent.Handle, 50);
  end;
end;

procedure TjachLogWriterThread.TerminatedSet;
begin
  inherited;
  FEntryQueue.DoShutDown;
  FTerminatedEvent.SetEvent;
end;

initialization
finalization
end.
