{ ******************************************************** }
{ **                                                    ** }
{ ** Basic multithreaded Log for Delphi                 ** }
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
uses Classes, SysUtils, System.Types, SyncObjs, System.Generics.Collections,
  ujachLogClasses;

{
  JACHLOG_LEVEL_
SysLog
Severity: emergency, critical, alert, error, warning, debug, informational, and notice.

FATAL, ERROR, WARN, INFO, DEBUG, TRACE, ALL, and OFF

ALL < TRACE < DEBUG < INFO < WARN < ERROR < FATAL < OFF.


  FATAL       emergency, critical
, ERROR       error,
, WARN        warning,
, INFO
, DEBUG       debug,
, TRACE       informational, ???

, alert, and notice.
}

type
  TjachLog = class
  strict private
    FCS: TCriticalSection;
    FRegisteredLoggers: TObjectList<TjachLogWriter>;
    FIndentSpaces: string;
    FCacheCS: TCriticalSection;
    FIsActive: Boolean;
    FDefaultTopic: TjachLogTopicIndex;
    FIsCached: Boolean;
    FLogLevel: array[TjachLogTopicIndex] of TLogLevel;
    FTopicName: array[TjachLogTopicIndex] of string;
    FCache: TLogCache;
    function GetExceptionStr(E: Exception): string;
    procedure CacheLog(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity; const S: string); inline;
  private
    FIncludeTopicName: Boolean;
    procedure SetIsCached(const Value: Boolean);
    function GetLogLevel(Index: TjachLogTopicIndex): TLogLevel;
    procedure SetLogLevel(Index: TjachLogTopicIndex; const Value: TLogLevel);
    function GetTopicName(Index: TjachLogTopicIndex): string;
    procedure SetTopicName(Index: TjachLogTopicIndex; const Value: string);
    procedure SetIncludeTopicName(const Value: Boolean);
  public
    constructor Create(ADefaultTopicLevel: TLogLevel = llInfo; ADefaultTopic: TjachLogTopicIndex = 0);
    destructor Destroy; override;

    property IsCached: Boolean read FIsCached write SetIsCached;
    property LogLevel[Index: TjachLogTopicIndex]: TLogLevel read GetLogLevel write SetLogLevel;
    property TopicName[Index: TjachLogTopicIndex]: string read GetTopicName write SetTopicName;
    property DefaultTopic: TjachLogTopicIndex read FDefaultTopic write FDefaultTopic;
    property IncludeTopicName: Boolean read FIncludeTopicName write SetIncludeTopicName;

    procedure IncIndent;
    procedure DecIndent;

    procedure RegisterLogWriter(ALogger: TjachLogWriter);
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
var
  Entry: TLogEntry;
begin
  Entry := TLogEntry.Create(ATopic, ALogSeverity, FIndentSpaces, S);
  try
    FCacheCS.Enter;
    try
      FCache.EntryList.Add(Entry);
    finally
      FCacheCS.Leave;
    end;
  except
    Entry.Free;
  end;
end;

constructor TjachLog.Create(ADefaultTopicLevel: TLogLevel = llInfo; ADefaultTopic: TjachLogTopicIndex = 0);
var
  I: Integer;
begin
  inherited Create;
  FCS := TCriticalSection.Create;
  FRegisteredLoggers := TObjectList<TjachLogWriter>.Create(True);
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
  FCacheCS.Free;
  FCS.Free;
  FRegisteredLoggers.Free;
  FCache.Free;
  inherited;
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
  Writer: TjachLogWriter;
begin
  if not FIsActive then
    Exit;

  if Byte(FLogLevel[ATopic]) > Byte(ALogSeverity) then
    if FIsCached then
      if FIncludeTopicName then
        CacheLog(ATopic, ALogSeverity, '[' + FTopicName[ATopic] + ']' + S)
      else
        CacheLog(ATopic, ALogSeverity, S)
    else
      for Writer in FRegisteredLoggers do
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

procedure TjachLog.RegisterLogWriter(ALogger: TjachLogWriter);
begin
  FRegisteredLoggers.Add(ALogger);
end;

procedure TjachLog.SetIncludeTopicName(const Value: Boolean);
begin
  FIncludeTopicName := Value;
end;

procedure TjachLog.SetIsCached(const Value: Boolean);
begin
  if FIsCached <> Value then
    FIsCached := Value;
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

procedure TjachLog.WriteCachedLog;
var
  SavedIndentSpaces: string;
  LogEntry: TLogEntry;
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
        for Writer in FRegisteredLoggers do
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

initialization
finalization
end.
