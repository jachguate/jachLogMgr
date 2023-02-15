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

{
Use the following symbols to tune the logging class interface
If your project doesn't want any call with a explicit topic, undefine
the AllowLogWithoutTopic symbol.

If you have a large codebase, during the transition you can maintain
the AllowLogWithoutTopic defined, and also define DeprecateCallsToLogWithoutTopic
so the compiler will emit a warning at every place you issue a call
without a excplicit topic.

When you're done transitioning you can undefine the AllowLogWithoutTopic symbo.

On the other hand, if your project is small and there's no topics at all,
and you don't want to see too much options in the code completion,
you can undefine the AllowLogWithTopic symbol, so you will only keep
the simplest calls available.
}
{$define AllowLogWithoutTopic}
{.$define DeprecateCallsToLogWithoutTopic}
{$define AllowLogWithTopic}
{
The AllowLogDebugWithVerbosity/AllowLogDebugWithoutVerbosity defines are mutually
exclusive. Only one of the two should be defined at any time, if both are
defined the library will keep AllowLogDebugWithVerbosity and ignore
AllowLogDebugWithoutVerbosity
}
{.$define AllowLogDebugWithVerbosity}
{$define AllowLogDebugWithoutVerbosity}
unit ujachLogMgr;

interface

uses Classes, System.SysUtils, System.Types, System.SyncObjs,
  System.Generics.Collections;

//adjust your symbols before the unit declaration, do not touch this part
{$ifdef ForceLogWithoutTopic}
  {$define AllowLogWithoutTopic}
  {$undef AllowLogWithTopic}
{$endif}
{$ifdef ForceLogWithTopic}
  {$undef AllowLogWithoutTopic}
  {$define AllowLogWithTopic}
{$endif}
{$ifdef ForceLogDebugWithoutVerbosity}
  {$undef AllowLogDebugWithVerbosity}
  {$define AllowLogDebugWithoutVerbosity}
{$endif}
{$ifdef ForceLogDebugWithVerbosity}
  {$define AllowLogDebugWithVerbosity}
  {$undef AllowLogDebugWithoutVerbosity}
{$endif}

{$ifndef AllowLogWithTopic}
  {$undef DeprecateCallsToLogWithoutTopic}
{$endif}
{$if not defined(AllowLogWithoutTopic) and not defined(AllowLogWithTopic)}
  {$define AllowLogWithoutTopic}
{$ifend}
{$if not defined(AllowLogDebugWithVerbosity) and not defined(AllowLogDebugWithoutVerbosity)}
  {$define AllowLogDebugWithoutVerbosity}
{$ifend}

{$if defined(AllowLogDebugWithVerbosity) and defined(AllowLogDebugWithoutVerbosity)}
  {$undef AllowLogDebugWithoutVerbosity}
{$ifend}


type
  TjachLogTopicIndex = Byte;

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
    function GetDebugVerbosity: Byte;
    property LogString: string read GetLogString;
    property Indent: string read GetIndent;
    property ThreadID: TThreadID read GetThreadID;
    property TimeStamp: TDateTime read GetTimeStamp;
    property Severity: TLogSeverity read GetSeverity;
    property Topic: TjachLogTopicIndex read GetTopic;
    property DebugVerbosity: Byte read GetDebugVerbosity;
  end;

  TjachLogEntryList = class(TList<IjachLogEntry>)
  end;

  TjachLogWriter = class
  private
    FLock: TCriticalSection;
    FThread: TThread;
    FIsActive: Boolean;
    FLogLevel: array[TjachLogTopicIndex] of TLogLevel;
    FDebugVerbosityThreshold: Byte;
    procedure SetIsActive(const Value: Boolean);
    function GetLogLevel(Index: TjachLogTopicIndex): TLogLevel;
    procedure SetLogLevel(Index: TjachLogTopicIndex; const Value: TLogLevel);
    procedure SetDebugVerbosityThreshold(const Value: Byte);
    procedure SetFriendlyName(const Value: string);
  protected
    const WWMAX_LEN = 255;
    var
      FFriendlyName: string;
    function WordWrap(const S: string; MaxLen: UInt16 = WWMAX_LEN): TStringDynArray; virtual;
  public
    constructor Create(ADefaultTopicLevel: TLogLevel = llAll); virtual;
    destructor Destroy; override;
    procedure OpenLogChannel; virtual;
    procedure CloseLogChannel; virtual;
    procedure Write(ATopic: TjachLogTopicIndex; ASeverity: TLogSeverity;
      ADebugVerbosity: Byte; const S, AIndentSpaces: string;
      const AThreadID: TThreadID; const ATimeStamp: TDateTime); virtual; abstract;
    procedure WriteEntry(AEntry: IjachLogEntry); virtual;
    function GetLock: TCriticalSection; virtual;
    procedure SetWriterThread(AThread: TThread);
    property IsActive: Boolean read FIsActive write SetIsActive;
    property Lock: TCriticalSection read GetLock;
    property Thread: TThread read FThread;
    property LogLevel[Index: TjachLogTopicIndex]: TLogLevel read GetLogLevel write SetLogLevel;
    property DebugVerbosityThreshold: Byte read FDebugVerbosityThreshold write SetDebugVerbosityThreshold;
    property FriendlyName: string read FFriendlyName write SetFriendlyName;
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
    FDebugVerbosityThreshold: Byte;
    function GetExceptionStr(E: Exception): string;
    procedure CacheLog(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity; ADebugVerbosity: Byte; const S: string); inline;
    procedure SetIsCached(const Value: Boolean);
    function GetLogLevel(Index: TjachLogTopicIndex): TLogLevel;
    procedure SetLogLevel(Index: TjachLogTopicIndex; const Value: TLogLevel);
    function GetTopicName(Index: TjachLogTopicIndex): string;
    procedure SetTopicName(Index: TjachLogTopicIndex; const Value: string);
    procedure SetIncludeTopicName(const Value: Boolean);
    procedure SetUseSeparateThreadToWrite(const Value: Boolean);
    procedure SetDebugVerbosityThreshold(const Value: Byte);
    procedure FreeRegisteredLogWriters;
    procedure TerminateCoordinatorThread;
    procedure InternalLog(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity; ADebugVerbosity: Byte; const S: string); overload;
    procedure InternalLog(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity; ADebugVerbosity: Byte; E: Exception); overload; inline;
    procedure InternalLog(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity; ADebugVerbosity: Byte; const ExtraMsg: string; E: Exception); overload; inline;
  public
    constructor Create(ADefaultTopicLevel: TLogLevel = llInfo; ADefaultTopic: TjachLogTopicIndex = 0);
    destructor Destroy; override;

    property IsCached: Boolean read FIsCached write SetIsCached;
    property DebugVerbosityThreshold: Byte read FDebugVerbosityThreshold write SetDebugVerbosityThreshold;
    property LogLevel[Index: TjachLogTopicIndex]: TLogLevel read GetLogLevel write SetLogLevel;
    property TopicName[Index: TjachLogTopicIndex]: string read GetTopicName write SetTopicName;
    property DefaultTopic: TjachLogTopicIndex read FDefaultTopic write FDefaultTopic;
    property IncludeTopicName: Boolean read FIncludeTopicName write SetIncludeTopicName;
    property IsActive: Boolean read FIsActive write FIsActive;
    property UseSeparateThreadToWrite: Boolean read FUseSeparateThreadToWrite write SetUseSeparateThreadToWrite;

    procedure IncIndent;
    procedure DecIndent;

    procedure RegisterLogWriter(ALogWriter: TjachLogWriter);
    procedure UnRegisterLogWriter(ALogWriter: TjachLogWriter);
    function FindLogWriterByClass(ALogWriterClass: TjachLogWriterClass): TjachLogWriter;
    function GetLogWriterByClass(ALogWriterClass: TjachLogWriterClass): TjachLogWriter;

    function GetIndentSpaces: string; inline;

    {$ifdef AllowLogWithTopic}
    procedure Log(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity; const S: string); overload; inline;
    procedure Log(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity; const S: string; const Args: array of const); overload;
    procedure Log(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity; E: Exception); overload; inline;
    procedure Log(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity; const ExtraMsg: string; E: Exception); overload; inline;
    procedure Log(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity; const S: string; const Args: array of const; E: Exception); overload;
    {$endif}
    {$ifdef AllowLogWithoutTopic}
    procedure Log(ALogSeverity: TLogSeverity; const S: string); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure Log(ALogSeverity: TLogSeverity; const S: string; const Args: array of const); overload; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure Log(ALogSeverity: TLogSeverity; E: Exception); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure Log(ALogSeverity: TLogSeverity; const ExtraMsg: string; E: Exception); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure Log(ALogSeverity: TLogSeverity; const S: string; const Args: array of const; E: Exception); overload; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    {$endif}
    {$ifdef AllowLogWithTopic}
    procedure LogEmergency(ATopic: TjachLogTopicIndex; const S: string); overload; inline;
    procedure LogEmergency(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const); overload;
    procedure LogEmergency(ATopic: TjachLogTopicIndex; E: Exception); overload; inline;
    procedure LogEmergency(ATopic: TjachLogTopicIndex; const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogEmergency(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const; E: Exception); overload;
    {$endif}
    {$ifdef AllowLogWithoutTopic}
    procedure LogEmergency(const S: string); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogEmergency(const S: string; const Args: array of const); overload; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogEmergency(E: Exception); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogEmergency(const ExtraMsg: string; E: Exception); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogEmergency(const S: string; const Args: array of const; E: Exception); overload; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    {$endif}
    {$ifdef AllowLogWithTopic}
    procedure LogAlert(ATopic: TjachLogTopicIndex; const S: string); overload; inline;
    procedure LogAlert(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const); overload;
    procedure LogAlert(ATopic: TjachLogTopicIndex; E: Exception); overload; inline;
    procedure LogAlert(ATopic: TjachLogTopicIndex; const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogAlert(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const; E: Exception); overload;
    {$endif}
    {$ifdef AllowLogWithoutTopic}
    procedure LogAlert(const S: string); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogAlert(const S: string; const Args: array of const); overload; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogAlert(E: Exception); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogAlert(const ExtraMsg: string; E: Exception); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogAlert(const S: string; const Args: array of const; E: Exception); overload; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    {$endif}
    {$ifdef AllowLogWithTopic}
    procedure LogCritical(ATopic: TjachLogTopicIndex; const S: string); overload; inline;
    procedure LogCritical(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const); overload;
    procedure LogCritical(ATopic: TjachLogTopicIndex; E: Exception); overload; inline;
    procedure LogCritical(ATopic: TjachLogTopicIndex; const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogCritical(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const; E: Exception); overload;
    {$endif}
    {$ifdef AllowLogWithoutTopic}
    procedure LogCritical(const S: string); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogCritical(const S: string; const Args: array of const); overload; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogCritical(E: Exception); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogCritical(const ExtraMsg: string; E: Exception); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogCritical(const S: string; const Args: array of const; E: Exception); overload; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    {$endif}
    {$ifdef AllowLogWithTopic}
    procedure LogError(ATopic: TjachLogTopicIndex; const S: string); overload; inline;
    procedure LogError(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const); overload;
    procedure LogError(ATopic: TjachLogTopicIndex; E: Exception); overload; inline;
    procedure LogError(ATopic: TjachLogTopicIndex; const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogError(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const; E: Exception); overload;
    {$endif}
    {$ifdef AllowLogWithoutTopic}
    procedure LogError(const S: string); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogError(const S: string; const Args: array of const); overload; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogError(E: Exception); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogError(const ExtraMsg: string; E: Exception); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogError(const S: string; const Args: array of const; E: Exception); overload; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    {$endif}
    {$ifdef AllowLogWithTopic}
    procedure LogWarning(ATopic: TjachLogTopicIndex; const S: string); overload; inline;
    procedure LogWarning(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const); overload;
    procedure LogWarning(ATopic: TjachLogTopicIndex; E: Exception); overload; inline;
    procedure LogWarning(ATopic: TjachLogTopicIndex; const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogWarning(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const; E: Exception); overload;
    {$endif}
    {$ifdef AllowLogWithoutTopic}
    procedure LogWarning(const S: string); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogWarning(const S: string; const Args: array of const); overload; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogWarning(E: Exception); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogWarning(const ExtraMsg: string; E: Exception); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogWarning(const S: string; const Args: array of const; E: Exception); overload; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    {$endif}
    {$ifdef AllowLogWithTopic}
    procedure LogNotice(ATopic: TjachLogTopicIndex; const S: string); overload; inline;
    procedure LogNotice(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const); overload;
    procedure LogNotice(ATopic: TjachLogTopicIndex; E: Exception); overload; inline;
    procedure LogNotice(ATopic: TjachLogTopicIndex; const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogNotice(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const; E: Exception); overload;
    {$endif}
    {$ifdef AllowLogWithoutTopic}
    procedure LogNotice(const S: string); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogNotice(const S: string; const Args: array of const); overload; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogNotice(E: Exception); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogNotice(const ExtraMsg: string; E: Exception); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogNotice(const S: string; const Args: array of const; E: Exception); overload; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    {$endif}
    {$ifdef AllowLogWithTopic}
    procedure LogInfo(ATopic: TjachLogTopicIndex; const S: string); overload; inline;
    procedure LogInfo(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const); overload;
    procedure LogInfo(ATopic: TjachLogTopicIndex; E: Exception); overload; inline;
    procedure LogInfo(ATopic: TjachLogTopicIndex; const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogInfo(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const; E: Exception); overload;
    {$endif}
    {$ifdef AllowLogWithoutTopic}
    procedure LogInfo(const S: string); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogInfo(const S: string; const Args: array of const); overload; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogInfo(E: Exception); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogInfo(const ExtraMsg: string; E: Exception); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogInfo(const S: string; const Args: array of const; E: Exception); overload; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    {$endif}
    {$if defined(AllowLogWithTopic) and defined(AllowLogDebugWithoutVerbosity)}
    procedure LogDebug(ATopic: TjachLogTopicIndex; const S: string); overload; inline;
    procedure LogDebug(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const); overload;
    procedure LogDebug(ATopic: TjachLogTopicIndex; E: Exception); overload; inline;
    procedure LogDebug(ATopic: TjachLogTopicIndex; const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogDebug(ATopic: TjachLogTopicIndex; const S: string; const Args: array of const; E: Exception); overload;
    {$ifend}
    {$if defined(AllowLogWithoutTopic) and defined(AllowLogDebugWithoutVerbosity)}
    procedure LogDebug(const S: string); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogDebug(const S: string; const Args: array of const); overload; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogDebug(E: Exception); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogDebug(const ExtraMsg: string; E: Exception); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogDebug(const S: string; const Args: array of const; E: Exception); overload; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    {$ifend}
    {$if defined(AllowLogWithTopic) and defined(AllowLogDebugWithVerbosity)}
    procedure LogDebug(ATopic: TjachLogTopicIndex; AVerbosity: Byte; const S: string); overload; inline;
    procedure LogDebug(ATopic: TjachLogTopicIndex; AVerbosity: Byte; const S: string; const Args: array of const); overload;
    procedure LogDebug(ATopic: TjachLogTopicIndex; AVerbosity: Byte; E: Exception); overload; inline;
    procedure LogDebug(ATopic: TjachLogTopicIndex; AVerbosity: Byte; const ExtraMsg: string; E: Exception); overload; inline;
    procedure LogDebug(ATopic: TjachLogTopicIndex; AVerbosity: Byte; const S: string; const Args: array of const; E: Exception); overload;
    {$endif}
    {$if defined(AllowLogWithoutTopic) and defined(AllowLogDebugWithVerbosity)}
    procedure LogDebug(AVerbosity: Byte; const S: string); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogDebug(AVerbosity: Byte; const S: string; const Args: array of const); overload; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogDebug(AVerbosity: Byte; E: Exception); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogDebug(AVerbosity: Byte; const ExtraMsg: string; E: Exception); overload; inline; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    procedure LogDebug(AVerbosity: Byte; const S: string; const Args: array of const; E: Exception); overload; {$ifdef DeprecateCallsToLogWithoutTopic}deprecated 'Add a topic to your call to Logging';{$endif}
    {$endif}
    procedure CacheClear;
    procedure WriteCachedLog;
  end;

  EjachLogError = class(Exception)
  end;

  EjachLogNoWriterRegistered = class(EjachLogError)
  end;

function LogSeverityToStr(ALogSeverity: TLogSeverity): string;
function CreateLogEntry(ATopic: TjachLogTopicIndex; ASeverity: TLogSeverity; ADebugVerbosity: Byte; AIndent, ALogString: string): IjachLogEntry; overload;
function CreateLogEntry(ATopic: TjachLogTopicIndex; ASeverity: TLogSeverity; ADebugVerbosity: Byte; AIndent, ALogString: string; AThreadID: TThreadID; ATimeStamp: TDateTime): IjachLogEntry; overload;

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
    FDebugVerbosity: Byte;
    function GetLogString: string;
    function GetIndent: string;
    function GetThreadID: TThreadID;
    function GetTimeStamp: TDateTime;
    function GetSeverity: TLogSeverity;
    function GetTopic: TjachLogTopicIndex;
    function GetDebugVerbosity: Byte;
  public
    constructor Create(ATopic: TjachLogTopicIndex; ASeverity: TLogSeverity; ADebugVerbosity: Byte; AIndent, ALogString: string); overload;
    constructor Create(ATopic: TjachLogTopicIndex; ASeverity: TLogSeverity; ADebugVerbosity: Byte; AIndent, ALogString: string; AThreadID: TThreadID; ATimeStamp: TDateTime); overload;
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

function CreateLogEntry(ATopic: TjachLogTopicIndex; ASeverity: TLogSeverity; ADebugVerbosity: Byte; AIndent, ALogString: string): IjachLogEntry;
begin
  Result := TLogEntry.Create(ATopic, ASeverity, ADebugVerbosity, AIndent, ALogString);
end;

function CreateLogEntry(ATopic: TjachLogTopicIndex; ASeverity: TLogSeverity; ADebugVerbosity: Byte; AIndent, ALogString: string; AThreadID: TThreadID; ATimeStamp: TDateTime): IjachLogEntry; overload;
begin
  Result := TLogEntry.Create(ATopic, ASeverity, ADebugVerbosity, AIndent, ALogString, AThreadID, ATimeStamp);
end;

{ TLogEntry }

constructor TLogEntry.Create(ATopic: TjachLogTopicIndex;
  ASeverity: TLogSeverity; ADebugVerbosity: Byte; AIndent, ALogString: string);
begin
  inherited Create;
  FTimeStamp := Now;
  FIndent := AIndent;
  FLogString := ALogString;
  FSeverity := ASeverity;
  FThreadID := GetCurrentThreadId;
  FTopic := ATopic;
  FDebugVerbosity := ADebugVerbosity;
end;

constructor TLogEntry.Create(ATopic: TjachLogTopicIndex;
  ASeverity: TLogSeverity; ADebugVerbosity: Byte; AIndent, ALogString: string;
  AThreadID: TThreadID; ATimeStamp: TDateTime);
begin
  Create(ATopic, ASeverity, ADebugVerbosity, AIndent, ALogString);
  FThreadID := AThreadID;
  FTimeStamp := ATimeStamp;
end;

function TLogEntry.GetDebugVerbosity: Byte;
begin
  Result := FDebugVerbosity;
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
var
  I: Integer;
begin
  inherited Create;
  FIsActive := True;
  FLock := TCriticalSection.Create;
  for I := Low(FLogLevel) to High(FLogLevel) do
    FLogLevel[I] := ADefaultTopicLevel;
  FFriendlyName := Self.ClassName;
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

function TjachLogWriter.GetLogLevel(Index: TjachLogTopicIndex): TLogLevel;
begin
  Result := FLogLevel[Index];
end;

procedure TjachLogWriter.OpenLogChannel;
begin

end;

procedure TjachLogWriter.SetDebugVerbosityThreshold(const Value: Byte);
begin
  FDebugVerbosityThreshold := Value;
end;

procedure TjachLogWriter.SetFriendlyName(const Value: string);
begin
  FFriendlyName := Value;
end;

procedure TjachLogWriter.SetIsActive(const Value: Boolean);
begin
  FIsActive := Value;
end;

procedure TjachLogWriter.SetLogLevel(Index: TjachLogTopicIndex;
  const Value: TLogLevel);
begin
  FLogLevel[Index] := Value;
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
    Write(AEntry.Topic, AEntry.Severity, AEntry.DebugVerbosity, AEntry.LogString, AEntry.Indent, AEntry.ThreadID, AEntry.TimeStamp);
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

procedure TjachLog.CacheLog(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity; ADebugVerbosity: Byte; const S: string);
begin
  FCacheCS.Enter;
  try
    FCache.EntryList.Add(CreateLogEntry (ATopic, ALogSeverity, ADebugVerbosity, FIndentSpaces, S));
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

procedure TjachLog.InternalLog(ATopic: TjachLogTopicIndex;
  ALogSeverity: TLogSeverity; ADebugVerbosity: Byte; const S: string);
var
  lRegisteredLogWriters: TList<TjachLogWriter>;
  Writer: TjachLogWriter;
begin
  if not FIsActive then
    Exit;

  if     (Byte(FLogLevel[ATopic]) > Byte(ALogSeverity))
     and ((ALogSeverity <> lsDebug) or (ADebugVerbosity <= FDebugVerbosityThreshold))
  then
    if FIsCached then
      if FIncludeTopicName then
        CacheLog(ATopic, ALogSeverity, ADebugVerbosity, '[' + FTopicName[ATopic] + ']' + S)
      else
        CacheLog(ATopic, ALogSeverity, ADebugVerbosity, S)
    else if FUseSeparateThreadToWrite then
      if FIncludeTopicName then
        TjachLogWriteCoordinatorThread(FWriteThread).FEntryQueue.PushItem(CreateLogEntry (ATopic, ALogSeverity, ADebugVerbosity, FIndentSpaces, '[' + FTopicName[ATopic] + ']' + S))
      else
        TjachLogWriteCoordinatorThread(FWriteThread).FEntryQueue.PushItem(CreateLogEntry (ATopic, ALogSeverity, ADebugVerbosity, FIndentSpaces, S))
    else
    begin
      lRegisteredLogWriters := FRegisteredLogWriters.LockList;
      try
        for Writer in lRegisteredLogWriters do
          if     (Writer.IsActive)
             and (Byte(Writer.FLogLevel[ATopic]) > Byte(ALogSeverity))
             and ((ALogSeverity <> lsDebug) or (ADebugVerbosity <= Writer.FDebugVerbosityThreshold))
          then
            try
              Writer.Lock.Enter;
              try
                Writer.OpenLogChannel;
                try
                  if FIncludeTopicName then
                    Writer.Write(ATopic, ALogSeverity, ADebugVerbosity, '[' + FTopicName[ATopic] + ']' + S, FIndentSpaces, GetCurrentThreadID, Now)
                  else
                    Writer.Write(ATopic, ALogSeverity, ADebugVerbosity, S, FIndentSpaces, GetCurrentThreadID, Now);
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

procedure TjachLog.InternalLog(ATopic: TjachLogTopicIndex;
  ALogSeverity: TLogSeverity; ADebugVerbosity: Byte; E: Exception);
begin
  InternalLog(ATopic, ALogSeverity, ADebugVerbosity, GetExceptionStr(E));
end;

procedure TjachLog.InternalLog(ATopic: TjachLogTopicIndex;
  ALogSeverity: TLogSeverity; ADebugVerbosity: Byte; const ExtraMsg:
  string; E: Exception);
begin
  InternalLog(ATopic, ALogSeverity, ADebugVerbosity, ExtraMsg + #13 + GetExceptionStr(E));
end;

{$ifdef AllowLogWithTopic}
procedure TjachLog.Log(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity; const S: string);
begin
  InternalLog(ATopic, ALogSeverity, 0{DebugVerbosity}, S);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.Log(ALogSeverity: TLogSeverity; const S: string);
begin
  InternalLog(FDefaultTopic, ALogSeverity, 0{DebugVerbosity}, S);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogEmergency(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const; E: Exception);
begin
  Log(ATopic, lsEmergency, Format(S, Args), E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogEmergency(ATopic: TjachLogTopicIndex;
  const ExtraMsg: string; E: Exception);
begin
  Log(ATopic, lsEmergency, ExtraMsg, E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogEmergency(ATopic: TjachLogTopicIndex; E: Exception);
begin
  Log(ATopic, lsEmergency, E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogEmergency(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const);
begin
  Log(ATopic, lsEmergency, Format(S, Args));
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogEmergency(ATopic: TjachLogTopicIndex; const S: string);
begin
  Log(ATopic, lsEmergency, S);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogEmergency(const S: string; const Args: array of const;
  E: Exception);
begin
  InternalLog(FDefaultTopic, lsEmergency, 0{DebugVerbosity}, Format(S, Args), E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogEmergency(const ExtraMsg: string; E: Exception);
begin
  InternalLog(FDefaultTopic, lsEmergency, 0{DebugVerbosity}, ExtraMsg, E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogEmergency(E: Exception);
begin
  InternalLog(FDefaultTopic, lsEmergency, 0{DebugVerbosity}, E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogEmergency(const S: string);
begin
  InternalLog(FDefaultTopic, lsEmergency, 0{DebugVerbosity}, S);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogEmergency(const S: string; const Args: array of const);
begin
  InternalLog(FDefaultTopic, lsEmergency, 0{DebugVerbosity}, Format(S, Args));
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogAlert(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const; E: Exception);
begin
  Log(ATopic, lsAlert, Format(S, Args), E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogAlert(ATopic: TjachLogTopicIndex;
  const ExtraMsg: string; E: Exception);
begin
  Log(ATopic, lsAlert, ExtraMsg, E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogAlert(ATopic: TjachLogTopicIndex; E: Exception);
begin
  Log(ATopic, lsAlert, E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogAlert(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const);
begin
  Log(ATopic, lsAlert, Format(S, Args));
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogAlert(ATopic: TjachLogTopicIndex; const S: string);
begin
  Log(ATopic, lsAlert, S);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogAlert(const S: string; const Args: array of const;
  E: Exception);
begin
  InternalLog(FDefaultTopic, lsAlert, 0{DebugVerbosity}, Format(S, Args), E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogAlert(const ExtraMsg: string; E: Exception);
begin
  InternalLog(FDefaultTopic, lsAlert, 0{DebugVerbosity}, ExtraMsg, E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogAlert(E: Exception);
begin
  InternalLog(FDefaultTopic, lsAlert, 0{DebugVerbosity}, E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogAlert(const S: string);
begin
  InternalLog(FDefaultTopic, lsAlert, 0{DebugVerbosity}, S);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogAlert(const S: string; const Args: array of const);
begin
  InternalLog(FDefaultTopic, lsAlert, 0{DebugVerbosity}, Format(S, Args));
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogCritical(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const; E: Exception);
begin
  Log(ATopic, lsCritical, Format(S, Args), E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogCritical(ATopic: TjachLogTopicIndex;
  const ExtraMsg: string; E: Exception);
begin
  Log(ATopic, lsCritical, ExtraMsg, E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogCritical(ATopic: TjachLogTopicIndex; E: Exception);
begin
  Log(ATopic, lsCritical, E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogCritical(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const);
begin
  Log(ATopic, lsCritical, Format(S, Args));
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogCritical(ATopic: TjachLogTopicIndex; const S: string);
begin
  Log(ATopic, lsCritical, S);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogCritical(const S: string; const Args: array of const;
  E: Exception);
begin
  InternalLog(FDefaultTopic, lsCritical, 0{DebugVerbosity}, Format(S, Args), E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogCritical(const ExtraMsg: string; E: Exception);
begin
  InternalLog(FDefaultTopic, lsCritical, 0{DebugVerbosity}, ExtraMsg, E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogCritical(E: Exception);
begin
  InternalLog(FDefaultTopic, lsCritical, 0{DebugVerbosity}, E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogCritical(const S: string);
begin
  InternalLog(FDefaultTopic, lsCritical, 0{DebugVerbosity}, S);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogCritical(const S: string; const Args: array of const);
begin
  InternalLog(FDefaultTopic, lsCritical, 0{DebugVerbosity}, Format(S, Args));
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogError(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const; E: Exception);
begin
  Log(ATopic, lsError, Format(S, Args), E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogError(ATopic: TjachLogTopicIndex;
  const ExtraMsg: string; E: Exception);
begin
  Log(ATopic, lsError, ExtraMsg, E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogError(ATopic: TjachLogTopicIndex; E: Exception);
begin
  Log(ATopic, lsError, E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogError(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const);
begin
  Log(ATopic, lsError, Format(S, Args));
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogError(ATopic: TjachLogTopicIndex; const S: string);
begin
  Log(ATopic, lsError, S);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogError(const S: string; const Args: array of const;
  E: Exception);
begin
  InternalLog(FDefaultTopic, lsError, 0{DebugVerbosity}, Format(S, Args), E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogError(const ExtraMsg: string; E: Exception);
begin
  InternalLog(FDefaultTopic, lsError, 0{DebugVerbosity}, ExtraMsg, E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogError(E: Exception);
begin
  InternalLog(FDefaultTopic, lsError, 0{DebugVerbosity}, E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogError(const S: string);
begin
  InternalLog(FDefaultTopic, lsError, 0{DebugVerbosity}, S);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogError(const S: string; const Args: array of const);
begin
  InternalLog(FDefaultTopic, lsError, 0{DebugVerbosity}, Format(S, Args));
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogWarning(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const; E: Exception);
begin
  Log(ATopic, lsWarning, Format(S, Args), E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogWarning(ATopic: TjachLogTopicIndex;
  const ExtraMsg: string; E: Exception);
begin
  Log(ATopic, lsWarning, ExtraMsg, E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogWarning(ATopic: TjachLogTopicIndex; E: Exception);
begin
  Log(ATopic, lsWarning, E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogWarning(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const);
begin
  Log(ATopic, lsWarning, Format(S, Args));
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogWarning(ATopic: TjachLogTopicIndex; const S: string);
begin
  Log(ATopic, lsWarning, S);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogWarning(const S: string; const Args: array of const;
  E: Exception);
begin
  InternalLog(FDefaultTopic, lsWarning, 0{DebugVerbosity}, Format(S, Args), E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogWarning(const ExtraMsg: string; E: Exception);
begin
  InternalLog(FDefaultTopic, lsWarning, 0{DebugVerbosity}, ExtraMsg, E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogWarning(E: Exception);
begin
  InternalLog(FDefaultTopic, lsWarning, 0{DebugVerbosity}, E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogWarning(const S: string);
begin
  InternalLog(FDefaultTopic, lsWarning, 0{DebugVerbosity}, S);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogWarning(const S: string; const Args: array of const);
begin
  InternalLog(FDefaultTopic, lsWarning, 0{DebugVerbosity}, Format(S, Args));
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogNotice(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const; E: Exception);
begin
  Log(ATopic, lsNotice, Format(S, Args), E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogNotice(ATopic: TjachLogTopicIndex;
  const ExtraMsg: string; E: Exception);
begin
  Log(ATopic, lsNotice, ExtraMsg, E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogNotice(ATopic: TjachLogTopicIndex; E: Exception);
begin
  Log(ATopic, lsNotice, E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogNotice(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const);
begin
  Log(ATopic, lsNotice, Format(S, Args));
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogNotice(ATopic: TjachLogTopicIndex; const S: string);
begin
  Log(ATopic, lsNotice, S);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogNotice(const S: string; const Args: array of const;
  E: Exception);
begin
  InternalLog(FDefaultTopic, lsNotice, 0{DebugVerbosity}, Format(S, Args), E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogNotice(const ExtraMsg: string; E: Exception);
begin
  InternalLog(FDefaultTopic, lsNotice, 0{DebugVerbosity}, ExtraMsg, E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogNotice(E: Exception);
begin
  InternalLog(FDefaultTopic, lsNotice, 0{DebugVerbosity}, E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogNotice(const S: string);
begin
  InternalLog(FDefaultTopic, lsNotice, 0{DebugVerbosity}, S);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogNotice(const S: string; const Args: array of const);
begin
  InternalLog(FDefaultTopic, lsNotice, 0{DebugVerbosity}, Format(S, Args));
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogInfo(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const; E: Exception);
begin
  Log(ATopic, lsInfo, Format(S, Args), E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogInfo(ATopic: TjachLogTopicIndex;
  const ExtraMsg: string; E: Exception);
begin
  Log(ATopic, lsInfo, ExtraMsg, E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogInfo(ATopic: TjachLogTopicIndex; E: Exception);
begin
  Log(ATopic, lsInfo, E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogInfo(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const);
begin
  Log(ATopic, lsInfo, Format(S, Args));
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.LogInfo(ATopic: TjachLogTopicIndex; const S: string);
begin
  Log(ATopic, lsInfo, S);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogInfo(const S: string; const Args: array of const;
  E: Exception);
begin
  InternalLog(FDefaultTopic, lsInfo, 0{DebugVerbosity}, Format(S, Args), E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogInfo(const ExtraMsg: string; E: Exception);
begin
  InternalLog(FDefaultTopic, lsInfo, 0{DebugVerbosity}, ExtraMsg, E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogInfo(E: Exception);
begin
  InternalLog(FDefaultTopic, lsInfo, 0{DebugVerbosity}, E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogInfo(const S: string);
begin
  InternalLog(FDefaultTopic, lsInfo, 0{DebugVerbosity}, S);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.LogInfo(const S: string; const Args: array of const);
begin
  InternalLog(FDefaultTopic, lsInfo, 0{DebugVerbosity}, Format(S, Args));
end;
{$endif}

{$if defined(AllowLogWithTopic) and defined(AllowLogDebugWithoutVerbosity)}
procedure TjachLog.LogDebug(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const; E: Exception);
begin
  Log(ATopic, lsDebug, Format(S, Args), E);
end;
{$ifend}

{$if defined(AllowLogWithTopic) and defined(AllowLogDebugWithoutVerbosity)}
procedure TjachLog.LogDebug(ATopic: TjachLogTopicIndex;
  const ExtraMsg: string; E: Exception);
begin
  Log(ATopic, lsDebug, ExtraMsg, E);
end;
{$ifend}

{$if defined(AllowLogWithTopic) and defined(AllowLogDebugWithoutVerbosity)}
procedure TjachLog.LogDebug(ATopic: TjachLogTopicIndex; E: Exception);
begin
  Log(ATopic, lsDebug, E);
end;
{$ifend}

{$if defined(AllowLogWithTopic) and defined(AllowLogDebugWithoutVerbosity)}
procedure TjachLog.LogDebug(ATopic: TjachLogTopicIndex; const S: string;
  const Args: array of const);
begin
  Log(ATopic, lsDebug, Format(S, Args));
end;
{$ifend}

{$if defined(AllowLogWithTopic) and defined(AllowLogDebugWithoutVerbosity)}
procedure TjachLog.LogDebug(ATopic: TjachLogTopicIndex; const S: string);
begin
  Log(ATopic, lsDebug, S);
end;
{$ifend}

{$if defined(AllowLogWithoutTopic) and defined(AllowLogDebugWithoutVerbosity)}
procedure TjachLog.LogDebug(const S: string; const Args: array of const;
  E: Exception);
begin
  InternalLog(FDefaultTopic, lsDebug, 0{DebugVerbosity}, Format(S, Args), E);
end;
{$ifend}

{$if defined(AllowLogWithoutTopic) and defined(AllowLogDebugWithoutVerbosity)}
procedure TjachLog.LogDebug(const ExtraMsg: string; E: Exception);
begin
  InternalLog(FDefaultTopic, lsDebug, 0{DebugVerbosity}, ExtraMsg, E);
end;
{$ifend}

{$if defined(AllowLogWithoutTopic) and defined(AllowLogDebugWithoutVerbosity)}
procedure TjachLog.LogDebug(E: Exception);
begin
  InternalLog(FDefaultTopic, lsDebug, 0{DebugVerbosity}, E);
end;
{$ifend}

{$if defined(AllowLogWithoutTopic) and defined(AllowLogDebugWithoutVerbosity)}
procedure TjachLog.LogDebug(const S: string);
begin
  InternalLog(FDefaultTopic, lsDebug, 0{DebugVerbosity}, S);
end;
{$ifend}

{$if defined(AllowLogWithoutTopic) and defined(AllowLogDebugWithoutVerbosity)}
procedure TjachLog.LogDebug(const S: string; const Args: array of const);
begin
  InternalLog(FDefaultTopic, lsDebug, 0{DebugVerbosity}, Format(S, Args));
end;
{$ifend}

{$if defined(AllowLogWithTopic) and defined(AllowLogDebugWithVerbosity)}
procedure TjachLog.LogDebug(ATopic: TjachLogTopicIndex; AVerbosity: Byte;
  const S: string; const Args: array of const; E: Exception);
begin
  InternalLog(ATopic, lsDebug, AVerbosity, Format(S, Args), E);
end;
{$ifend}

{$if defined(AllowLogWithTopic) and defined(AllowLogDebugWithVerbosity)}
procedure TjachLog.LogDebug(ATopic: TjachLogTopicIndex; AVerbosity: Byte;
  const ExtraMsg: string; E: Exception);
begin
  InternalLog(ATopic, lsDebug, AVerbosity, ExtraMsg, E);
end;
{$ifend}

{$if defined(AllowLogWithTopic) and defined(AllowLogDebugWithVerbosity)}
procedure TjachLog.LogDebug(ATopic: TjachLogTopicIndex; AVerbosity: Byte;
  E: Exception);
begin
  InternalLog(ATopic, lsDebug, AVerbosity, E);
end;
{$ifend}

{$if defined(AllowLogWithTopic) and defined(AllowLogDebugWithVerbosity)}
procedure TjachLog.LogDebug(ATopic: TjachLogTopicIndex; AVerbosity: Byte;
  const S: string; const Args: array of const);
begin
  InternalLog(ATopic, lsDebug, AVerbosity, Format(S, Args));
end;
{$ifend}

{$if defined(AllowLogWithTopic) and defined(AllowLogDebugWithVerbosity)}
procedure TjachLog.LogDebug(ATopic: TjachLogTopicIndex; AVerbosity: Byte;
  const S: string);
begin
  InternalLog(ATopic, lsDebug, AVerbosity, S);
end;
{$ifend}

{$if defined(AllowLogWithoutTopic) and defined(AllowLogDebugWithVerbosity)}
procedure TjachLog.LogDebug(AVerbosity: Byte; const S: string;
  const Args: array of const;
  E: Exception);
begin
  InternalLog(FDefaultTopic, lsDebug, AVerbosity, Format(S, Args), E);
end;
{$ifend}

{$if defined(AllowLogWithoutTopic) and defined(AllowLogDebugWithVerbosity)}
procedure TjachLog.LogDebug(AVerbosity: Byte; const ExtraMsg: string;
  E: Exception);
begin
  InternalLog(FDefaultTopic, lsDebug, AVerbosity, ExtraMsg, E);
end;
{$ifend}

{$if defined(AllowLogWithoutTopic) and defined(AllowLogDebugWithVerbosity)}
procedure TjachLog.LogDebug(AVerbosity: Byte; E: Exception);
begin
  InternalLog(FDefaultTopic, lsDebug, AVerbosity, E);
end;
{$ifend}

{$if defined(AllowLogWithoutTopic) and defined(AllowLogDebugWithVerbosity)}
procedure TjachLog.LogDebug(AVerbosity: Byte; const S: string);
begin
  InternalLog(FDefaultTopic, lsDebug, AVerbosity, S);
end;
{$ifend}

{$if defined(AllowLogWithoutTopic) and defined(AllowLogDebugWithVerbosity)}
procedure TjachLog.LogDebug(AVerbosity: Byte; const S: string;
  const Args: array of const);
begin
  InternalLog(FDefaultTopic, lsDebug, AVerbosity, Format(S, Args));
end;
{$ifend}

{$ifdef AllowLogWithTopic}
procedure TjachLog.Log(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity;
  const ExtraMsg: string; E: Exception);
begin
  Log(ATopic, ALogSeverity,  ExtraMsg + #13 + GetExceptionStr(E));
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.Log(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity;
  const S: string; const Args: array of const; E: Exception);
begin
  Log(ATopic, ALogSeverity, Format(S, Args), E);
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.Log(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity;
  E: Exception);
begin
  Log(ATopic, ALogSeverity, GetExceptionStr(E));
end;
{$endif}

{$ifdef AllowLogWithTopic}
procedure TjachLog.Log(ATopic: TjachLogTopicIndex; ALogSeverity: TLogSeverity;
  const S: string; const Args: array of const);
begin
  Log(ATopic, ALogSeverity, Format(S, Args));
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.Log(ALogSeverity: TLogSeverity; const S: string;
  const Args: array of const; E: Exception);
begin
  InternalLog(FDefaultTopic, ALogSeverity, 0{DebugVerbosity}, Format(S, Args), E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.Log(ALogSeverity: TLogSeverity; const ExtraMsg: string;
  E: Exception);
begin
  InternalLog(FDefaultTopic, ALogSeverity, 0{DebugVerbosity}, ExtraMsg, E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.Log(ALogSeverity: TLogSeverity; E: Exception);
begin
  InternalLog(FDefaultTopic, ALogSeverity, 0{DebugVerbosity}, E);
end;
{$endif}

{$ifdef AllowLogWithoutTopic}
procedure TjachLog.Log(ALogSeverity: TLogSeverity; const S: string;
  const Args: array of const);
begin
  InternalLog(FDefaultTopic, ALogSeverity, 0{DebugVerbosity}, Format(S, Args));
end;
{$endif}

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

procedure TjachLog.SetDebugVerbosityThreshold(const Value: Byte);
begin
  FDebugVerbosityThreshold := Value;
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
  procedure WriteDirect;
  var
    SavedIndentSpaces: string;
    lRegisteredLogWriters: TList<TjachLogWriter>;
    Writer: TjachLogWriter;
    LogEntry: IjachLogEntry;
  begin
    SavedIndentSpaces := FIndentSpaces;
    try
      FIndentSpaces := '';
      FCacheCS.Enter;
      try
        lRegisteredLogWriters := FRegisteredLogWriters.LockList;
        try
          for Writer in lRegisteredLogWriters do
          begin
            if Writer.IsActive then
              try
                Writer.Lock.Enter;
                try
                  Writer.OpenLogChannel;
                  try
                    Writer.Write(0, lsInfo, 0{Verbosity}, 'Cached LOG Write BEGIN ********************', '', GetCurrentThreadId, Now);
                    for LogEntry in FCache.EntryList do
                      if     (Byte(Writer.FLogLevel[LogEntry.Topic]) > Byte(LogEntry.Severity))
                         and ((LogEntry.Severity <> lsDebug) or (LogEntry.DebugVerbosity <= Writer.FDebugVerbosityThreshold))
                      then
                        Writer.WriteEntry(LogEntry);
                    Writer.Write(0, lsInfo, 0{Verbosity}, 'Cached LOG Write END **********************', '', GetCurrentThreadId, Now);
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
        finally
          FRegisteredLogWriters.UnlockList;
        end;
        FCache.EntryList.Clear;
      finally
        FCacheCS.Leave;
      end;
    finally
      FIndentSpaces := SavedIndentSpaces;
    end;
  end;

  procedure WriteToCoordinator;
  var
    LogEntry: IjachLogEntry;
  begin
    FCacheCS.Enter;
    try
      TjachLogWriteCoordinatorThread(FWriteThread).FEntryQueue.PushItem(CreateLogEntry (0, lsInfo, 0{DebugVerbosity}, '', 'Cached LOG Write BEGIN ********************'));
      for LogEntry in FCache.EntryList do
        TjachLogWriteCoordinatorThread(FWriteThread).FEntryQueue.PushItem(LogEntry);
      TjachLogWriteCoordinatorThread(FWriteThread).FEntryQueue.PushItem(CreateLogEntry (0, lsInfo, 0{DebugVerbosity}, '', 'Cached LOG Write END ********************'));
      FCache.EntryList.Clear;
    finally
      FCacheCS.Leave;
    end;
  end;
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
  try
    if FUseSeparateThreadToWrite then
      WriteToCoordinator
    else
      WriteDirect;
  except
    on E:Exception do
    begin
      FIndentSpaces := '';
      InternalLog(FDefaultTopic, lsNotice, 0{DebugVerbosity}, 'Cached LOG Write Error', E);
    end;
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
  {$ifdef debug}NameThreadForDebugging('Log coordinator thread');{$endif}
  while not Terminated do
  begin
    case FEntryQueue.PopItem(AEntry) of
      wrSignaled:
        begin
          if Terminated then Exit;
          WriterList := FLog.FRegisteredLogWriters.LockList;
          try
            for Writer in WriterList do
              if     (not Terminated)
                 and (Writer.IsActive)
                 and (Byte(Writer.LogLevel[AEntry.Topic]) > Byte(AEntry.Severity))
              then
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
  {$ifdef debug}NameThreadForDebugging(FWriter.ClassName + ' writer thread');{$endif}
  while not Terminated do
  begin
    if     (not FEntryQueue.ShutDown)
       and (FEntryQueue.QueueSize > 0) then
    begin
      FWriter.GetLock.Enter;
      try
        FWriter.OpenLogChannel;
        try
          while     (not Terminated)
                and (not FEntryQueue.ShutDown)
                and (FEntryQueue.QueueSize > 0) do
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
  if Assigned(jachLog) then
    FreeAndNil(jachLog);
end.
