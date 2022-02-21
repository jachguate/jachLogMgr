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

unit ujachLogClasses;

interface
uses Classes, System.Types, System.Generics.Collections, System.SyncObjs;

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
    property Lock: TCriticalSection read GetLock;
    property Thread: TThread read FThread;
  end;

  TLogCache = class
  private
    FEntryList: TjachLogEntryList;
  public
    constructor Create;
    destructor Destroy; override;
    property EntryList: TjachLogEntryList read FEntryList;
  end;

function LogSeverityToStr(ALogSeverity: TLogSeverity): string;
function CreateLogEntry(ATopic: TjachLogTopicIndex; ASeverity: TLogSeverity; AIndent, ALogString: string): IjachLogEntry;

implementation

uses
  System.SysUtils, Winapi.Windows;

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
  FLock := TCriticalSection.Create;
end;

destructor TjachLogWriter.Destroy;
begin
  FLock.Free;
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FThread.Free;
  end;
  inherited;
end;

function TjachLogWriter.GetLock: TCriticalSection;
begin
  Result := FLock;
end;

procedure TjachLogWriter.OpenLogChannel;
begin

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


end.
