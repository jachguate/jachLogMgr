unit ujachLogClasses;

interface
uses Classes, System.Types, System.Generics.Collections, System.SyncObjs;

type
  TjachLogTopicIndex = 0..63;

  TLogSeverity = (          lsEmergency{0}, lsAlert{1}, lsCritical{2}, lsError{3}, lsWarning{4}, lsNotice{5}, lsInfo{6}, lsDebug{7});
  TLogLevel    = (llOff{0}, llEmergency{1}, llAlert{2}, llCritical{3}, llError{4}, llWarning{5}, llNotice{6}, llInfo{7}, llDebug{8}, llAll{9});

  TLogEntry = class
  private
    FLogString: string;
    FIndent: string;
    FThreadID: TThreadID;
    FTimeStamp: TDateTime;
    FSeverity: TLogSeverity;
    FTopic: TjachLogTopicIndex;
  public
    constructor Create(ATopic: TjachLogTopicIndex; ASeverity: TLogSeverity; AIndent, ALogString: string);
    property LogString: string read FLogString;
    property Indent: string read FIndent;
    property ThreadID: TThreadID read FThreadID;
    property TimeStamp: TDateTime read FTimeStamp;
    property Severity: TLogSeverity read FSeverity;
    property Topic: TjachLogTopicIndex read FTopic;
  end;

  TLogEntryList = class(TObjectList<TLogEntry>)
  end;

  TjachLogWriter = class
  protected
    const
      WWMAX_LEN = 255;
    function WordWrap(const S: string; MaxLen: UInt16 = WWMAX_LEN): TStringDynArray; virtual;
  public
    procedure OpenLogChannel; virtual; abstract;
    procedure CloseLogChannel; virtual; abstract;
    procedure Write(ATopic: TjachLogTopicIndex; ASeverity: TLogSeverity;
      const S, AIndentSpaces: string; const AThreadID: TThreadID;
      const ATimeStamp: TDateTime); virtual; abstract;
    procedure WriteEntry(AEntry: TLogEntry); virtual;
    function GetLock: TCriticalSection; virtual; abstract;
  public
    property Lock: TCriticalSection read GetLock;
  end;

  TLogCache = class
  private
    FEntryList: TLogEntryList;
  public
    constructor Create;
    destructor Destroy; override;
    property EntryList: TLogEntryList read FEntryList;
  end;

function LogSeverityToStr(ALogSeverity: TLogSeverity): string;

implementation

uses
  System.SysUtils, Winapi.Windows;

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

{ TjachLogWriter }

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

procedure TjachLogWriter.WriteEntry(AEntry: TLogEntry);
begin
  Write(AEntry.Topic, AEntry.Severity, AEntry.LogString, AEntry.Indent, AEntry.ThreadID, AEntry.TimeStamp);
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


end.
