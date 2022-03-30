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

unit ujachLogToFMXMemo;

interface
uses ujachLogMgr, System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  Generics.Collections;

type
  TjachLogToFMXMemo = class(TjachLogWriter)
  private
    FEntries: TThreadedQueue<IjachLogEntry>;
    FMemo: TMemo;
    FTimer: TTimer;
    FMaxLength: Integer;
    FCurrentLineLength: Integer;
    FDateTimeFormat: string;
    FMessagesAdded: Boolean;
    procedure SetMemo(const Value: TMemo);
    procedure TimerEvent(Sender: TObject);
    function GetRefreshInterval: Cardinal;
    procedure SetRefreshInterval(const Value: Cardinal);
    procedure CreateTimer;
    procedure FreeTimer;
    procedure ActivateTimer;
    function GetIsMainThread: Boolean;
    procedure SetMaxLength(const Value: Integer);
    procedure SetDateTimeFormat(const Value: string);
  public
    procedure OpenLogChannel; override;
    procedure CloseLogChannel; override;
    procedure WriteEntry(AEntry: IjachLogEntry); override;
    procedure Write(ATopic: TjachLogTopicIndex; ASeverity: TLogSeverity;
      const S, AIndentSpaces: string; const AThreadID: TThreadID;
      const ATimeStamp: TDateTime); override;
  public
    constructor Create(ADefaultTopicLevel: TLogLevel = llAll); override;
    destructor Destroy; override;
    property Memo: TMemo read FMemo write SeTMemo;
    property RefreshInterval: Cardinal read GetRefreshInterval write SetRefreshInterval;
    property MaxLength: Integer read FMaxLength write SetMaxLength;
    property DateTimeFormat: string read FDateTimeFormat write SetDateTimeFormat;
  end;


implementation

uses
  Winapi.Windows;

{ TjachLogToFMXMemo }

procedure TjachLogToFMXMemo.ActivateTimer;
begin
  FTimer.Enabled := True;
end;

procedure TjachLogToFMXMemo.CloseLogChannel;
begin
  inherited;
  if GetIsMainThread then
  begin
    FMemo.Lines.EndUpdate;
    if FMessagesAdded then
    begin
      FMemo.GoToTextEnd;
      FMessagesAdded := False;
    end;
  end;
end;

constructor TjachLogToFMXMemo.Create;
begin
  inherited;
  FEntries := TThreadedQueue<IjachLogEntry>.Create(32768, 0, 0);
  IsActive := False;
  if GetIsMainThread then
    CreateTimer
  else
    TThread.Synchronize(TThread.Current, CreateTimer);
  FDateTimeFormat := 'yyyy-mm-dd hh:nn:ss:zzz';
  FMaxLength := -1;
end;

procedure TjachLogToFMXMemo.CreateTimer;
begin
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 500;
  FTimer.OnTimer := TimerEvent;
end;

destructor TjachLogToFMXMemo.Destroy;
begin
  if GetIsMainThread then
    FreeTimer
  else
    TThread.Synchronize(TThread.Current, FreeTimer);
  FEntries.Free;
  inherited;
end;

procedure TjachLogToFMXMemo.FreeTimer;
begin
  FTimer.Free;
end;

function TjachLogToFMXMemo.GetIsMainThread: Boolean;
begin
  Result := TThread.Current.ThreadID = MainThreadID;
end;

function TjachLogToFMXMemo.GetRefreshInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TjachLogToFMXMemo.OpenLogChannel;
begin
  inherited;
  if GetIsMainThread then
  begin
    FMessagesAdded := False;
    FMemo.Lines.BeginUpdate;
  end;
end;

procedure TjachLogToFMXMemo.SetDateTimeFormat(const Value: string);
begin
  FDateTimeFormat := Value;
end;

procedure TjachLogToFMXMemo.SetMaxLength(const Value: Integer);
begin
  FMaxLength := Value;
  FCurrentLineLength := FMaxLength;
end;

procedure TjachLogToFMXMemo.SetRefreshInterval(const Value: Cardinal);
begin
  FTimer.Interval := Value;
end;

procedure TjachLogToFMXMemo.SetMemo(const Value: TMemo);
begin
  FMemo := Value;
  if Assigned(FMemo) then
    if GetIsMainThread then
      ActivateTimer
    else
      TThread.Synchronize(TThread.Current, ActivateTimer);
end;

procedure TjachLogToFMXMemo.TimerEvent(Sender: TObject);
  function FindParentForm(AControl: TFmxObject): TCustomForm;
  begin
    while Assigned(AControl) and (not (AControl is TCustomForm))  do
      AControl := AControl.Parent;
    if Assigned(AControl) and (AControl is TCustomForm) then
      Result := TCustomForm(AControl)
    else
      Result := nil;
  end;

  function MeasureFontWidth: Single;
  var
    lForm: TCustomForm;
  begin
    lForm := FindParentForm(FMemo);
    if Assigned(lForm) then
    begin
      lForm.Canvas.Font.Assign(FMemo.Font);
      Result := lForm.Canvas.TextWidth('W');
    end
    else
      Result := 20;
  end;

  procedure CalculateCurrentLength;
  begin
    FCurrentLineLength := Round(FMemo.Width / MeasureFontWidth) - 3;
  end;

var
  lEntry: IjachLogEntry;
  IsModified: Boolean;
begin
  IsModified := False;
  if not Assigned(FMemo) then
    Exit;

  if FMaxLength = -1 then
    CalculateCurrentLength;

  FMemo.Lines.BeginUpdate;
  try
    while FEntries.TotalItemsPushed > FEntries.TotalItemsPopped do
    begin
      lEntry := FEntries.PopItem;
      if Assigned(lEntry) then
      begin
        IsModified := True;
        Write(lEntry.Topic, lEntry.Severity, lEntry.LogString, lEntry.Indent, lEntry.ThreadID, lEntry.TimeStamp);
      end;
    end;
  finally
    FMemo.Lines.EndUpdate;
  end;
  if IsModified then
    FMemo.GoToTextEnd;
end;

procedure TjachLogToFMXMemo.Write(ATopic: TjachLogTopicIndex;
  ASeverity: TLogSeverity; const S, AIndentSpaces: string;
  const AThreadID: TThreadID; const ATimeStamp: TDateTime);

var
  DT, Margin: string;
  Msgs: TStringDynArray;
  I, LineLength: Integer;
begin
  //inherited;
  if not GetIsMainThread then Exit;

  DT := Format('%s %8.8x %-5s %s', [FormatDateTime(FDateTimeFormat, Now),
    AThreadID, LogSeverityToStr(ASeverity), AIndentSpaces]);
  Margin := StringOfChar(' ', Length(DT) + 1);

  LineLength := FCurrentLineLength - Length(DT);

  if LineLength < 20 then
    LineLength := 20;

  Msgs := WordWrap(S, LineLength);

  FMemo.Lines.Add(DT + ' ' + Msgs[0]);
  for I := 1 to High(Msgs) do
    FMemo.Lines.Add(Margin + Msgs[I]);
  FMessagesAdded := True;
end;

procedure TjachLogToFMXMemo.WriteEntry(AEntry: IjachLogEntry);
begin
  //inherited;
  if Assigned(AEntry) then
    case FEntries.PushItem(AEntry) of
      wrSignaled, wrTimeout, wrAbandoned, wrError, wrIOCompletion: ;
    end;
end;

end.
