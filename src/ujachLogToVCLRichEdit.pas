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

unit ujachLogToVCLRichEdit;

interface
uses ujachLogMgr, Vcl.Graphics, Vcl.ExtCtrls, Vcl.ComCtrls, Generics.Collections;


type
  TjachLogToVCLRichEdit = class(TjachLogWriter)
  private
    FEntries: TThreadedQueue<IjachLogEntry>;
    FRichEdit: TRichEdit;
    FTimer: TTimer;
    FFontColor: array[TLogSeverity] of TColor;
    FFontStyle: array[TLogSeverity] of TFontStyles;
    FMaxLength: Integer;
    FCurrentLineLength: Integer;
    FDateTimeFormat: string;
    FMessagesAdded: Boolean;
    procedure SetRichEdit(const Value: TRichEdit);
    procedure TimerEvent(Sender: TObject);
    function GetRefreshInterval: Cardinal;
    procedure SetRefreshInterval(const Value: Cardinal);
    procedure CreateTimer;
    procedure FreeTimer;
    procedure ActivateTimer;
    function GetIsMainThread: Boolean;
    function GetFontColor(ASeverity: TLogSeverity): TColor;
    function GetFontStyle(ASeverity: TLogSeverity): TFontStyles;
    procedure SetFontColor(ASeverity: TLogSeverity; const Value: TColor);
    procedure SetFontStyle(ASeverity: TLogSeverity; const Value: TFontStyles);
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
    constructor Create;
    destructor Destroy; override;
    property RichEdit: TRichEdit read FRichEdit write SetRichEdit;
    property RefreshInterval: Cardinal read GetRefreshInterval write SetRefreshInterval;
    property FontColor[ASeverity: TLogSeverity]: TColor read GetFontColor write SetFontColor;
    property FontStyle[ASeverity: TLogSeverity]: TFontStyles read GetFontStyle write SetFontStyle;
    property MaxLength: Integer read FMaxLength write SetMaxLength;
    property DateTimeFormat: string read FDateTimeFormat write SetDateTimeFormat;
  end;


implementation

uses
  System.Types, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms,
  Winapi.Windows;

{ TjachLogToVCLRichEdit }

procedure TjachLogToVCLRichEdit.ActivateTimer;
begin
  FTimer.Enabled := True;
end;

procedure TjachLogToVCLRichEdit.CloseLogChannel;
begin
  inherited;
  if GetIsMainThread then
  begin
    FRichEdit.Lines.EndUpdate;
    if FMessagesAdded then
    begin
      FRichEdit.CaretPos := Point(0, FRichEdit.Lines.Count - 1);
      FMessagesAdded := False;
      FRichEdit.Update;
    end;
  end;
end;

constructor TjachLogToVCLRichEdit.Create;
begin
  inherited Create;
  FEntries := TThreadedQueue<IjachLogEntry>.Create(32768, 0, 0);
  IsActive := False;
  if GetIsMainThread then
    CreateTimer
  else
    TThread.Synchronize(TThread.Current, CreateTimer);


  FFontColor[lsEmergency] := RGB(128, 32, 32); //very dark red
  FFontColor[lsAlert]     := RGB(192, 32, 32);     //dark red
  FFontColor[lsCritical]  := RGB(224, 16, 16);  //low dark red
  FFontColor[lsError]     := clRed;
  FFontColor[lsWarning]   := RGB(224, 64, 0); //Dark Orange;
  FFontColor[lsNotice]    := RGB(255, 128, 64); //Orange;
  FFontColor[lsInfo]      := clBlue;
  FFontColor[lsDebug]     := RGB(128, 128, 128);  //Gray

  FFontStyle[lsEmergency] := [fsBold];
  FFontStyle[lsAlert]     := [fsItalic];
  FFontStyle[lsCritical]  := [fsItalic];
  FFontStyle[lsError]     := [fsBold];
  FFontStyle[lsWarning]   := [fsItalic];
  FFontStyle[lsNotice]    := [];
  FFontStyle[lsInfo]      := [];
  FFontStyle[lsDebug]     := [fsItalic];

  FDateTimeFormat := 'yyyy-mm-dd hh:nn:ss:zzz';
  FMaxLength := -1;
end;

procedure TjachLogToVCLRichEdit.CreateTimer;
begin
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 500;
  FTimer.OnTimer := TimerEvent;
end;

destructor TjachLogToVCLRichEdit.Destroy;
begin
  if GetIsMainThread then
    FreeTimer
  else
    TThread.Synchronize(TThread.Current, FreeTimer);
  FEntries.Free;
  inherited;
end;

procedure TjachLogToVCLRichEdit.FreeTimer;
begin
  FTimer.Free;
end;

function TjachLogToVCLRichEdit.GetFontColor(ASeverity: TLogSeverity): TColor;
begin
  Result := FFontColor[ASeverity];
end;

function TjachLogToVCLRichEdit.GetFontStyle(
  ASeverity: TLogSeverity): TFontStyles;
begin
  Result := FFontStyle[ASeverity];
end;

function TjachLogToVCLRichEdit.GetIsMainThread: Boolean;
begin
  Result := TThread.Current.ThreadID = MainThreadID;
end;

function TjachLogToVCLRichEdit.GetRefreshInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TjachLogToVCLRichEdit.OpenLogChannel;
begin
  inherited;
  if GetIsMainThread then
  begin
    FMessagesAdded := False;
    FRichEdit.Lines.BeginUpdate;
  end;
end;

procedure TjachLogToVCLRichEdit.SetDateTimeFormat(const Value: string);
begin
  FDateTimeFormat := Value;
end;

procedure TjachLogToVCLRichEdit.SetFontColor(ASeverity: TLogSeverity;
  const Value: TColor);
begin
  FFontColor[ASeverity] := Value;
end;

procedure TjachLogToVCLRichEdit.SetFontStyle(ASeverity: TLogSeverity;
  const Value: TFontStyles);
begin
  FFontStyle[ASeverity] := Value;
end;

procedure TjachLogToVCLRichEdit.SetMaxLength(const Value: Integer);
begin
  FMaxLength := Value;
  FCurrentLineLength := FMaxLength;
end;

procedure TjachLogToVCLRichEdit.SetRefreshInterval(const Value: Cardinal);
begin
  FTimer.Interval := Value;
end;

procedure TjachLogToVCLRichEdit.SetRichEdit(const Value: TRichEdit);
begin
  FRichEdit := Value;
  if Assigned(FRichEdit) then
    if GetIsMainThread then
      ActivateTimer
    else
      TThread.Synchronize(TThread.Current, ActivateTimer);
end;

procedure TjachLogToVCLRichEdit.TimerEvent(Sender: TObject);
  function FindParentForm(AControl: TWinControl): TCustomForm;
  begin
    while Assigned(AControl) and (not (AControl is TCustomForm))  do
      AControl := AControl.Parent;
    if Assigned(AControl) and (AControl is TCustomForm) then
      Result := TCustomForm(AControl)
    else
      Result := nil;
  end;

  function MeasureFontWidth: Integer;
  var
    lForm: TCustomForm;
  begin
    lForm := FindParentForm(FRichEdit);
    if Assigned(lForm) then
    begin
      lForm.Canvas.Font := FRichEdit.Font;
      Result := lForm.Canvas.TextWidth('W');
    end
    else
      Result := 20;
  end;

  procedure CalculateCurrentLength;
  begin
    FCurrentLineLength := FRichEdit.Width div MeasureFontWidth;
  end;

var
  lEntry: IjachLogEntry;
  IsModified: Boolean;
begin
  IsModified := False;
  if not Assigned(FRichEdit) then
    Exit;

  if FMaxLength = -1 then
    CalculateCurrentLength;

  FRichEdit.Lines.BeginUpdate;
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
    FRichEdit.Lines.EndUpdate;
  end;
  if IsModified then
  begin
    FRichEdit.CaretPos := Point(0, FRichEdit.Lines.Count - 1);
    FRichEdit.Update;
  end;
  //AdjustToTheEnd;
end;

procedure TjachLogToVCLRichEdit.Write(ATopic: TjachLogTopicIndex;
  ASeverity: TLogSeverity; const S, AIndentSpaces: string;
  const AThreadID: TThreadID; const ATimeStamp: TDateTime);

var
  DT, Margin: string;
  Msgs: TStringDynArray;
  I, LineLength: Integer;
begin
  //inherited;
  if not GetIsMainThread then Exit;

  FRichEdit.SelStart := MaxInt;
  FRichEdit.SelAttributes.Color := FFontColor[ASeverity];
  FRichEdit.SelAttributes.Style := FFontStyle[ASeverity];

  DT := Format('%s %8.8x %-5s %s', [FormatDateTime(FDateTimeFormat, Now),
    AThreadID, LogSeverityToStr(ASeverity), AIndentSpaces]);
  Margin := StringOfChar(' ', Length(DT));

  LineLength := FCurrentLineLength - Length(DT);

  if LineLength < 20 then
    LineLength := 20;

  Msgs := WordWrap(S, LineLength);

  FRichEdit.Lines.Add(DT + ' ' + Msgs[0]);
  for I := 1 to High(Msgs) do
  begin
    FRichEdit.SelStart := MaxInt;
    FRichEdit.SelAttributes.Color := FFontColor[ASeverity];
    FRichEdit.SelAttributes.Style := FFontStyle[ASeverity];
    FRichEdit.Lines.Add(Margin + ' ' + Msgs[I]);
  end;
end;

procedure TjachLogToVCLRichEdit.WriteEntry(AEntry: IjachLogEntry);
begin
  //inherited;
  if Assigned(AEntry) then
    case FEntries.PushItem(AEntry) of
      wrSignaled, wrTimeout, wrAbandoned, wrError, wrIOCompletion: ;
    end;
end;

end.
