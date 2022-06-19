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

unit ujachLogToConsole;

interface

{$define AutoRegisterjachLogToDisk}

uses
  UjachLogMgr, System.SyncObjs;

type
  TjachLogToConsole = class(TjachLogWriter)
  public
    procedure Write(ATopic: TjachLogTopicIndex; ASeverity: TLogSeverity;
      ADebugVerbosity: Byte; const S, AIndentSpaces: string;
      const AThreadID: TThreadID; const ATimeStamp: TDateTime); override;
  end;

implementation

uses
    System.IOUtils
  {$ifdef MSWindows}
  , Windows
  {$endif}
  , System.SysUtils
  , System.Types;

{ TjachLogToConsole }

procedure TjachLogToConsole.Write(ATopic: TjachLogTopicIndex;
  ASeverity: TLogSeverity; ADebugVerbosity: Byte; const S, AIndentSpaces: string;
  const AThreadID: TThreadID; const ATimeStamp: TDateTime);
var
  DT: string;
  Margin: string;
  Msgs: TStringDynArray;
  I: Integer;
  lpConsoleScreenBufferInfo: _CONSOLE_SCREEN_BUFFER_INFO;
  MaxWidth: Integer;
begin
  try
    if not GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), lpConsoleScreenBufferInfo) then
      RaiseLastOSError;
    if IsMultiThread then
      DT := Format('%s %.8x %-5s', [FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now)
        , GetCurrentThreadID
        , LogSeverityToStr(ASeverity)])
    else
      DT := Format('%s %-5s', [FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now)
        , LogSeverityToStr(ASeverity)]);

    Margin := StringOfChar(' ', Length(DT));
    MaxWidth := lpConsoleScreenBufferInfo.dwSize.X - Length(AIndentSpaces) - Length(DT) - 2;
    if MaxWidth < 10 then
      MaxWidth := 10;

    Msgs := WordWrap(S, MaxWidth);
    Writeln(DT + ' ' + AIndentSpaces + Msgs[0]);
    for I := 1 to High(Msgs) do
      Writeln(Margin + ' ' + AIndentSpaces + Msgs[I]);
  except
    on E: Exception do
      Writeln(E.Message);
    //no exceptions!
  end;
end;

initialization
finalization
end.
