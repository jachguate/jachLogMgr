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

unit ujachLogToSysLogIndyUDP;

interface

uses
  UjachLogMgr, IdBaseComponent, IdComponent, IdUDPBase, IdUDPClient,
  IdSysLogMessage, IdSysLog;

type
  TjachLogToSysLogIndyUDP = class(TjachLogWriter)
  private
    FIdSysLog: TIdSysLog;
    FIdSysLogMessage: TIdSysLogMessage;
  public
    procedure OpenLogChannel; override;
    procedure CloseLogChannel; override;
    procedure Write(ATopic: TjachLogTopicIndex; ASeverity: TLogSeverity;
      const S, AIndentSpaces: string; const AThreadID: TThreadID;
      const ATimeStamp: TDateTime); override;
  public
    constructor Create(ADefaultTopicLevel: TLogLevel = llAll); override;
    destructor Destroy; override;
    property idSysLog: TidSysLog read FIdSysLog;
    property idSysLogMessage: TIdSysLogMessage read FIdSysLogMessage;
  end;

implementation

uses
    System.SysUtils
  , System.Types
  , idGlobal;

{ TjachLogToSysLogIndyUDP }

procedure TjachLogToSysLogIndyUDP.CloseLogChannel;
begin
  if FIdSysLog.Active then
    FidSysLog.Active := False;
end;

constructor TjachLogToSysLogIndyUDP.Create;
begin
  inherited;
  FIdSysLog := TIdSysLog.Create(nil);
  FIdSysLogMessage := TIdSysLogMessage.Create(nil);

  FIdSysLogMessage.Facility := TIdSyslogFacility.sfUserLevel;
  FIdSysLogMessage.Msg.PID := idGlobal.CurrentProcessId;
  FIdSysLogMessage.Msg.PIDAvailable := True;
  FIdSysLogMessage.Msg.Process := ExtractFileName(ParamStr(0));
end;

destructor TjachLogToSysLogIndyUDP.Destroy;
begin
  FIdSysLogMessage.Free;
  FIdSysLog.Free;
  inherited;
end;

procedure TjachLogToSysLogIndyUDP.OpenLogChannel;
begin
  if not FIdSysLog.Active then
    FIdSysLog.Active := True;
end;

procedure TjachLogToSysLogIndyUDP.Write(ATopic: TjachLogTopicIndex;
  ASeverity: TLogSeverity; const S, AIndentSpaces: string;
  const AThreadID: TThreadID; const ATimeStamp: TDateTime);
begin
  FIdSysLogMessage.TimeStamp := ATimeStamp;
  FIdSysLogMessage.Severity := TIdSyslogSeverity(ASeverity);
  FIdSysLogMessage.Msg.Content := S;
  FIdSysLog.SendLogMessage(FIdSysLogMessage, False);
end;

initialization
finalization
end.

