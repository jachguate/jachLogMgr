{ ******************************************************** }
{ **                                                    ** }
{ ** Multithreaded Log for Delphi                       ** }
{ **                                                    ** }
{ ** Author:                                            ** }
{ **     Juan Antonio Castillo H. (jachguate)           ** }
{ **                                                    ** }
{ ** Copyright (c) 2007-2023                            ** }
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

unit ujachLogToDB;

interface

uses
  UjachLogMgr, Data.DB;

type
  TjachLogToDB = class(TjachLogWriter)
  private
  protected
    FKeepConnected: Boolean;
    FIsFirstConnection: Boolean;
    FIsTableAutoCreated: Boolean;
    FIsConnectionAutoCreated: Boolean;
    FCanAutoCreateTable: Boolean;
    procedure SetCanAutoCreateTable(const Value: Boolean); virtual;
    procedure SetKeepConnected(const Value: Boolean); virtual;
    procedure SetIsTableAutoCreated(const Value: Boolean); virtual;
    function GetIsConnected: Boolean; virtual;
    procedure SetIsConnectionAutoCreated(const Value: Boolean); virtual;
    function GetConnection: TCustomConnection; virtual; abstract;
    procedure AutoCreateTable; virtual; abstract;
  protected
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure StartTransaction; virtual;
    procedure CommitTransaction; virtual;
    property KeepConnected: Boolean read FKeepConnected write SetKeepConnected;
    property CanAutoCreateTable: Boolean read FCanAutoCreateTable write SetCanAutoCreateTable;
    property IsTableAutoCreated: Boolean read FIsTableAutoCreated write SetIsTableAutoCreated;
    property IsConnected: Boolean read GetIsConnected;
    property IsConnectionAutoCreated: Boolean read FIsConnectionAutoCreated write SetIsConnectionAutoCreated;
  public
    procedure OpenLogChannel; override;
    procedure CloseLogChannel; override;
  public
    constructor Create(ADefaultTopicLevel: TLogLevel = llAll); override;
  end;

implementation

{ TjachLogToDB }

procedure TjachLogToDB.CloseLogChannel;
begin
  inherited;
  CommitTransaction;
  if not FKeepConnected then
    Disconnect;
end;

procedure TjachLogToDB.CommitTransaction;
begin
end;

procedure TjachLogToDB.Connect;
begin
  GetConnection.Open;
  if FIsFirstConnection and FIsTableAutoCreated then
    AutoCreateTable;
  FIsFirstConnection := False;
end;

constructor TjachLogToDB.Create;
begin
  inherited;
  FFriendlyName := 'Database';
  IsActive := False;
  FIsFirstConnection := True;
  FCanAutoCreateTable := False;
  FIsTableAutoCreated := False;
  FKeepConnected := True;
  FIsConnectionAutoCreated := False;
end;

procedure TjachLogToDB.Disconnect;
begin
  GetConnection.Close;
end;

function TjachLogToDB.GetIsConnected: Boolean;
begin
  Result := GetConnection.Connected;
end;

procedure TjachLogToDB.OpenLogChannel;
begin
  inherited;
  if not IsConnected then
    Connect;
  StartTransaction;
end;

procedure TjachLogToDB.SetCanAutoCreateTable(const Value: Boolean);
begin
  FCanAutoCreateTable := Value;
end;

procedure TjachLogToDB.SetIsConnectionAutoCreated(const Value: Boolean);
begin
  FIsConnectionAutoCreated := Value;
end;

procedure TjachLogToDB.SetIsTableAutoCreated(const Value: Boolean);
begin
  if FCanAutoCreateTable then
    FIsTableAutoCreated := Value;
end;

procedure TjachLogToDB.SetKeepConnected(const Value: Boolean);
begin
  FKeepConnected := Value;
end;

procedure TjachLogToDB.StartTransaction;
begin
end;


initialization
finalization
end.

