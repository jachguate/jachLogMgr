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

unit ujachLogTodbExpress;

interface

uses
  System.Classes, UjachLogMgr, UjachLogToDB, Data.DB, Data.DBXCommon, Data.SqlExpr;

type
  TSQLConnectionEvent = procedure(AConnection: TSQLConnection) of object;

  TjachLogTodbExpress = class(TjachLogToDB)
  protected
    FConnection: TSQLConnection;
    FInsertLogQuery: TSQLQuery;
    FOnConfigureConnection: TSQLConnectionEvent;
    FVendorLib: string;
    FDriverName: string;
    FGetDriverFunc: string;
    FLibraryName: string;
    FParams: TStrings;
    FInsertSQL: TStrings;
    FTransaction: TDBXTransaction;
  private
    procedure SetOnConfigureConnection(const Value: TSQLConnectionEvent);
    procedure SetDriverName(const Value: string);
    procedure SetGetDriverFunc(const Value: string);
    procedure SetLibraryName(const Value: string);
    procedure SetVendorLib(const Value: string);
    procedure SetInsertSQL(const Value: TStrings);
    procedure SetParams(const Value: TStrings);
  protected
    function GetConnection: TCustomConnection; override;
    function GetSQLConnection: TSQLConnection; virtual;
    procedure StartTransaction; override;
    procedure CommitTransaction; override;
    //procedure AutoCreateStructure; virtual; abstract;
    procedure CreateConnection; virtual;
    procedure ConnectionBeforeConnect(Sender: TObject); virtual;
    procedure ConnectionAfterConnect(Sender: TObject); virtual;
    procedure PrepareInsertLogQuery; virtual;
    procedure ConfigureInsertParameters; virtual;
    procedure Connect; override;
  public
    constructor Create(ADefaultTopicLevel: TLogLevel = llAll); override;
    destructor Destroy; override;
    property Connection: TSQLConnection read GetSQLConnection;
    property OnConfigureConnection: TSQLConnectionEvent read FOnConfigureConnection write SetOnConfigureConnection;
    property DriverName: string read FDriverName write SetDriverName;
    property GetDriverFunc: string read FGetDriverFunc write SetGetDriverFunc;
    property LibraryName: string read FLibraryName write SetLibraryName;
    property VendorLib: string read FVendorLib write SetVendorLib;
    property Params: TStrings read FParams write SetParams;
    property InsertSQL: TStrings read FInsertSQL write SetInsertSQL;
  end;

implementation

{ TjachLogTodbExpress }

procedure TjachLogTodbExpress.CommitTransaction;
begin
  inherited;
  GetSQLConnection.CommitFreeAndNil(FTransaction);
end;

procedure TjachLogTodbExpress.ConfigureInsertParameters;
begin

end;

procedure TjachLogTodbExpress.Connect;
begin
  inherited;
  PrepareInsertLogQuery;
end;

procedure TjachLogTodbExpress.ConnectionAfterConnect(Sender: TObject);
begin
  if not Assigned(FInsertLogQuery) then
  begin
    FInsertLogQuery := TSQLQuery.Create(FConnection);
    FInsertLogQuery.SQLConnection := FConnection;
  end;
end;

procedure TjachLogTodbExpress.ConnectionBeforeConnect(Sender: TObject);
var
  Conn: TSQLConnection;
  I: Integer;
  ParamName: string;
begin
  Conn := Sender as TSQLConnection;
  if FDriverName <> '' then
    Conn.DriverName := FDriverName;
  if FGetDriverFunc <> '' then
    Conn.GetDriverFunc := FGetDriverFunc;
  if FLibraryName <> '' then
    Conn.LibraryName := FLibraryName;
  if FVendorLib <> '' then
    Conn.VendorLib := FVendorLib;
  for I := 0 to FParams.Count - 1 do
  begin
    ParamName := FParams.Names[I];
    if (ParamName <> '') then
      Conn.Params.Values[ParamName] := FParams.Values[ParamName];
  end;
  if Assigned(FOnConfigureConnection) then
    FOnConfigureConnection(Sender as TSQLConnection);
end;

constructor TjachLogTodbExpress.Create(ADefaultTopicLevel: TLogLevel);
begin
  inherited;
  FParams := TStringList.Create;
  FInsertSQL := TStringList.Create;
end;

procedure TjachLogTodbExpress.CreateConnection;
begin
  FConnection := TSQLConnection.Create(nil);
  FConnection.LoginPrompt := False;
  FConnection.BeforeConnect := ConnectionBeforeConnect;
  FConnection.AfterConnect := ConnectionAfterConnect;
end;

destructor TjachLogTodbExpress.Destroy;
begin
  FParams.Free;
  FInsertSQL.Free;
  FConnection.Free;
  inherited;
end;

function TjachLogTodbExpress.GetConnection: TCustomConnection;
begin
  Result := GetSQLConnection;
end;

function TjachLogTodbExpress.GetSQLConnection: TSQLConnection;
begin
  if not Assigned(FConnection) then
    CreateConnection;
  Result := FConnection;
end;

procedure TjachLogTodbExpress.PrepareInsertLogQuery;
begin
  if not FInsertLogQuery.Prepared then
  begin
    FInsertLogQuery.SQL.Text := FInsertSQL.Text;
    ConfigureInsertParameters;
    FInsertLogQuery.Prepared := True;
  end;
end;

procedure TjachLogTodbExpress.SetDriverName(const Value: string);
begin
  FDriverName := Value;
end;

procedure TjachLogTodbExpress.SetGetDriverFunc(const Value: string);
begin
  FGetDriverFunc := Value;
end;

procedure TjachLogTodbExpress.SetInsertSQL(const Value: TStrings);
begin
  if Assigned(Value) then
    FInsertSQL.Assign(Value)
  else
    FInsertSQL.Clear;
end;

procedure TjachLogTodbExpress.SetLibraryName(const Value: string);
begin
  FLibraryName := Value;
end;

procedure TjachLogTodbExpress.SetOnConfigureConnection(
  const Value: TSQLConnectionEvent);
begin
  FOnConfigureConnection := Value;
end;

procedure TjachLogTodbExpress.SetParams(const Value: TStrings);
begin
  if Assigned(Value) then
    FParams.Assign(Value)
  else
    FParams.Clear;
end;

procedure TjachLogTodbExpress.SetVendorLib(const Value: string);
begin
  FVendorLib := Value;
end;

procedure TjachLogTodbExpress.StartTransaction;
begin
  inherited;
  FTransaction := GetSQLConnection.BeginTransaction;
end;

initialization
finalization
end.
