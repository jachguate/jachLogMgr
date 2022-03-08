unit usvcDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs,
  Vcl.ExtCtrls;

type
  TsvcDemo = class(TService)
    tmrServiceRunning: TTimer;
    procedure ServiceBeforeInstall(Sender: TService);
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceBeforeUninstall(Sender: TService);
    procedure ServiceAfterUninstall(Sender: TService);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceShutdown(Sender: TService);
    procedure ServicePause(Sender: TService; var Paused: Boolean);
    procedure ServiceContinue(Sender: TService; var Continued: Boolean);
    procedure ServiceExecute(Sender: TService);
    procedure tmrServiceRunningTimer(Sender: TObject);
  private
    procedure SetServiceDescription;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  svcDemo: TsvcDemo;

implementation

uses
  ujachLogAuto, ujachLogMgr, System.Win.Registry;

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  svcDemo.Controller(CtrlCode);
end;

function TsvcDemo.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TsvcDemo.ServiceAfterInstall(Sender: TService);
begin
  SetServiceDescription;
  jachLog.LogInfo('Service installed succesfully');
end;

procedure TsvcDemo.ServiceAfterUninstall(Sender: TService);
begin
  jachLog.LogAlert('Service uninstalled succesfully');
end;

procedure TsvcDemo.ServiceBeforeInstall(Sender: TService);
begin
  jachLog.LogInfo('Service is being installed');
end;

procedure TsvcDemo.ServiceBeforeUninstall(Sender: TService);
begin
  jachLog.LogAlert('Service is being uninstalled');
end;

procedure TsvcDemo.ServiceContinue(Sender: TService; var Continued: Boolean);
begin
  try
    jachLog.LogAlert('Service is continuing');
    tmrServiceRunning.Enabled := True;
    Continued := True;
    jachLog.LogInfo('Service continued succesfully');
  except
    on E: Exception do
    begin
      Continued := False;
      jachLog.LogError('Error while continuing the service', E);
    end;
  end;
end;

procedure TsvcDemo.ServiceExecute(Sender: TService);
begin
  jachLog.LogAlert('Service is executing');
  while not Self.Terminated do
  begin
    Self.ServiceThread.ProcessRequests(false);
    sleep(30);
  end;
end;

procedure TsvcDemo.ServicePause(Sender: TService; var Paused: Boolean);
begin
  try
    jachLog.LogAlert('Service is pausing');
    tmrServiceRunning.Enabled := False;
    Paused := True;
    jachLog.LogInfo('Service is paused succesfully');
  except
    on E: Exception do
    begin
      Paused := False;
      jachLog.LogError('Error pausing the service', E);
    end;
  end;
end;

procedure TsvcDemo.ServiceShutdown(Sender: TService);
begin
  jachLog.LogAlert('Service shutdown');
end;

procedure TsvcDemo.ServiceStart(Sender: TService; var Started: Boolean);
begin
  try
    jachLog.LogInfo('Service starting');
    tmrServiceRunning.Enabled := True;
    Started := True;
    jachLog.LogInfo('Service started succesfully');
  except
    on E: Exception do
    begin
      Started := False;
      jachLog.LogCritical('Error starting the service', E);
    end;
  end;
end;

procedure TsvcDemo.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  try
    jachLog.LogAlert('Service stoping');
    tmrServiceRunning.Enabled := False;
    Stopped := True;
    jachLog.LogAlert('Service stopped succesfully');
  except
    on E: Exception do
    begin
      Stopped := False;
      jachLog.LogCritical('Error stopping the service', E);
    end;
  end;
end;

procedure TsvcDemo.SetServiceDescription;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create(KEY_READ or KEY_WRITE);
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKey('\SYSTEM\CurrentControlSet\Services\' + Name, false) then
        Reg.WriteString('Description', 'Just log messages to file to demonstrate jachLog library service integration');
    finally
      Reg.Free;
    end;
  except
    on E:Exception do
      jachLog.LogError('Could not set service description value, error:', E);
  end;
end;

procedure TsvcDemo.tmrServiceRunningTimer(Sender: TObject);
begin
  jachLog.LogInfo('The jachLog demo service is still running');
end;

end.
