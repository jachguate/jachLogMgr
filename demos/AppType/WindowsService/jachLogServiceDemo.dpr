program jachLogServiceDemo;

uses
  Vcl.SvcMgr,
  usvcDemo in 'usvcDemo.pas' {svcDemo: TService};

{$R *.RES}

begin
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TsvcDemo, svcDemo);
  Application.Run;
end.
