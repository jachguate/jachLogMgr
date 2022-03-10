program SysLogIndyUDPDemo;

{$R *.dres}

uses
  Vcl.Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  ujachLogToSysLogIndyUDP in '..\..\..\src\ujachLogToSysLogIndyUDP.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
