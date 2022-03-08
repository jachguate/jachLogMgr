program VCLDesktopGUIDemo;

{$R *.dres}

uses
  Vcl.Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  ujachLogToVCLRichEdit in '..\..\..\src\ujachLogToVCLRichEdit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
