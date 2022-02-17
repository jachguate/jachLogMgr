program LogGUIDemo;

uses
  Vcl.Forms,
  ufrmMain in 'ufrmMain.pas' {Form3},
  UjachLogMgr in '..\..\src\UjachLogMgr.pas',
  ujachLogToDisk in '..\..\src\ujachLogToDisk.pas',
  ujachLogClasses in '..\..\src\ujachLogClasses.pas',
  ujachLogAuto in '..\..\src\ujachLogAuto.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
