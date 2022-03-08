program FMXDesktopGUIDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  ufrmMain in 'ufrmMain.pas' {Form3},
  ujachLogToFMXMemo in '..\..\..\src\ujachLogToFMXMemo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
