program BasicConsoleDemo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  ujachLogAuto in '..\..\src\ujachLogAuto.pas',
  UjachLogMgr in '..\..\src\UjachLogMgr.pas';

begin
  try
    jachLog.LogInfo('Program started');
    jachLog.IncIndent;
    jachLog.LogInfo('This is an example of how to use the log class in a console application. In this case, thanks to the ujachLogAuto unit, '
     + 'the same log will be printed to console and writed to a disk file that will preserve the output'#13
     + 'To make the example look like a real process, we will add some entries to the log that simulates a real processing'
      );
    jachLog.LogInfo('Scanning input files...');
    Sleep(500);
    jachLog.LogInfo('Consolidating information...');
    Sleep(987);
    jachLog.LogInfo('Connecting to database...');
    Sleep(312);
    jachLog.LogInfo('Updating information...');
    Sleep(68);
    jachLog.LogInfo('Consuming webservice to report completion of the process...');
    Sleep(142);
    jachLog.LogInfo('Process finished succesfully');
    jachLog.DecIndent;
    jachLog.LogInfo('Program ended');
    Writeln('Press any key to exit.');
    Readln;
  except
    on E: Exception do
      jachLog.LogError(E);
  end;
end.
