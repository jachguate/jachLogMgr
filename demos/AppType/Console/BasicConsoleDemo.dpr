program BasicConsoleDemo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Winapi.ShellAPI,
  Winapi.Windows,
  ujachLogAuto in '..\..\..\src\ujachLogAuto.pas',
  UjachLogMgr in '..\..\..\src\UjachLogMgr.pas';

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
    jachLog.LogWarning('  this is a Warning example!');
    jachLog.LogInfo('Connecting to database...');
    Sleep(312);
    jachLog.LogAlert('  this is an Alert example!');
    jachLog.LogInfo('Updating information...');
    Sleep(68);
    jachLog.LogInfo('Consuming webservice to report completion of the process...');
    try
      raise EProgrammerNotFound.Create('This is a sample exception to be logged');
    except
      on E:Exception do
      begin
        jachLog.LogError('An example error was raised during processing:', E);
      end;
    end;
    Sleep(142);
    jachLog.LogInfo('Process finished succesfully');
    jachLog.DecIndent;
    jachLog.LogInfo('Program ended');
    jachLog.LogInfo('Look at the %s folder for the log file output of this execution.', [GetLogDiskWriter.BasePath]);
    Writeln('Press any key to exit.');
    Readln;
    Winapi.ShellAPI.ShellExecute(0, 'open', PChar(GetLogDiskWriter.BasePath), nil, nil, SW_SHOWNORMAL)
  except
    on E: Exception do
      Writeln(E.Message);
  end;
end.
