unit ujachLogAuto;

interface

uses
  ujachLogClasses, UjachLogMgr, ujachLogToDisk;

var
  jachLog: TjachLog;

  function GetLogDiskWriter: TjachLogToDisk;

implementation

var
  lLogToDisk: TjachLogToDisk;

function GetLogDiskWriter: TjachLogToDisk;
begin
  Result := lLogToDisk;
end;

initialization
  jachLog := TjachLog.Create();
  lLogToDisk := TjachLogToDisk.Create;
  jachLog.RegisterLogWriter(lLogToDisk);
finalization
  jachLog.Free;
end.
