program jachLogMegaDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  System.SysUtils,
  System.Classes,
  ujachLogMgr,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  ufraWriterConfig in 'ufraWriterConfig.pas' {fraWriterConfig: TFrame},
  uDemoConstants in 'uDemoConstants.pas',
  ufraWriterToDiskConfig in 'ufraWriterToDiskConfig.pas' {fraWriterToDiskConfig: TFrame},
  ufraWriterToFMXMemoConfig in 'ufraWriterToFMXMemoConfig.pas' {fraWriterToFMXMemoConfig: TFrame},
  ufraWriterToSysLogIndyUDPConfig in 'ufraWriterToSysLogIndyUDPConfig.pas' {fraWriterToSysLogIndyUDPConfig: TFrame};

{$R *.res}

  procedure InitLog();
  begin
    {$ifdef debug}TThread.NameThreadForDebugging('Main thread');{$endif}
    jachLog := TjachLog.Create();
    jachLog.TopicName[TOPIC_GENERAL] := 'General';
    jachLog.TopicName[TOPIC_SECURITY] := 'Security';
    jachLog.TopicName[TOPIC_NETWORK] := 'Network';
    jachLog.IncludeTopicName := True;
    jachLog.IsActive := True;
  end;

begin
  InitLog();
  try
    Application.Initialize;
    Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
  except
    on E:Exception do
      jachLog.LogCritical('MegaDemo terminated abnormally', E);
  end;
end.
