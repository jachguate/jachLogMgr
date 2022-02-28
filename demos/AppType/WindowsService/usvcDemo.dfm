object svcDemo: TsvcDemo
  OldCreateOrder = False
  DisplayName = 'jachLogServiceDemo'
  BeforeInstall = ServiceBeforeInstall
  AfterInstall = ServiceAfterInstall
  BeforeUninstall = ServiceBeforeUninstall
  AfterUninstall = ServiceAfterUninstall
  OnContinue = ServiceContinue
  OnExecute = ServiceExecute
  OnPause = ServicePause
  OnShutdown = ServiceShutdown
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
  object tmrServiceRunning: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = tmrServiceRunningTimer
    Left = 88
    Top = 56
  end
end
