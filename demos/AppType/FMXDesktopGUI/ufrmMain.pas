unit ufrmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, ujachLogToFMXMemo;

type
  TForm3 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    btnOpenLogFolder: TButton;
    Splitter1: TSplitter;
    memoLog: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure btnOpenLogFolderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FLoggerToFMXMemo: TjachLogToFMXMemo;
  public
  end;

var
  Form3: TForm3;

implementation

uses
  ujachLogMgr, ujachLogAuto, Winapi.ShellAPI, Winapi.Windows;

{$R *.fmx}

procedure TForm3.btnOpenLogFolderClick(Sender: TObject);
begin
  ForceDirectories(GetLogDiskWriter.BasePath);
  Winapi.ShellAPI.ShellExecute(0, 'open', PChar(GetLogDiskWriter.BasePath), nil, nil, SW_SHOWNORMAL)
end;

procedure TForm3.Button1Click(Sender: TObject);
begin
//  Screen.Cursor := crHourGlass;
  try
    try
      jachLog.IncIndent;
      jachLog.LogInfo('This is an example of how to use the log class in a VCL application. This application uses the ujachLogAuto '
       + 'unit to autocreate the log object and disk destination.'#13
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
      jachLog.LogInfo('Look at the %s folder for the log file output of this execution.', [GetLogDiskWriter.BasePath]);
    except
      on E: Exception do
        jachLog.LogCritical('Processing failure with error:', E);
    end;
  finally
//    Screen.Cursor := crDefault;
  end;
end;

procedure TForm3.FormCreate(Sender: TObject);
//var
//  strDemoText: TResourceStream;
begin
  FLoggerToFMXMemo := TjachLogToFMXMemo.Create;
  FLoggerToFMXMemo.Memo := memoLog;
  FLoggerToFMXMemo.IsActive := True;
  jachLog.RegisterLogWriter(FLoggerToFMXMemo);
  jachLog.LogInfo('Program started');
  btnOpenLogFolder.Text := 'Open log folder: ' + GetLogDiskWriter.BasePath;
//  strDemoText := TResourceStream.Create(hInstance, 'DemoText', RT_RCDATA);
//  try
//    richDemoText.Lines.LoadFromStream(strDemoText);
//  finally
//    strDemoText.Free;
//  end;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  jachLog.UnRegisterLogWriter(FLoggerToFMXMemo);
  FreeAndNil(FLoggerToFMXMemo);
  jachLog.LogInfo('Program ended');
end;

end.
