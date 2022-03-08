unit ufrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  ujachLogToVCLRichEdit;

type
  TfrmMain = class(TForm)
    Button1: TButton;
    btnOpenLogFolder: TButton;
    richDemoText: TRichEdit;
    Panel1: TPanel;
    Splitter1: TSplitter;
    richLog: TRichEdit;
    procedure Button1Click(Sender: TObject);
    procedure btnOpenLogFolderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FLoggerToRichEdit: TjachLogToVCLRichEdit;
  public
  end;

var
  frmMain: TfrmMain;

implementation

uses
  ujachLogAuto, ujachLogMgr, Winapi.ShellAPI;

{$R *.dfm}

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
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
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.btnOpenLogFolderClick(Sender: TObject);
begin
  ForceDirectories(GetLogDiskWriter.BasePath);
  Winapi.ShellAPI.ShellExecute(0, 'open', PChar(GetLogDiskWriter.BasePath), nil, nil, SW_SHOWNORMAL)
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  jachLog.UnRegisterLogWriter(FLoggerToRichEdit);
  FreeAndNil(FLoggerToRichEdit);
  jachLog.LogInfo('Program ended');
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  strDemoText: TResourceStream;
begin
  FLoggerToRichEdit := TjachLogToVCLRichEdit.Create;
  FLoggerToRichEdit.RichEdit := richLog;
  FLoggerToRichEdit.IsActive := True;
  jachLog.RegisterLogWriter(FLoggerToRichEdit);
  jachLog.LogInfo('Program started');
  btnOpenLogFolder.Caption := 'Open log folder: ' + GetLogDiskWriter.BasePath;
  strDemoText := TResourceStream.Create(hInstance, 'DemoText', RT_RCDATA);
  try
    richDemoText.Lines.LoadFromStream(strDemoText);
  finally
    strDemoText.Free;
  end;
end;

end.
