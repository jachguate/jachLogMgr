unit ufrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TfrmMain = class(TForm)
    Button1: TButton;
    btnOpenLogFolder: TButton;
    richDemoText: TRichEdit;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure btnOpenLogFolderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
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
  jachLog.DebugVerbosityThreshold := 3;
  jachLog.LogLevel[jachLog.DefaultTopic] := llAll;
  Screen.Cursor := crHourGlass;
  try
    try
      jachLog.IncIndent;
      jachLog.LogInfo('This is an example of how to use the log class in a VCL application. This application uses the ujachLogAuto '
       + 'unit to autocreate the log object and disk destination.'#13
       + 'To make the example look like a real process, we will add some entries to the log that simulates a real processing'
        );
      jachLog.LogInfo('Scanning input files...');
      jachLog.LogDebug(0, 'The input filename is c:\input.txt');
      jachLog.LogDebug(1, 'File read starts');
      Sleep(100);
      jachLog.LogDebug(3, '1000 records readed so far');
      Sleep(100);
      jachLog.LogDebug(3, '2000 records readed so far');
      Sleep(100);
      jachLog.LogDebug(3, '3000 records readed so far');
      Sleep(100);
      jachLog.LogDebug(3, '4000 records readed so far');
      Sleep(100);
      jachLog.LogDebug(3, '5000 records readed so far');
      Sleep(100);
      jachLog.LogDebug(3, '6000 records readed so far');
      Sleep(48);
      jachLog.LogDebug(3, '6872 records readed');
      jachLog.LogDebug(1, 'File read end');
      jachLog.LogInfo('Consolidating information...');
      jachLog.LogDebug(1, 'Data processing starts');
      Sleep(88);
      jachLog.LogDebug(3, '348 summary records created');
      Sleep(88);
      jachLog.LogDebug(3, '1342 input records processed');
      Sleep(88);
      jachLog.LogDebug(3, '2654 input records processed');
      Sleep(88);
      jachLog.LogDebug(3, '4058 input records processed');
      Sleep(14);
      jachLog.LogWarning('The record #4201 is malformed in the input file, ignoring it!');
      Sleep(88);
      jachLog.LogDebug(3, '5362 input records processed');
      Sleep(88);
      jachLog.LogDebug(3, '6614 input records processed');
      Sleep(12);
      jachLog.LogDebug(3, '6872 input records processed');
      jachLog.LogDebug(1, 'Data processing ends');
      jachLog.LogDebug(0, 'The output filename is c:\consolidated.txt');
      Sleep(58);
      jachLog.LogDebug(3, '182 summary records written so far');
      Sleep(58);
      jachLog.LogDebug(3, '348 summary records written so far');
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
  jachLog.LogInfo('Program ended');
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  strDemoText: TResourceStream;
begin
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
