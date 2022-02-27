unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm3 = class(TForm)
    cboxLevel: TComboBox;
    Label1: TLabel;
    Button1: TButton;
    lblPath: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cboxLevelChange(Sender: TObject);
  private
  public
  end;

var
  Form3: TForm3;

implementation

uses
  ujachLogAuto, ujachLogClasses;

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
begin
  jachLog.LogInfo('********************** First message written ********************');
  jachLog.LogDebug('This is a debug message, it is only written when the log level is debug. This is the greatest of all severities');
  jachLog.LogInfo('This is a informational message, it is only written when the log level is debug or info'#13
                + 'The level filter works on a equal or less than basis. '
                + 'In other words, all the messages of a severity equal or less than the current log level are included in the log output.'#13
                + 'On the other hand, all messages of a severity greater than the current log level are immediately discarded.');
  jachLog.LogNotice('This is a notice message, it is only written when the log level is debug, info or notice');
  jachLog.LogWarning('This is a warning, it is only written when the log level is debug, info, notice or warning');
  jachLog.LogError('This is an error, it is only written when the log level is debug, info, notice, warning or error');
  jachLog.LogCritical('This is a message of a critical condition, it is only written when the log level is debug, info, notice, warning, error or critical.'#13
                    + 'In a production environment, there''s no reason to put the log level on a value less than critical, since all the following messages should be known by operators and/or support.');
  jachLog.LogAlert('This is an alert, it is only written when the log level is debug, info, notice, warning, error, critical or alert.'#13
                 + 'In a production environment, this message usually generates a form of alert even for remote operators, like emails, SMS, etc.');
  jachLog.LogEmergency('This is an emergency message, it is written on all levls except Off.'#13
                     + 'In a production environment, this message also usually generates a form of alert even for remote operators, like emails, SMS, etc.');
  jachLog.LogInfo('********************** Last message written ********************');
end;

procedure TForm3.cboxLevelChange(Sender: TObject);
begin
  jachLog.LogLevel[jachLog.DefaultTopic] := TLogLevel(cboxLevel.ItemIndex);
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  GetLogDiskWriter.MaxLineSize := 80;
  lblPath.Caption := 'Log folder: ' + GetLogDiskWriter.BasePath;
  jachLog.LogLevel[jachLog.DefaultTopic] := TLogLevel(cboxLevel.ItemIndex);
end;

end.
