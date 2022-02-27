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
    cboxTopic: TComboBox;
    Label2: TLabel;
    chbxIncludeTopicName: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cboxLevelChange(Sender: TObject);
    procedure cboxTopicClick(Sender: TObject);
    procedure chbxIncludeTopicNameClick(Sender: TObject);
  private
  public
  end;

var
  Form3: TForm3;

implementation

uses
  ujachLogAuto, ujachLogClasses;

{$R *.dfm}

const
  LTOPIC_GENERAL         = 0;
  LTOPIC_TRANSACTION     = 1;
  LTOPIC_NETWORK         = 2;
  LTOPIC_SECURITY        = 3;
  LTOPIC_AUTHORIZATION   = 4;
  LTOPIC_ENVIRONMENT     = 5;
  MAX_TOPIC              = 5;

const
  TOPIC_NAMES: array[0..MAX_TOPIC] of string = ('General', 'Transaction', 'Network', 'Security', 'Authorization', 'Environment');

procedure TForm3.Button1Click(Sender: TObject);

  procedure WriteLog(ATopic: TjachLogTopicIndex);
  begin
    jachLog.LogInfo(ATopic, '********************** [%s] First message written ********************', [TOPIC_NAMES[ATopic]]);
    jachLog.LogDebug(ATopic, 'This is a debug message on %s topic, it is only written when the log level is debug. This is the greatest of all severities', [TOPIC_NAMES[ATopic]]);
    jachLog.LogInfo(ATopic, 'This is a informational message on %s topic, it is only written when the log level is debug or info'#13
                          + 'The level filter works on a equal or less than basis. '
                          + 'In other words, all the messages of a severity equal or less than the current log level are included in the log output.'#13
                          + 'On the other hand, all messages of a severity greater than the current log level are immediately discarded.', [TOPIC_NAMES[ATopic]]);
    jachLog.LogNotice(ATopic, 'This is a notice message on %s topic, it is only written when the log level is debug, info or notice', [TOPIC_NAMES[ATopic]]);
    jachLog.LogWarning(ATopic, 'This is a warning on %s topic, it is only written when the log level is debug, info, notice or warning', [TOPIC_NAMES[ATopic]]);
    jachLog.LogError(ATopic, 'This is an error on %s topic, it is only written when the log level is debug, info, notice, warning or error', [TOPIC_NAMES[ATopic]]);
    jachLog.LogCritical(ATopic, 'This is a message of a critical condition on %s topic, it is only written when the log level is debug, info, notice, warning, error or critical.'#13
                              + 'In a production environment, there''s no reason to put the log level on a value less than critical, since all the following messages should be known by operators and/or support.', [TOPIC_NAMES[ATopic]]);
    jachLog.LogAlert(ATopic, 'This is an alert on %s topic, it is only written when the log level is debug, info, notice, warning, error, critical or alert.'#13
                           + 'In a production environment, this message usually generates a form of alert even for remote operators, like emails, SMS, etc.', [TOPIC_NAMES[ATopic]]);
    jachLog.LogEmergency(ATopic, 'This is an emergency message on %s topic, it is written on all levls except Off.'#13
                               + 'In a production environment, this message also usually generates a form of alert even for remote operators, like emails, SMS, etc.', [TOPIC_NAMES[ATopic]]);
    jachLog.LogInfo(ATopic, '********************** [%s] Last message written ********************', [TOPIC_NAMES[ATopic]]);
  end;
var
  I: Integer;

begin
  for I := 0 to MAX_TOPIC do
    WriteLog(I);
end;

procedure TForm3.cboxLevelChange(Sender: TObject);
begin
  jachLog.LogLevel[cboxTopic.ItemIndex] := TLogLevel(cboxLevel.ItemIndex);
end;

procedure TForm3.cboxTopicClick(Sender: TObject);
begin
  cboxLevel.ItemIndex := Integer(jachLog.LogLevel[cboxTopic.ItemIndex]);
end;

procedure TForm3.chbxIncludeTopicNameClick(Sender: TObject);
begin
  jachLog.IncludeTopicName := chbxIncludeTopicName.Checked;
end;

procedure TForm3.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to MAX_TOPIC do
  begin
    cboxTopic.Items.Add(TOPIC_NAMES[I]);
    jachLog.TopicName[I] := TOPIC_NAMES[I];
  end;
  cboxTopic.ItemIndex := 0;
  GetLogDiskWriter.MaxLineSize := 80;
  lblPath.Caption := 'Log folder: ' + GetLogDiskWriter.BasePath;
  jachLog.LogLevel[cboxTopic.ItemIndex] := TLogLevel(cboxLevel.ItemIndex);
end;

end.
