unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.Generics.Collections, Vcl.ExtCtrls;

type
  TMsg = class
  private
    FTimeStamp: TDateTime;
    FMsg: string;
    FTID: TThreadID;
  public
    constructor Create(S: string);
  end;

  TForm3 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Timer1: TTimer;
    lblThreadCount: TLabel;
    lblPath: TLabel;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FMsgs: TThreadedQueue<TMsg>;
    FActiveThreads: Integer;
    FMoreMsgs: Boolean;
    procedure ThreadTerminate(Sender: TObject);
  public
    procedure AddMsg(S: string); overload;
    procedure AddMsg(S: string; Args: array of const); overload;
  end;


  TMyThread = class(TThread)
  private
    FForm: TForm3;
  protected
    constructor Create(AForm: TForm3; ATerminateMethod: TNotifyEvent);
    procedure Execute; override;
  end;

var
  Form3: TForm3;

implementation

uses
  ujachLogMgr, ujachLogAuto, System.Types;

{$R *.dfm}

{ TMyThread }

constructor TMyThread.Create(AForm: TForm3; ATerminateMethod: TNotifyEvent);
begin
  FForm := AForm;
  OnTerminate := ATerminateMethod;
  inherited Create(False);
end;

procedure TMyThread.Execute;
var
  I: Integer;
const
  MaxEntries = 10000;
begin
  inherited;
  System.AtomicIncrement(FForm.FActiveThreads);
  try
    FForm.AddMsg('Starting thread');
    jachLog.LogInfo('Starting thread');
    for I := 1 to MaxEntries do
      jachLog.LogInfo('Entry %d', [I]);
    jachLog.LogInfo('Ending thread');
    FForm.AddMsg('Ending thread');
    FForm.FMoreMsgs := True;
  finally
    System.AtomicDecrement(FForm.FActiveThreads);
  end;
end;

{ TForm3 }

procedure TForm3.AddMsg(S: string);
begin
  FMsgs.PushItem(TMsg.Create(S));
end;

procedure TForm3.AddMsg(S: string; Args: array of const);
begin
  AddMsg(Format(S, Args));
end;

procedure TForm3.Button1Click(Sender: TObject);
begin
  TMyThread.Create(Self, ThreadTerminate);
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
  TMyThread.Create(Self, ThreadTerminate);
  TMyThread.Create(Self, ThreadTerminate);
  TMyThread.Create(Self, ThreadTerminate);
  TMyThread.Create(Self, ThreadTerminate);
  TMyThread.Create(Self, ThreadTerminate);
  TMyThread.Create(Self, ThreadTerminate);
  TMyThread.Create(Self, ThreadTerminate);
  TMyThread.Create(Self, ThreadTerminate);
  TMyThread.Create(Self, ThreadTerminate);
  TMyThread.Create(Self, ThreadTerminate);
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  jachLog.IsCached := True;
  FMsgs := TThreadedQueue<TMsg>.Create(512, 100, 10);
  lblPath.Caption := 'You can find the log in folder: ' + GetLogDiskWriter.BasePath;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  FMsgs.Free;
end;

procedure TForm3.ThreadTerminate(Sender: TObject);
begin
  if (FActiveThreads = 0) and (FMoreMsgs) then
  begin
    FMoreMsgs := False;
    AddMsg('Writing cached log begin');
    jachLog.WriteCachedLog;
    AddMsg('Writing cached log end');
  end;
end;

procedure TForm3.Timer1Timer(Sender: TObject);
var
  Msg: TMsg;
begin
  lblThreadCount.Caption := Format('%d threads', [FActiveThreads]);
  while FMsgs.PopItem(Msg) = TWaitResult.wrSignaled do
  begin
    Memo1.Lines.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Msg.FTimeStamp) + Format(' %.4x %s', [Msg.FTID, Msg.FMsg]));
    Msg.Free;
  end;
end;

{ TMsg }

constructor TMsg.Create(S: string);
begin
  FMsg := S;
  FTimeStamp := Now;
  FTID := GetCurrentThreadId;
end;

end.
