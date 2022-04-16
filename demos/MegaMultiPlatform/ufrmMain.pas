unit ufrmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.MultiView, FMX.TabControl,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, ujachLogMgr, ufraWriterConfig, System.Generics.Collections,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Data.Bind.Components, Data.Bind.DBScope,
  FireDAC.Stan.StorageBin, Data.Bind.GenData, Data.Bind.ObjectScope,
  FMX.ScrollBox, FMX.Memo, FMX.ListBox;

type
  TRegisteredWriter = class
  private
    FFrame: TfraWriterConfig;
    FFrameClass: TfraWriterConfigClass;
    FWriter: TjachLogWriter;
    FWriterClass: TjachLogWriterClass;
    FName: string;
    FConfigRoutine: TProc<TjachLogWriter>;
    FWriterIsCreatedActive: Boolean;
    procedure SetFrame(const Value: TfraWriterConfig);
    procedure SetFrameClass(const Value: TfraWriterConfigClass);
    procedure SetWriter(const Value: TjachLogWriter);
    procedure SetWriterClass(const Value: TjachLogWriterClass);
    procedure SetName(const Value: string);
    procedure SetConfigRoutine(const Value: TProc<TjachLogWriter>);
    procedure SetWriterIsCreatedActive(const Value: Boolean);
  public
    property WriterClass: TjachLogWriterClass read FWriterClass write SetWriterClass;
    property FrameClass: TfraWriterConfigClass read FFrameClass write SetFrameClass;
    property Name: string read FName write SetName;
    property ConfigRoutine: TProc<TjachLogWriter> read FConfigRoutine write SetConfigRoutine;
    property Writer: TjachLogWriter read FWriter write SetWriter;
    property Frame: TfraWriterConfig read FFrame write SetFrame;
    property WriterIsCreatedActive: Boolean read FWriterIsCreatedActive write SetWriterIsCreatedActive;
  end;

  TRegisteredWriterList = class(TObjectList<TRegisteredWriter>)
  end;

  TRegisteredWriterQueue = class(TQueue<TRegisteredWriter>)
  end;

  TfrmMain = class(TForm)
    MultiView1: TMultiView;
    tabcGeneral: TTabControl;
    tiLogConfig: TTabItem;
    tiWriters: TTabItem;
    swIsActive: TSwitch;
    pnlContent: TPanel;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    lblWriteInAuxThreads: TLabel;
    swIsCached: TSwitch;
    swUseSeparateThreadToWrite: TSwitch;
    toolNavPane: TToolBar;
    lblTitle: TLabel;
    lvwAvailableWriters: TListView;
    pnlWriters: TVertScrollBox;
    tmrRemoveWriter: TTimer;
    tiLogGenerator: TTabItem;
    memGenerators: TFDMemTable;
    memGeneratorsID: TIntegerField;
    memGeneratorsName: TStringField;
    memGeneratorsHelp: TStringField;
    memGeneratorsCategory: TStringField;
    lvwGenerators: TListView;
    bsrcGenerators: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkListControlToField1: TLinkListControlToField;
    absrcAvailableWriters: TAdapterBindSource;
    datAdapterAvailableWriters: TDataGeneratorAdapter;
    LinkListControlToField2: TLinkListControlToField;
    lblTapTheWriter: TLabel;
    tmrAdjustAllFramesHeight: TTimer;
    tiLog: TTabItem;
    memoLog: TMemo;
    btnClearLog: TSpeedButton;
    procedure swIsActiveSwitch(Sender: TObject);
    procedure swIsCachedSwitch(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure swUseSeparateThreadToWriteSwitch(Sender: TObject);
    procedure tmrRemoveWriterTimer(Sender: TObject);
    procedure lvwGeneratorsItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure absrcAvailableWritersCreateAdapter(Sender: TObject;
      var ABindSourceAdapter: TBindSourceAdapter);
    procedure lvwAvailableWritersItemClickEx(const Sender: TObject;
      ItemIndex: Integer; const LocalClickPos: TPointF;
      const ItemObject: TListItemDrawable);
    procedure FormResize(Sender: TObject);
    procedure tmrAdjustAllFramesHeightTimer(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FAvailableWriters: TRegisteredWriterList;
    FCreatedWriters: TRegisteredWriterList;
    FToRemoveWriters: TRegisteredWriterQueue;
    FFrameNameCount: Integer;
    FTopicControlCurrentY: Single;
    FMaxTopicIndex: TjachLogTopicIndex;
    FActiveContinuousThreads: TList<TThread>;
    procedure RegisterWriterClass(AWriterClass: TjachLogWriterClass;
      AFrameClass: TfraWriterConfigClass; AName: string;
      AWriterIsCreatedActive: Boolean;
      AConfigRoutine: TProc<TjachLogWriter> = nil);
    procedure RegisterAllWriters;
    procedure FillGeneratorsTable;
    procedure CreateWriterOfClass(AWriterClass: TjachLogWriterClass);
    procedure CreateWriter(ARegisteredWriter: TRegisteredWriter);
    procedure RemoveWriter(ARegisteredWriter: TRegisteredWriter);
    procedure WriterFrameUserClose(Sender: TObject);
    function FindWriterByFrame(AFrame: TfraWriterConfig): TRegisteredWriter;
    procedure AdjustAllFramesHeight;
    procedure CreateFilterControlsForTopic(ATopicIndex: TjachLogTopicIndex);
    procedure CreateFilterControlsForTopics;
    procedure cboxLevelChange(Sender: TObject);
    procedure TerminateAllContinuousThreads;
  private
    //Log generators
    procedure GenerateBasicSingle;
    procedure GenerateBasicFullSet;
    procedure GeneratePerformance10000;
    procedure GenerateSimulation;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  ujachLogToDisk, ujachLogToFMXMemo, ujachLogToSysLogIndyUDP, uDemoConstants,
  ufraWriterToDiskConfig, FMX.Platform, ufraWriterToFMXMemoConfig,
  ufraWriterToSysLogIndyUDPConfig, System.Threading;

{$R *.fmx}

type
  TContinuousThread = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;

procedure TfrmMain.absrcAvailableWritersCreateAdapter(Sender: TObject;
  var ABindSourceAdapter: TBindSourceAdapter);
begin
  FAvailableWriters := TRegisteredWriterList.Create(False);
  FCreatedWriters := TRegisteredWriterList.Create(False);
  FToRemoveWriters := TRegisteredWriterQueue.Create;
  RegisterAllWriters;
  ABindSourceAdapter := TListBindSourceAdapter<TRegisteredWriter>.Create(Self, FAvailableWriters, False);
end;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  FillGeneratorsTable;
  CreateFilterControlsForTopics;
  FActiveContinuousThreads := TList<TThread>.Create;
end;

procedure TfrmMain.CreateFilterControlsForTopic(
  ATopicIndex: TjachLogTopicIndex);
var
  lLabel: TLabel;
  lCombo: TComboBox;
begin
  lLabel := TLabel.Create(Self);
  lLabel.Parent := tiLogConfig;
  lLabel.Text := jachLog.TopicName[ATopicIndex] + ' filter';
  lLabel.Position.X := lblWriteInAuxThreads.Position.X;
  lLabel.Width := lblWriteInAuxThreads.Width;
  lLabel.Position.Y := FTopicControlCurrentY;

  lCombo := TComboBox.Create(Self);
  lCombo.Parent := tiLogConfig;
  lCombo.Position.X := swUseSeparateThreadToWrite.Position.X;
  lCombo.Position.Y := lLabel.Position.Y;
  lCombo.Width := swUseSeparateThreadToWrite.Width;
  lCombo.Items.Text := 'Off'#13'Emergency'#13'Alert'#13'Critical'#13'Error'#13'Warning'#13'Notice'#13'Info'#13'Debug'#13'All';
  lCombo.ItemIndex := Integer(jachLog.LogLevel[ATopicIndex]);
  lCombo.Tag := ATopicIndex;
  lCombo.OnChange := cboxLevelChange;

  lLabel.Height := lCombo.Height;

  FTopicControlCurrentY := FTopicControlCurrentY + lCombo.Height + 2;

  if ATopicIndex > FMaxTopicIndex then
    FMaxTopicIndex := ATopicIndex;
end;

procedure TfrmMain.CreateFilterControlsForTopics;
var
  I: Integer;
begin
  FTopicControlCurrentY := lblWriteInAuxThreads.Position.Y + lblWriteInAuxThreads.Height + 2;
  for I := Low(TjachLogTopicIndex) to High(TjachLogTopicIndex) do
    if jachLog.TopicName[I] <> '' then
      CreateFilterControlsForTopic(I)
    else
      break;
end;

procedure TfrmMain.CreateWriter(ARegisteredWriter: TRegisteredWriter);
var
  IndexOfWriter: Integer;
begin
  IndexOfWriter := FAvailableWriters.IndexOf(ARegisteredWriter);
  if IndexOfWriter = -1 then
    raise Exception.Create('Writer is not registered!');

  ARegisteredWriter.Writer := ARegisteredWriter.WriterClass.Create();
  if ARegisteredWriter.WriterIsCreatedActive then
    ARegisteredWriter.Writer.IsActive := True;

  if Assigned(ARegisteredWriter.ConfigRoutine) then
    ARegisteredWriter.ConfigRoutine(ARegisteredWriter.Writer);
  ARegisteredWriter.Frame := ARegisteredWriter.FrameClass.Create(Self, ARegisteredWriter.Writer);
  Inc(FFrameNameCount);
  ARegisteredWriter.Frame.Name := ARegisteredWriter.FrameClass.ClassName + '_' + IntToStr(FFrameNameCount);
  ARegisteredWriter.Frame.lblWriterName.Text := Format('%s (%s.%s)'
    , [  ARegisteredWriter.Name
       , ARegisteredWriter.WriterClass.UnitName
       , ARegisteredWriter.WriterClass.ClassName
      ]);
  ARegisteredWriter.Frame.Parent := pnlWriters;
  ARegisteredWriter.Frame.Position.Y := 10000;
  ARegisteredWriter.Frame.Align := TAlignLayout.Top;
  ARegisteredWriter.Frame.OnUserClose := WriterFrameUserClose;

  tmrAdjustAllFramesHeight.Enabled := True;

  jachLog.RegisterLogWriter(ARegisteredWriter.Writer);

  absrcAvailableWriters.Active := False;
  try
    FAvailableWriters.Delete(IndexOfWriter);
    FCreatedWriters.Add(ARegisteredWriter);
  finally
    absrcAvailableWriters.Active := True;
  end;
end;

procedure TfrmMain.CreateWriterOfClass(AWriterClass: TjachLogWriterClass);
var
  I: Integer;
begin
  for I := 0 to FAvailableWriters.Count - 1 do
    if FAvailableWriters[I].WriterClass = AWriterClass then
    begin
      CreateWriter(FAvailableWriters[I]);
      Break;
    end;
end;

destructor TfrmMain.Destroy;
begin
  while FAvailableWriters.Count <> 0 do
  begin
    FAvailableWriters[0].Free;
    FAvailableWriters.Remove(FAvailableWriters[0]);
  end;
  while FCreatedWriters.Count <> 0 do
  begin
    FCreatedWriters[0].Free;
    FCreatedWriters.Remove(FCreatedWriters[0]);
  end;
  FAvailableWriters.Free;
  FCreatedWriters.Free;
  FToRemoveWriters.Free;
  FActiveContinuousThreads.Free;
  inherited;
end;

procedure TfrmMain.AdjustAllFramesHeight;
var
  I: Integer;
begin
  for I := 0 to FCreatedWriters.Count - 1 do
    FCreatedWriters[I].Frame.AdjustHeight;
end;

procedure TfrmMain.btnClearLogClick(Sender: TObject);
begin
  memoLog.Lines.Clear;
end;

procedure TfrmMain.cboxLevelChange(Sender: TObject);
var
  lCombo: TComboBox;
  IsError: Boolean;
  RegWriter: TRegisteredWriter;
begin
  lCombo := Sender as TComboBox;
  jachLog.LogLevel[lCombo.Tag] := TLogLevel(lCombo.ItemIndex);
  lCombo.ItemIndex := Integer(jachLog.LogLevel[lCombo.Tag]);
  //
  IsError := False;
  if jachLog.LogLevel[lCombo.Tag] <> TLogLevel.llAll then
  begin
    for RegWriter in FCreatedWriters do
    begin
      if RegWriter.Writer.LogLevel[lCombo.Tag] <> TLogLevel.llAll then
      begin
        IsError := True;
        break;
      end;
    end;
  end;

  if IsError then
    ShowMessage('Warning!'#13
      + 'You''re mixing filters at two levels. At log level, which are applied globally and at the writer level, which affects what is delivered to that destination.'#13
      + 'It is recommended you apply filters only at one level, unless you really know what you''re doing, since it may be confussing determining if all the desired info is really reaching the destination.'#13
      + 'To apply filter only at one level, set the other level to llAll, so it doesn''t filter anything there.'
    );
end;

const
  GEN_BASIC_SINGLEMAINTHREAD = 001;
  GEN_BASIC_FULLSETMAINTHREAD = 002;
  GEN_BASIC_SINGLESECONDARYTHREAD = 051;
  GEN_BASIC_FULLSETSECONDARYTHREAD = 052;
  GEN_PERF_10000MAINTHREAD = 101;
  GEN_PERF_10000SECONDARYTHREAD = 151;
  GEN_PERF_10000SECONDARYTHREAD_3 = 152;
  GEN_SIMULATION_MAINTHREAD = 201;
  GEN_SIMULATION_SECONDARYTHREAD = 251;
  GEN_CONTINOUS_SECONDARYTHREAD = 351;
  GEN_CONTINOUS_TERMINATE = 352;


procedure TfrmMain.FillGeneratorsTable;
begin
  try
    bsrcGenerators.DataSource.Enabled := False;
    try
      if not memGenerators.Active then
        memGenerators.Open
      else
        while not memGenerators.IsEmpty do
          memGenerators.Delete;
      memGenerators.AppendRecord([GEN_BASIC_SINGLEMAINTHREAD, 'Basic/Main', 'Hello world', 'Writes a single log entry from the main thread.']);
      memGenerators.AppendRecord([GEN_BASIC_FULLSETMAINTHREAD, 'Basic/Main', 'Full set of messages', 'Writes a example of each message level for all categories from the main thread']);
      memGenerators.AppendRecord([GEN_BASIC_SINGLESECONDARYTHREAD, 'Basic/Thread', 'Hello world', 'Writes a single log entry from a spawn secondary thread.']);
      memGenerators.AppendRecord([GEN_BASIC_FULLSETSECONDARYTHREAD, 'Basic/Thread', 'Full set of messages', 'Writes a example of each message level for all categories from a spawn secondary thread']);
      memGenerators.AppendRecord([GEN_PERF_10000MAINTHREAD, 'Performance/Main', '10000 entries', 'Writes 10000 entries from the main thread. Compare the time it takes with the cache inactive vs cache active, and writing in aux threads vs writing in the main thread.']);
      memGenerators.AppendRecord([GEN_PERF_10000SECONDARYTHREAD, 'Performance/Thread', '10000 entries', 'Writes 10000 entries from a spawn secondary thread. Compare performance of writing in aux thrads vs not doing it.']);
      memGenerators.AppendRecord([GEN_PERF_10000SECONDARYTHREAD_3, 'Performance/Thread', '10000 entries * 3', 'Spawns 3 threads that concurrently writes 10000 entries each. Compare performance of writing in aux thrads vs not doing it.']);
      memGenerators.AppendRecord([GEN_SIMULATION_MAINTHREAD, 'Simulation', 'Main thread', 'Simulates a process running in the main thread. The user interface is blocked during the time the simulation runs.']);
      memGenerators.AppendRecord([GEN_SIMULATION_SECONDARYTHREAD, 'Simulation', 'Secondary thread', 'Simulates a process running in a spawn secondary main thread. The user interface is responsive during the time the simulation runs.']);
      memGenerators.AppendRecord([GEN_CONTINOUS_SECONDARYTHREAD, 'Simulation', 'Continous', 'Simulates a worker running in a secondary main thread that is continuously producing log entries. The user interface keesp responsive.']);
      memGenerators.AppendRecord([GEN_CONTINOUS_TERMINATE, 'Simulation', 'Terminate continous', 'Terminates all worker threads producing continuous log entries.']);
    finally
      bsrcGenerators.DataSource.Enabled := True;
    end;
  except
    on E: Exception do
      jachLog.LogError('Filling demo data', E);
  end;
end;

function TfrmMain.FindWriterByFrame(
  AFrame: TfraWriterConfig): TRegisteredWriter;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FCreatedWriters.Count - 1 do
    if FCreatedWriters[I].Frame = AFrame then
    begin
      Result := FCreatedWriters[I];
      break;
    end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TerminateAllContinuousThreads;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  pnlContent.Align := TAlignLayout.Client;
  swIsActive.IsChecked := jachLog.IsActive;
  swIsCached.IsChecked := jachLog.IsCached;
  swUseSeparateThreadToWrite.IsChecked := jachLog.UseSeparateThreadToWrite;
  CreateWriterOfClass(TjachLogToDisk);
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  AdjustAllFramesHeight;
end;

procedure RunWithCursor(ACursor: TCursor; Proc: TProc);
var
  CS: IFMXCursorService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
  begin
    CS := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService;
  end;
  if Assigned(CS) then
  begin
    CS.SetCursor(ACursor);
  end;
  Proc();
end;

procedure TfrmMain.GenerateBasicFullSet;
  procedure GenerateFullSetOfTopic(ATopic: TjachLogTopicIndex);
  begin
    jachLog.LogEmergency(ATopic, 'This is an example emergency message');
    jachLog.LogAlert(ATopic, 'This is an example alert message');
    jachLog.LogCritical(ATopic, 'This is an example critical message');
    jachLog.LogError(ATopic, 'This is an example error message');
    jachLog.LogWarning(ATopic, 'This is an example warning message');
    jachLog.LogNotice(ATopic, 'This is an example notice message');
    jachLog.LogInfo(ATopic, 'This is an example info message');
    jachLog.LogDebug(ATopic, 'This is an example debug message');
  end;
var
  lTopic: TjachLogTopicIndex;
begin
  for lTopic := 0 to FMaxTopicIndex do
    GenerateFullSetOfTopic(lTopic);
  if jachLog.IsCached then
    jachLog.WriteCachedLog;
end;

procedure TfrmMain.GenerateBasicSingle;
begin
  jachLog.LogInfo(TOPIC_GENERAL, 'Hello logs world');
  if jachLog.IsCached then
    jachLog.WriteCachedLog;
end;

procedure TfrmMain.GeneratePerformance10000;
var
  I: Integer;
begin
  for I := 1 to 10000 do
    jachLog.LogInfo(TOPIC_GENERAL, 'Message No. %d', [I]);
  if jachLog.IsCached then
    jachLog.WriteCachedLog;
end;

procedure TfrmMain.GenerateSimulation;
var
  I: Integer;
begin
  jachLog.LogInfo('Simultaion!');
  jachLog.LogInfo('This simulates a process that runs for some 35 seconds and generates a log of its actions, including some warnings.');
  jachLog.LogInfo('Procss starts');
  jachLog.LogInfo('Configuring the process');
  sleep(20);
  jachLog.LogInfo('Reading input file to memory');
  sleep(500);
  jachLog.LogInfo('Processing lines');
  for I := 1 to 300 do
  begin
    sleep(100);
    case random(30) of
      1: jachLog.LogWarning('Line %d is malformed, ignored', [I]);
      2: jachLog.LogWarning('Line %d customer does not exists, ignored', [I]);
      3: jachLog.LogWarning('Line %d product is out of inventory, ignored', [I]);
      else jachLog.LogInfo('Line %d is processed without problems', [I]);
    end;
  end;
  jachLog.LogInfo('Creating database records');
  sleep(5000);
  jachLog.LogInfo('Process terminated succesfully, check the ignored lines and reprocess them.');
  if jachLog.IsCached then
    jachLog.WriteCachedLog;
end;

procedure TfrmMain.lvwAvailableWritersItemClickEx(const Sender: TObject;
  ItemIndex: Integer; const LocalClickPos: TPointF;
  const ItemObject: TListItemDrawable);
begin
  CreateWriter(FAvailableWriters[ItemIndex]);
end;

procedure TfrmMain.lvwGeneratorsItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  tabcGeneral.ActiveTab := tiLog;
  Application.ProcessMessages;
  case memGeneratorsID.Value of
    GEN_BASIC_SINGLEMAINTHREAD:        RunWithCursor(crHourGlass, GenerateBasicSingle);
    GEN_BASIC_FULLSETMAINTHREAD:       RunWithCursor(crHourGlass, GenerateBasicFullSet);
    GEN_BASIC_SINGLESECONDARYTHREAD:   TTask.Create(GenerateBasicSingle).Start;
    GEN_BASIC_FULLSETSECONDARYTHREAD:  TTask.Create(GenerateBasicFullSet).Start;
    GEN_PERF_10000MAINTHREAD:          RunWithCursor(crHourGlass, GeneratePerformance10000);
    GEN_PERF_10000SECONDARYTHREAD:     TTask.Create(GeneratePerformance10000).Start;
    GEN_PERF_10000SECONDARYTHREAD_3:
      begin
                                       TTask.Create(GeneratePerformance10000).Start;
                                       TTask.Create(GeneratePerformance10000).Start;
                                       TTask.Create(GeneratePerformance10000).Start;
      end;
    GEN_SIMULATION_MAINTHREAD:         RunWithCursor(crHourGlass, GenerateSimulation);
    GEN_SIMULATION_SECONDARYTHREAD:    TTask.Create(GenerateSimulation).Start;
    GEN_CONTINOUS_SECONDARYTHREAD:     FActiveContinuousThreads.Add(TContinuousThread.Create);
    GEN_CONTINOUS_TERMINATE:           TerminateAllContinuousThreads;
  end;
end;

procedure TfrmMain.RegisterAllWriters;
begin
  RegisterWriterClass(TjachLogToDisk, TfraWriterToDiskConfig, 'Disk', True);
  RegisterWriterClass(TjachLogToFMXMemo, TfraWriterToFMXMemoConfig, 'UI', True,
    procedure (AWriter: TjachLogWriter)
    var
      Writer: TjachLogToFMXMemo;
    begin
      Writer := TjachLogToFMXMemo(AWriter);
      Writer.Memo := memoLog;
    end
  );
  RegisterWriterClass(TjachLogToSysLogIndyUDP, TfraWriterToSysLogIndyUDPConfig
    , 'SysLog (Indy UDP)', False);
end;

procedure TfrmMain.RegisterWriterClass(AWriterClass: TjachLogWriterClass;
  AFrameClass: TfraWriterConfigClass; AName: string;
  AWriterIsCreatedActive: Boolean; AConfigRoutine: TProc<TjachLogWriter>);
var
  Reg: TRegisteredWriter;
begin
  Reg := TRegisteredWriter.Create;
  try
    Reg.WriterClass := AWriterClass;
    Reg.FrameClass := AFrameClass;
    Reg.Name := AName;
    Reg.ConfigRoutine := AConfigRoutine;
    Reg.WriterIsCreatedActive := AWriterIsCreatedActive;
    FAvailableWriters.Add(Reg);
  except
    on E:Exception do
    begin
      jachLog.LogError('Error registering log writer class %s', [AWriterClass.ClassName], E);
      Reg.Free;
      raise;
    end;
  end;
end;

procedure TfrmMain.RemoveWriter(ARegisteredWriter: TRegisteredWriter);
var
  IndexOfWriter: Integer;
begin
  IndexOfWriter := FCreatedWriters.IndexOf(ARegisteredWriter);
  if IndexOfWriter = -1 then
    raise Exception.Create('Writer is not created!');

  ARegisteredWriter.Frame.Free;
  ARegisteredWriter.Frame := nil;
  jachLog.UnRegisterLogWriter(ARegisteredWriter.Writer);
  ARegisteredWriter.Writer.Free;
  ARegisteredWriter.Writer := nil;

  absrcAvailableWriters.Active := False;
  try
    FCreatedWriters.Delete(IndexOfWriter);
    FAvailableWriters.Add(ARegisteredWriter);
  finally
    absrcAvailableWriters.Active := True;
  end;
end;

procedure TfrmMain.swIsActiveSwitch(Sender: TObject);
begin
  jachLog.IsActive := swIsActive.IsChecked;
end;

procedure TfrmMain.swIsCachedSwitch(Sender: TObject);
begin
  jachLog.IsCached := swIsCached.IsChecked;
  if not jachLog.IsCached then
    jachLog.WriteCachedLog;
end;

procedure TfrmMain.swUseSeparateThreadToWriteSwitch(Sender: TObject);
begin
  jachLog.UseSeparateThreadToWrite := swUseSeparateThreadToWrite.IsChecked;
end;

procedure TfrmMain.TerminateAllContinuousThreads;
var
  lThread: TThread;
begin
  for lThread in FActiveContinuousThreads do
    lThread.Terminate;
  for lThread in FActiveContinuousThreads do
    lThread.WaitFor;
  for lThread in FActiveContinuousThreads do
    lThread.Free;
  FActiveContinuousThreads.Clear;
end;

procedure TfrmMain.tmrAdjustAllFramesHeightTimer(Sender: TObject);
begin
  tmrAdjustAllFramesHeight.Enabled := False;
  AdjustAllFramesHeight;
end;

procedure TfrmMain.tmrRemoveWriterTimer(Sender: TObject);
begin
  tmrRemoveWriter.Enabled := False;
  while FToRemoveWriters.Count > 0 do
    try
      RemoveWriter(FToRemoveWriters.Dequeue);
    except
      on E: Exception do
        jachLog.LogError('While removing writer', E);
    end;
end;

procedure TfrmMain.WriterFrameUserClose(Sender: TObject);
var
  lFrame: TfraWriterConfig;
  lWriter: TRegisteredWriter;
begin
  lFrame := Sender as TfraWriterConfig;
  lWriter := FindWriterByFrame(lFrame);
  if Assigned(lWriter) then
  begin
    FToRemoveWriters.Enqueue(lWriter);
    tmrRemoveWriter.Enabled := True;
  end
  else
    raise Exception.Create('Writer not found!');
end;

{ TRegisteredWriter }

procedure TRegisteredWriter.SetConfigRoutine(
  const Value: TProc<TjachLogWriter>);
begin
  FConfigRoutine := Value;
end;

procedure TRegisteredWriter.SetFrame(const Value: TfraWriterConfig);
begin
  FFrame := Value;
end;

procedure TRegisteredWriter.SetFrameClass(const Value: TfraWriterConfigClass);
begin
  FFrameClass := Value;
end;

procedure TRegisteredWriter.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TRegisteredWriter.SetWriter(const Value: TjachLogWriter);
begin
  FWriter := Value;
end;

procedure TRegisteredWriter.SetWriterClass(const Value: TjachLogWriterClass);
begin
  FWriterClass := Value;
end;

procedure TRegisteredWriter.SetWriterIsCreatedActive(const Value: Boolean);
begin
  FWriterIsCreatedActive := Value;
end;

{ TContinuousThread }

constructor TContinuousThread.Create;
begin
  inherited Create;
  FreeOnTerminate := False;
end;

procedure TContinuousThread.Execute;
  procedure MakeNewConnection(IPs: TStringList);
  var
    IP: string;
  begin
    IP := Format('192.168.%d.%d', [Random(5), 14 + Random(200)]);
    jachLog.logInfo(TOPIC_NETWORK, 'New connection is being established from IP %s', [IP]);
    if Random(30) = 1 then
      jachLog.LogWarning(TOPIC_SECURITY, 'Invalid security credentials from IP %s, connection dropped', [IP])
    else
      IPs.Add(IP);
  end;
var
  IP: string;
  IPs: TStringList;
begin
  IPs := TStringList.Create;
  try
    jachLog.LogInfo(TOPIC_GENERAL, 'Worker thread example with continuous log generation');
    while not Terminated do
    begin
      if jachLog.IsCached then
      begin
        Synchronize(
          procedure
          begin
            frmMain.swIsCached.IsChecked := False;
          end);
        jachLog.IsCached := False;
        jachLog.LogWarning('Do not use cached when continuous log generation, the continuous demo is not designed to run on cached mode.');
      end;

      while IPs.Count = 0 do
        MakeNewConnection(IPs);
      IP := IPs[Random(IPs.Count)];
      case Random(100) of
         0..9:
           MakeNewConnection(IPs);
         10..19:
           begin
             jachLog.logInfo(TOPIC_NETWORK, 'Connection from IP %s is terminated', [IP]);
             IPs.Delete(IPs.IndexOf(IP));
           end;
         20..39:
           jachLog.logInfo(TOPIC_GENERAL, 'Customer %5.5d modified from IP %s', [4000 + Random(40000), IP]);
         40..59:
           jachLog.logInfo(TOPIC_GENERAL, 'New order for Customer %5.5d from IP %s', [4000 + Random(40000), IP]);
         60..64:
           jachLog.logInfo(TOPIC_GENERAL, 'Customer %5.5d added from IP %s', [50000 + Random(1000), IP]);
         65,69:
             jachLog.logInfo(TOPIC_NETWORK, 'Network status: %d connections established', [IPs.Count]);
         97:
           jachLog.LogWarning(TOPIC_SECURITY, 'Invalid authorization token from IP %s', [IP]);
         98:
           jachLog.LogWarning(TOPIC_SECURITY, 'Invalid authorization token from IP %s', [IP]);
         99:
           begin
             jachLog.LogError(TOPIC_NETWORK, 'Network interface RESET, all %d connections dropped!', [IPs.Count]);
             IPs.Clear;
           end;
         else
           if IPs.Count > 0 then
           begin
             IP := IPs[Random(IPs.Count)];
             jachLog.logInfo(TOPIC_GENERAL, 'Order %6.6d for customer %5.5d is finished from IP %s', [2450000 + Random(10000), 4000 + Random(40000), IP]);
           end;
      end;
      if not Terminated then
        sleep(300 + Random(1000));
    end;
  finally
    IPs.Free;
  end;
end;

end.
