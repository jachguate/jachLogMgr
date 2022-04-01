unit ufraWriterConfig;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, ujachLogMgr, FMX.ListBox;

type
  TfraWriterConfig = class(TFrame)
    FlowLayout1: TFlowLayout;
    lblWriterName: TLabel;
    btnDestroyWriter: TButton;
    swActive: TSwitch;
    procedure btnDestroyWriterClick(Sender: TObject);
    procedure swActiveClick(Sender: TObject);
  private
    FWriter: TjachLogWriter;
    FOnUserClose: TNotifyEvent;
    procedure SetOnUserClose(const Value: TNotifyEvent);
    procedure DoOnUserClose;
    procedure CreateFilterControlsForTopic(ATopicIndex: TjachLogTopicIndex);
    procedure CreateFilterControlsForTopics;
    procedure cobxLevelChange(Sender: TObject);
  protected
    property Writer: TjachLogWriter read FWriter;
  public
    constructor Create(AOwner: TComponent; AWriter: TjachLogWriter); reintroduce; virtual;
    procedure AdjustHeight;
    property OnUserClose: TNotifyEvent read FOnUserClose write SetOnUserClose;
  end;

  TfraWriterConfigClass = class of TfraWriterConfig;

implementation

{$R *.fmx}

procedure TfraWriterConfig.AdjustHeight;
var
  MaxLayoutHeight: Single;
  I: Integer;
begin
  MaxLayoutHeight := 0;
  for I := 0 to FlowLayout1.ControlsCount - 1 do
    if FlowLayout1.Controls[I].Position.Y + FlowLayout1.Controls[I].Size.Height > MaxLayoutHeight then
      MaxLayoutHeight := FlowLayout1.Controls[I].Position.Y + FlowLayout1.Controls[I].Size.Height;
  FlowLayout1.Height := MaxLayoutHeight;
  Self.Height := FlowLayout1.Position.Y + MaxLayoutHeight;
end;

procedure TfraWriterConfig.btnDestroyWriterClick(Sender: TObject);
begin
  DoOnUserClose;
end;

procedure TfraWriterConfig.cobxLevelChange(Sender: TObject);
var
  lCombo: TComboBox;
begin
  lCombo := Sender as TComboBox;
  FWriter.LogLevel[lCombo.Tag] := TLogLevel(lCombo.ItemIndex);
  lCombo.ItemIndex := Integer(FWriter.LogLevel[lCombo.Tag]);


  if     (FWriter.LogLevel[lCombo.Tag] <> TLogLevel.llAll)
     and (jachLog.LogLevel[lCombo.Tag] <> TLogLevel.llAll)
  then
    ShowMessage('Warning!'#13
      + 'You''re mixing filters at two levels. At log level, which are applied globally and at the writer level, which affects what is delivered to that destination.'#13
      + 'It is recommended you apply filters only at one level, unless you really know what you''re doing, since it may be confussing determining if all the desired info is really reaching the destination.'#13
      + 'To apply filter only at one level, set the other level to llAll, so it doesn''t filter anything there.'
    );
end;

constructor TfraWriterConfig.Create(AOwner: TComponent;
  AWriter: TjachLogWriter);

begin
  inherited Create(AOwner);
  FWriter := AWriter;
  swActive.IsChecked := FWriter.IsActive;
  CreateFilterControlsForTopics;
end;

procedure TfraWriterConfig.CreateFilterControlsForTopic(
  ATopicIndex: TjachLogTopicIndex);
var
  lPanel: TPanel;
  lLabel: TLabel;
  lCombo: TComboBox;
begin
  lPanel := TPanel.Create(Self);
  lPanel.Width := 160;
  lPanel.Height := 22;
  lPanel.Parent := FlowLayout1;
  lCombo := TComboBox.Create(Self);
  lCombo.Parent := lPanel;
  lCombo.Align := TAlignLayout.Right;
  lCombo.Width := 80;
  lCombo.Items.Text := 'Off'#13'Emergency'#13'Alert'#13'Critical'#13'Error'#13'Warning'#13'Notice'#13'Info'#13'Debug'#13'All';
  lCombo.ItemIndex := Integer(FWriter.LogLevel[ATopicIndex]);
  lCombo.Tag := ATopicIndex;
  lCombo.OnChange := cobxLevelChange;
  lLabel := TLabel.Create(Self);
  lLabel.Parent := lPanel;
  lLabel.Align := TAlignLayout.Client;
  lLabel.Text := jachLog.TopicName[ATopicIndex] + ' filter';
end;

procedure TfraWriterConfig.CreateFilterControlsForTopics;
var
  I: Integer;
begin
  for I := Low(TjachLogTopicIndex) to High(TjachLogTopicIndex) do
    if jachLog.TopicName[I] <> '' then
      CreateFilterControlsForTopic(I)
    else
      break;
end;

procedure TfraWriterConfig.DoOnUserClose;
begin
  if Assigned(FOnUserClose) then
    FOnUserClose(Self);
end;

procedure TfraWriterConfig.SetOnUserClose(const Value: TNotifyEvent);
begin
  FOnUserClose := Value;
end;

procedure TfraWriterConfig.swActiveClick(Sender: TObject);
begin
  FWriter.IsActive := swActive.IsChecked;
end;

end.
