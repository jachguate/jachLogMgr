unit ufraWriterToSysLogIndyUDPConfig;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ufraWriterConfig, FMX.Controls.Presentation, FMX.Layouts, FMX.Edit,
  ujachLogMgr, ujachLogToSysLogIndyUDP, FMX.ListBox;

type
  TfraWriterToSysLogIndyUDPConfig = class(TfraWriterConfig)
    pnlHost: TPanel;
    lblHost: TLabel;
    edtHost: TEdit;
    pnlPort: TPanel;
    lblPort: TLabel;
    edtPort: TEdit;
    pnlIPVersion: TPanel;
    lblIPVersion: TLabel;
    cbxIPVersion: TComboBox;
    pnlFacility: TPanel;
    lblFacility: TLabel;
    cbxFacility: TComboBox;
    procedure edtHostValidate(Sender: TObject; var Text: string);
    procedure edtPortValidate(Sender: TObject; var Text: string);
    procedure cbxIPVersionChange(Sender: TObject);
    procedure cbxFacilityChange(Sender: TObject);
  private
    FWriter2: TjachLogToSysLogIndyUDP;
  public
    constructor Create(AOwner: TComponent; AWriter: TjachLogWriter); override;
  end;

var
  fraWriterToSysLogIndyUDPConfig: TfraWriterToSysLogIndyUDPConfig;

implementation

uses
  IdGlobal, IdSysLogMessage;

{$R *.fmx}

{ TfraWriterToSysLogIndyUDPConfig }

procedure TfraWriterToSysLogIndyUDPConfig.cbxFacilityChange(Sender: TObject);
begin
  inherited;
  FWriter2.idSysLogMessage.Facility := TIdSyslogFacility(cbxFacility.ItemIndex);
  cbxFacility.ItemIndex := Integer(FWriter2.idSysLogMessage.Facility);
end;

procedure TfraWriterToSysLogIndyUDPConfig.cbxIPVersionChange(Sender: TObject);
begin
  inherited;
  FWriter2.idSysLog.IPVersion := TIdIPVersion(cbxIPVersion.ItemIndex);
  cbxIPVersion.ItemIndex := Integer(FWriter2.idSysLog.IPVersion);
end;

constructor TfraWriterToSysLogIndyUDPConfig.Create(AOwner: TComponent;
  AWriter: TjachLogWriter);
begin
  inherited;
  FWriter2 := AWriter as TjachLogToSysLogIndyUDP;
  edtHost.Text := FWriter2.idSysLog.Host;
  edtPort.Text := FWriter2.idSysLog.Port.ToString;
  cbxIPVersion.ItemIndex := Integer(FWriter2.idSysLog.IPVersion);
  cbxFacility.ItemIndex := Integer(FWriter2.idSysLogMessage.Facility);
end;

procedure TfraWriterToSysLogIndyUDPConfig.edtHostValidate(Sender: TObject;
  var Text: string);
begin
  inherited;
  FWriter2.idSysLog.Host := Text.Trim;
  Text := FWriter2.idSysLog.Host;
end;

procedure TfraWriterToSysLogIndyUDPConfig.edtPortValidate(Sender: TObject;
  var Text: string);
var
  Port: Integer;
begin
  inherited;
  Port := StrToIntDef(Text.Trim, 0);
  if (Port > 0) and (Port <= 65535) then
    FWriter2.idSysLog.Port := Port;
  Text := FWriter2.idSysLog.Port.ToString;
end;

end.
