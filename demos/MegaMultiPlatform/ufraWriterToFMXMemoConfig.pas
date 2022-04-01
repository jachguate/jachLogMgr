unit ufraWriterToFMXMemoConfig;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ufraWriterConfig, FMX.Controls.Presentation, FMX.Layouts, ujachLogMgr,
  ujachLogToFMXMemo, FMX.Edit;

type
  TfraWriterToFMXMemoConfig = class(TfraWriterConfig)
    pnlRefreshInterval: TPanel;
    lblRefreshInterval: TLabel;
    edtRefreshInterval: TEdit;
    pnlMaxLineSize: TPanel;
    lblMaxLineSize: TLabel;
    edtMaxLineSize: TEdit;
    pnlDateTimeFormat: TPanel;
    lblDateTimeFormat: TLabel;
    edtDateTimeFormat: TEdit;
    procedure edtRefreshIntervalValidate(Sender: TObject; var Text: string);
    procedure edtMaxLineSizeValidate(Sender: TObject; var Text: string);
    procedure edtDateTimeFormatValidate(Sender: TObject; var Text: string);
  private
    FWriter2: TjachLogToFMXMemo;
  public
    constructor Create(AOwner: TComponent; AWriter: TjachLogWriter); override;
  end;

implementation

{$R *.fmx}

{ TfraWriterConfig1 }

constructor TfraWriterToFMXMemoConfig.Create(AOwner: TComponent;
  AWriter: TjachLogWriter);
begin
  inherited;
  FWriter2 := AWriter as TjachLogToFMXMemo;
  edtRefreshInterval.Text := FWriter2.RefreshInterval.ToString;
  edtMaxLineSize.Text := FWriter2.MaxLineSize.ToString;
  edtDateTimeFormat.Text := FWriter2.DateTimeFormat;
end;

procedure TfraWriterToFMXMemoConfig.edtDateTimeFormatValidate(Sender: TObject;
  var Text: string);
var
  s: string;
begin
  inherited;
  if Text.Trim <> '' then
  begin
    s := FormatDateTime(Text.Trim, Now);
    FWriter2.DateTimeFormat := Text.Trim;
  end;
  Text := FWriter2.DateTimeFormat;
end;

procedure TfraWriterToFMXMemoConfig.edtMaxLineSizeValidate(Sender: TObject;
  var Text: string);
var
  Size: Integer;
begin
  inherited;
  if     TryStrToInt(Text.Trim, Size)
     and ((Size > 0) or (Size = -1))
  then
    FWriter2.MaxLineSize := Size;
  Text := FWriter2.MaxLineSize.ToString;
end;

procedure TfraWriterToFMXMemoConfig.edtRefreshIntervalValidate(Sender: TObject;
  var Text: string);
var
  Interval: Integer;
begin
  inherited;
  Interval := StrToIntDef(Text.Trim, 0);
  if Interval > 0 then
    FWriter2.RefreshInterval := Interval;
  Text := FWriter2.RefreshInterval.ToString;
end;

end.
