unit ufrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm3 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
  public
  end;

var
  Form3: TForm3;

implementation

uses
  UjachLogMgr, ujachLogAuto, ujachLogClasses;


{$R *.dfm}

procedure TForm3.Button13Click(Sender: TObject);
var
  I: Integer;
begin
end;

procedure TForm3.Button1Click(Sender: TObject);
begin
  ShowMessage('SizeOf(TLogSeverity)=' + IntToStr(SizeOf(TLogSeverity)));
end;

procedure TForm3.Button2Click(Sender: TObject);
var
  I: Integer;
begin
  //TjachLog.MaxFileSize := 50 * 1024;
  jachLog.LogInfo('Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola '
    + 'Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola '
    + 'Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola '
    + 'Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola '
    + 'Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola '
    + 'Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola '
    + 'Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola '
    + 'Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola '
    + 'Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola '
    + 'Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola '
    + 'Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola '
    + 'Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola '
  );
  for I := 1 to 10 do
    jachLog.LogInfo('Iteración %d', [I]);
end;

procedure TForm3.Button3Click(Sender: TObject);
begin
  jachLog.LogWarning('Advertencia');
end;

procedure TForm3.Button4Click(Sender: TObject);
begin
  try
    raise Exception.Create('Esto es un error');
  except
    on E:Exception do
    begin
      jachLog.LogError('Error al ejecutar proceso', E);
      raise;
    end;
  end;

end;

procedure TForm3.Button5Click(Sender: TObject);
begin
  jachLog.LogCritical('Error que jode todo');
end;

procedure TForm3.Button6Click(Sender: TObject);
begin
  jachLog.CacheClear;
end;

end.
