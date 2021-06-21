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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
  public
  end;

var
  Form3: TForm3;

implementation

uses
  UjachLogMgr;


{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
begin
  TjachLog.IsActive := not TjachLog.IsActive;
end;

procedure TForm3.Button2Click(Sender: TObject);
var
  I: Integer;
begin
  TjachLog.MaxFileSize := 50 * 1024;
  TjachLog.LogMessage('Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola Hola '
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
    TjachLog.LogMessage('Iteración %d', [I]);
end;

procedure TForm3.Button3Click(Sender: TObject);
begin
  TjachLog.LogWarn('Advertencia');
end;

procedure TForm3.Button4Click(Sender: TObject);
begin
  try
    raise Exception.Create('Esto es un error');
  except
    on E:Exception do
    begin
      TjachLog.LogError(E, 'Error al ejecutar proceso');
      raise;
    end;
  end;

end;

procedure TForm3.Button5Click(Sender: TObject);
begin
  TjachLog.LogFatalError('Error que jode todo');
end;

end.
