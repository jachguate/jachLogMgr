object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'LevelFilter'
  ClientHeight = 142
  ClientWidth = 350
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    350
    142)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 35
    Width = 42
    Height = 13
    Caption = 'Log level'
  end
  object lblPath: TLabel
    Left = 8
    Top = 5
    Width = 32
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    Caption = 'lblPath'
  end
  object cboxLevel: TComboBox
    Left = 71
    Top = 32
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemIndex = 7
    TabOrder = 0
    Text = 'Info'
    OnChange = cboxLevelChange
    Items.Strings = (
      'Off'
      'Emergency'
      'Alert'
      'Critical'
      'Error'
      'Warning'
      'Notice'
      'Info'
      'Debug')
  end
  object Button1: TButton
    Left = 8
    Top = 59
    Width = 208
    Height = 25
    Caption = 'Generate log'
    TabOrder = 1
    OnClick = Button1Click
  end
end
