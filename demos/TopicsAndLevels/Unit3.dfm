object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'LevelFilter'
  ClientHeight = 127
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
    127)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 61
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
  object Label2: TLabel
    Left = 8
    Top = 34
    Width = 25
    Height = 13
    Caption = 'Topic'
  end
  object cboxLevel: TComboBox
    Left = 71
    Top = 58
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemIndex = 7
    TabOrder = 1
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
    Top = 85
    Width = 208
    Height = 25
    Caption = 'Generate log'
    TabOrder = 2
    OnClick = Button1Click
  end
  object cboxTopic: TComboBox
    Left = 71
    Top = 31
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnClick = cboxTopicClick
  end
  object chbxIncludeTopicName: TCheckBox
    Left = 232
    Top = 33
    Width = 97
    Height = 17
    Caption = 'Topic name'
    TabOrder = 3
    OnClick = chbxIncludeTopicNameClick
  end
end
