object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 360
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 137
    Height = 25
    Caption = 'Toggle Active'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 39
    Width = 137
    Height = 25
    Caption = 'Write Msg'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 70
    Width = 137
    Height = 25
    Caption = 'Write Warn'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 8
    Top = 101
    Width = 137
    Height = 25
    Caption = 'Write Error'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 8
    Top = 132
    Width = 137
    Height = 25
    Caption = 'Write FatalError'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 248
    Top = 8
    Width = 125
    Height = 25
    Caption = 'Clear cache'
    TabOrder = 5
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 248
    Top = 39
    Width = 125
    Height = 25
    Caption = 'Cache write Msg'
    TabOrder = 6
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 248
    Top = 70
    Width = 125
    Height = 25
    Caption = 'Cache write Warn'
    TabOrder = 7
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 248
    Top = 101
    Width = 125
    Height = 25
    Caption = 'Cache write Error'
    TabOrder = 8
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 248
    Top = 132
    Width = 125
    Height = 25
    Caption = 'Cache write Fatal Error'
    TabOrder = 9
    OnClick = Button10Click
  end
  object Button11: TButton
    Left = 248
    Top = 163
    Width = 125
    Height = 25
    Caption = 'Write cache'
    TabOrder = 10
    OnClick = Button11Click
  end
  object Button12: TButton
    Left = 248
    Top = 194
    Width = 125
    Height = 25
    Caption = 'Force write cache'
    TabOrder = 11
    OnClick = Button12Click
  end
  object Button13: TButton
    Left = 248
    Top = 225
    Width = 125
    Height = 25
    Caption = 'Cache 1000 messages'
    TabOrder = 12
    OnClick = Button13Click
  end
end
