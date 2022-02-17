object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'MultiThreadedCacheDemo'
  ClientHeight = 341
  ClientWidth = 488
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    488
    341)
  PixelsPerInch = 96
  TextHeight = 13
  object lblThreadCount: TLabel
    Left = 434
    Top = 38
    Width = 46
    Height = 13
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = '0 threads'
    ExplicitLeft = 370
  end
  object lblPath: TLabel
    Left = 8
    Top = 5
    Width = 32
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    Caption = 'lblPath'
  end
  object Button1: TButton
    Left = 8
    Top = 33
    Width = 153
    Height = 25
    Caption = 'Spawn new thread'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 64
    Width = 472
    Height = 269
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object Button2: TButton
    Left = 167
    Top = 33
    Width = 153
    Height = 25
    Caption = 'Spawn 10 new threads'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Timer1: TTimer
    Interval = 50
    OnTimer = Timer1Timer
    Left = 16
    Top = 72
  end
end
