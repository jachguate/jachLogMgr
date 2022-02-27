object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'jachLog VCL Desktop demo'
  ClientHeight = 250
  ClientWidth = 526
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 156
    Width = 526
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 95
  end
  object richDemoText: TRichEdit
    Left = 0
    Top = 0
    Width = 526
    Height = 156
    TabStop = False
    Align = alTop
    Color = clBtnFace
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 0
    Zoom = 100
    ExplicitTop = -3
  end
  object Panel1: TPanel
    Left = 0
    Top = 159
    Width = 526
    Height = 74
    Align = alTop
    TabOrder = 1
    ExplicitLeft = -8
    ExplicitTop = 172
    object btnOpenLogFolder: TButton
      Left = 8
      Top = 39
      Width = 510
      Height = 25
      Caption = 'Open log folder'
      TabOrder = 0
      OnClick = btnOpenLogFolderClick
    end
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 510
      Height = 25
      Caption = 'Write some log'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
end
