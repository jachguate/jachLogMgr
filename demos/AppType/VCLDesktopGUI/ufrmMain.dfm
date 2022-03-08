object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'jachLog VCL Desktop demo'
  ClientHeight = 458
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
  end
  object Panel1: TPanel
    Left = 0
    Top = 159
    Width = 526
    Height = 74
    Align = alTop
    TabOrder = 1
    DesignSize = (
      526
      74)
    object btnOpenLogFolder: TButton
      Left = 8
      Top = 39
      Width = 510
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Open log folder'
      TabOrder = 0
      OnClick = btnOpenLogFolderClick
    end
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 510
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Write some log (same thread)'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object richLog: TRichEdit
    Left = 0
    Top = 233
    Width = 526
    Height = 225
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 2
    WordWrap = False
    Zoom = 100
  end
end
