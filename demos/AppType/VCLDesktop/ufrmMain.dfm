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
  object richDemoText: TRichEdit
    Left = 0
    Top = 0
    Width = 526
    Height = 176
    TabStop = False
    Align = alClient
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
    ExplicitTop = 2
  end
  object Panel1: TPanel
    Left = 0
    Top = 176
    Width = 526
    Height = 74
    Align = alBottom
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
      Caption = 'Write some log to disk'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
end
