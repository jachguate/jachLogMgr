object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'jachLog VCL Desktop demo'
  ClientHeight = 414
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
    Height = 258
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
    ExplicitHeight = 156
  end
  object Panel1: TPanel
    Left = 0
    Top = 258
    Width = 526
    Height = 156
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      526
      156)
    object Label1: TLabel
      Left = 16
      Top = 74
      Width = 22
      Height = 13
      Caption = 'Host'
    end
    object Label2: TLabel
      Left = 16
      Top = 101
      Width = 20
      Height = 13
      Caption = 'Port'
    end
    object Label3: TLabel
      Left = 16
      Top = 128
      Width = 33
      Height = 13
      Caption = 'Facility'
    end
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
    object edtHost: TEdit
      Left = 80
      Top = 70
      Width = 150
      Height = 21
      TabOrder = 2
      Text = 'localhost'
    end
    object edtPort: TEdit
      Left = 80
      Top = 97
      Width = 150
      Height = 21
      TabOrder = 3
      Text = '514'
    end
    object cmbFacility: TComboBox
      Left = 80
      Top = 124
      Width = 150
      Height = 21
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 4
      Text = 'User Level'
      Items.Strings = (
        'Kernel'
        'User Level'
        'Mail System'
        'System Daemon'
        'Security One'
        'SysLog Internal'
        'LPR'
        'NNTP'
        'UUCP'
        'Clock Daemon One'
        'Security Two'
        'FTP Daemon'
        'NTP'
        'LogAudit'
        'LogAlert'
        'Clock Daemon Two'
        'LocalUse Zero'
        'LocalUse One'
        'LocalUse Two'
        'LocalUse Three'
        'LocalUse Four'
        'LocalUse Five'
        'LocalUse Six'
        'LocalUse Seven')
    end
  end
end
