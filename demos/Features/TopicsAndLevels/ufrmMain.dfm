object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Topics and level filter demo'
  ClientHeight = 301
  ClientWidth = 634
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object richDemoText: TRichEdit
    Left = 0
    Top = 0
    Width = 634
    Height = 166
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
    ExplicitHeight = 129
  end
  object Panel1: TPanel
    Left = 0
    Top = 166
    Width = 634
    Height = 135
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 560
    DesignSize = (
      634
      135)
    object Label1: TLabel
      Left = 8
      Top = 97
      Width = 42
      Height = 13
      Caption = 'Log level'
    end
    object Label2: TLabel
      Left = 8
      Top = 70
      Width = 25
      Height = 13
      Caption = 'Topic'
    end
    object btnOpenLogFolder: TButton
      Left = 8
      Top = 39
      Width = 618
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Open log folder'
      TabOrder = 0
      OnClick = btnOpenLogFolderClick
      ExplicitWidth = 510
    end
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 618
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Generate log'
      TabOrder = 1
      OnClick = Button1Click
    end
    object cboxLevel: TComboBox
      Left = 71
      Top = 94
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 7
      TabOrder = 2
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
    object cboxTopic: TComboBox
      Left = 71
      Top = 67
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      OnClick = cboxTopicClick
    end
    object chbxIncludeTopicName: TCheckBox
      Left = 232
      Top = 69
      Width = 97
      Height = 17
      Caption = 'Topic name'
      TabOrder = 4
      OnClick = chbxIncludeTopicNameClick
    end
  end
end
