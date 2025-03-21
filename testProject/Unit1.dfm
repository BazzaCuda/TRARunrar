object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 294
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 15
  object label1: TLabel
    Left = 56
    Top = 145
    Width = 31
    Height = 15
    Caption = 'Error: '
  end
  object lblError: TLabel
    Left = 96
    Top = 145
    Width = 170
    Height = 15
    Caption = 'error codes will be reported here'
  end
  object Label2: TLabel
    Left = 56
    Top = 53
    Width = 325
    Height = 15
    Caption = ' (copy and paste the full path to your alternative archive here)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsItalic]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 280
    Top = 83
    Width = 149
    Height = 15
    Caption = 'Archives or Files in Archive'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 280
    Top = 213
    Width = 297
    Height = 30
    Caption = 
      'N.B. The supplied test archive doesn'#39't have an encrypted header,' +
      '  so a password isn'#39't required to just list the files.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsItalic]
    ParentFont = False
    WordWrap = True
  end
  object Label5: TLabel
    Left = 400
    Top = 53
    Width = 212
    Height = 15
    Caption = 'Try using an invalid password for Extract'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsItalic]
    ParentFont = False
  end
  object lblLastError: TLabel
    Left = 150
    Top = 188
    Width = 88
    Height = 15
    Caption = 'Last result code: '
  end
  object edtRARArchive: TLabeledEdit
    Left = 56
    Top = 24
    Width = 324
    Height = 23
    EditLabel.Width = 70
    EditLabel.Height = 15
    EditLabel.Caption = 'RAR Archive'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -12
    EditLabel.Font.Name = 'Segoe UI'
    EditLabel.Font.Style = [fsBold]
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Text = '..\..\testArchive.rar'
  end
  object btnListFiles: TButton
    Left = 168
    Top = 101
    Width = 75
    Height = 25
    Caption = 'List Files'
    TabOrder = 1
    OnClick = btnListFilesClick
  end
  object Memo1: TMemo
    Left = 280
    Top = 102
    Width = 313
    Height = 105
    TabOrder = 2
  end
  object btnExtract: TButton
    Left = 56
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Extract'
    TabOrder = 3
    OnClick = btnExtractClick
  end
  object edtExtractPath: TLabeledEdit
    Left = 56
    Top = 256
    Width = 324
    Height = 23
    EditLabel.Width = 68
    EditLabel.Height = 15
    EditLabel.Caption = 'Extract Path'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -12
    EditLabel.Font.Name = 'Segoe UI'
    EditLabel.Font.Style = [fsBold]
    EditLabel.ParentFont = False
    TabOrder = 4
    Text = '.\extracted_files\'
  end
  object edtPassword: TLabeledEdit
    Left = 400
    Top = 24
    Width = 193
    Height = 23
    EditLabel.Width = 171
    EditLabel.Height = 15
    EditLabel.Caption = 'Password (default is in the code)'
    TabOrder = 5
    Text = ''
  end
  object btnFindFiles: TButton
    Left = 64
    Top = 101
    Width = 83
    Height = 25
    Caption = 'Find Files'
    TabOrder = 6
    OnClick = btnFindFilesClick
  end
  object RAR1: TRAR
    onError = RAR1Error
    onListFile = RAR1ListFile
    onPasswordRequired = RAR1PasswordRequired
    onNextVolumeRequired = RAR1NextVolumeRequired
    onProgress = RAR1Progress
    Top = 8
  end
end
