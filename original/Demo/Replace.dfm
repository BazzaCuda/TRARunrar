object ReplaceForm: TReplaceForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'replace'
  ClientHeight = 75
  ClientWidth = 253
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object existent: TLabel
    Left = 8
    Top = 8
    Width = 35
    Height = 13
    Caption = 'replace'
  end
  object archive: TLabel
    Left = 8
    Top = 27
    Width = 35
    Height = 13
    Caption = 'archive'
  end
  object replaceButton: TButton
    Left = 8
    Top = 46
    Width = 75
    Height = 25
    Caption = 'replace'
    TabOrder = 0
    OnClick = replaceButtonClick
  end
  object cancelButton: TButton
    Left = 170
    Top = 46
    Width = 75
    Height = 25
    Caption = 'cancel'
    TabOrder = 1
    OnClick = cancelButtonClick
  end
  object skipButton: TButton
    Left = 89
    Top = 46
    Width = 75
    Height = 25
    Caption = 'skip'
    TabOrder = 2
    OnClick = skipButtonClick
  end
end
