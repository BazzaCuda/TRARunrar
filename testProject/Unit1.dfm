object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object RAR1: TRAR
    onError = RAR1Error
    onListFile = RAR1ListFile
    onPasswordRequired = RAR1PasswordRequired
    onNextVolumeRequired = RAR1NextVolumeRequired
    onProgress = RAR1Progress
    onReplace = RAR1Replace
    Left = 256
    Top = 160
  end
end
