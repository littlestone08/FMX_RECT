object Form1: TForm1
  Left = 222
  Top = 114
  Caption = 'Form1'
  ClientHeight = 154
  ClientWidth = 476
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    476
    154)
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox1: TPaintBox
    Left = 0
    Top = 0
    Width = 476
    Height = 154
    Align = alClient
    Anchors = []
    OnPaint = PaintBox1Paint
    ExplicitLeft = 16
    ExplicitWidth = 105
    ExplicitHeight = 105
  end
  object Button1: TButton
    Left = 109
    Top = 117
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Caption = #25171#24320
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 206
    Top = 117
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Caption = #25773#25918
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 303
    Top = 117
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Caption = #26242#20572
    TabOrder = 2
    OnClick = Button3Click
  end
  object OpenDialog1: TOpenDialog
    FileName = 'D:\WORK170908\PCIeReceiver\Test\acs_test\res\*.wav'
    Left = 128
    Top = 24
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 128
    Top = 72
  end
end
