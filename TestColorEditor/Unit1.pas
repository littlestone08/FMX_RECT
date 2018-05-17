unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Colors, FMX.ListBox, FMX.Objects,
  FMX.Ani;

type
  TForm1 = class(TForm)
    SpeedButton1: TSpeedButton;
    GradientEdit1: TGradientEdit;
    ColorQuad1: TColorQuad;
    ColorComboBox1: TColorComboBox;
    ComboColorBox1: TComboColorBox;
    ColorPicker1: TColorPicker;
    Ellipse1: TEllipse;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Rectangle1: TRectangle;
    GradientAnimation1: TGradientAnimation;
    sbSave: TSpeedButton;
    sbLoad: TSpeedButton;
    Brush1: TBrushObject;
    Brush2: TBrushObject;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure sbLoadClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  FMX.Design.Brush;

{$R *.fmx}

procedure TForm1.sbLoadClick(Sender: TObject);
var
  stm: TMemoryStream;
  temp: TBrushObject;
begin
  stm:= TMemoryStream.Create;
  temp:= TBrushObject.Create(Nil);
  try
    stm.LoadFromFile('d:\Brush123');
    stm.ReadComponent(temp);
    self.Fill.Assign(temp.Brush);
  finally
    temp.Free;
    stm.Free;
  end;

end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  ShowGradientDialog(Rectangle1.Fill.Gradient);
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  ShowColorDialog(Self.Fill.Color);
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
  ShowBrushDialog(Rectangle1.Fill, [TBrushKind.None, TBrushKind.Solid, TBrushKind.Gradient, TBrushKind.Bitmap, TBrushKind.Resource],
    True);
end;

end.
