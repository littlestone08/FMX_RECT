unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.DateUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Effects,
  FMX.Filter.Effects, FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Layouts, FMX.ExtCtrls, uRadioSpectrumChart, FMXTee.Engine, FMXTee.Series,
  FMXTee.Procs, FMXTee.Chart;

type


  TForm3 = class(TForm)
    Button3: TButton;
    Timer1: TTimer;
    Button1: TButton;
    Button2: TButton;
    SignalChart1: TSignalChart;
    Test1: TTest;
    procedure Button3Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure SignalChart1Resized(Sender: TObject);
  private
    { Private declarations }
    FData: TArray<Single>;
    FStop: Boolean;
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
  end;

var
  Form3: TForm3;

implementation


{$R *.fmx}


procedure TForm3.Button1Click(Sender: TObject);
begin
  FStop:= True;
  Timer1.Enabled:= False;
end;

procedure TForm3.Button2Click(Sender: TObject);
var
  x: TBitmap;
begin
  SignalChart1.AxisesData.Left.MinValue:=   self.SignalChart1.AxisesData.Left.MinValue - 1;
  SignalChart1.InvalidateRect(SignalChart1.ClipRect);

  SignalChart1.Repaint;
//  RadioSpectrumChart1.InvalidateRect(RadioSpectrumChart1.BoundsRect);
  x:= TBitmap.Create;
  try
    x.Width:= 100;
    x.Height:= 100;
    x.Canvas.Stroke.Color:= TAlphaColors.Red;
    x.Canvas.Stroke.Kind:= TBrushKind.Solid;
    x.Canvas.DrawLine(TPointF.Create(0, 0), TPointF.Create(100, 100), 1);
    x.SaveToFile('d:\1.png');
  finally
    x.Free;
  end;
end;

procedure TForm3.Button3Click(Sender: TObject);
begin
  Timer1.Enabled:= True;
  FStop:= False;
//  while not FStop do
//  begin
//    Timer1Timer(Nil);
//    Application.ProcessMessages;
//  end;
end;

constructor TForm3.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TForm3.Destroy;
begin

  inherited;
end;

procedure TForm3.FormCreate(Sender: TObject);

begin
  SetLength(FData, 1024);
end;

procedure TForm3.FormPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
Procedure TestText;
var
  ARect: TRectF;
begin
//  ARect:= TRectF.Create(0, 0, 10, Canvas.TextHeight(' '));
  ARect:= TRectF.Create(0, 0, 200, 200);
  Canvas.MeasureText(ARect, '测试的文字', False, [], TTextAlign.Leading, TTextAlign.Leading);
//  Canvas.FillRect(ARect, 0, 0, [], 1);
  Canvas.FillText(ARect, '测试的文字', False, 1, [], TTextAlign.Center, TTextAlign.Center);
  Canvas.Stroke.Kind:= TBrushKind.Solid;
  Canvas.DrawRect(ARect, 0, 0, [], 1);
end;
procedure test();
var
  x: TBitmap;
begin
//  RadioSpectrumChart1.InvalidateRect(RadioSpectrumChart1.BoundsRect);
  x:= TBitmap.Create;
  try
    x.Width:= 100;
    x.Height:= 100;
//    x.Canvas.Stroke.Color:= TAlphaColors.Red;
//    x.Canvas.Stroke.Kind:= TBrushKind.Solid;
//    x.Canvas.DrawLine(TPointF.Create(0, 0), TPointF.Create(100, 100), 1);
//    x.SaveToFile('d:\1.png');
//    x.LoadFromFile('C:\Users\Public\Pictures\Sample Pictures\202830_1383931710.png');

    x.Canvas.Stroke.Color:= TAlphaColors.Red;
    x.Canvas.Stroke.Kind:= TBrushKind.Solid;
    x.Canvas.Stroke.Thickness:= 10;

    x.Canvas.Fill.Color:= TAlphaColors.Lime;
    x.Canvas.Fill.Kind:= TBrushKind.Solid;

    x.Canvas.FillRect(TRectF.Create(2, 2, 200, 200), 0, 0, [], 1);
//    x.Canvas.DrawRect(TRectF.Create(1, 1, 201, 201), 0, 0, [], 1);

    x.Canvas.BeginScene();
    x.Canvas.DrawLine(TPointF.Create(2, 2), TPointF.Create(200, 200), 1);

//    Canvas.Stroke.Color:= TAlphaColors.Lime;
//    Canvas.Stroke.Kind:= TBrushKind.Solid;
//    Canvas.DrawRect(TRectF.Create(2, 2, 100, 200), 0, 0, [], 1);
    x.Canvas.EndScene;
    Canvas.BeginScene();
    Canvas.DrawBitmap(x, x.Bounds, Self.ClientRect, 1);
    Canvas.EndScene;

  finally
    x.Free;
  end;

end;
begin
//  test;
  TestText;
end;

procedure TForm3.SignalChart1Resized(Sender: TObject);
begin
//  CnDebugger.LogMsg('Resized');
  self.Fill.Color
end;

procedure TForm3.Timer1Timer(Sender: TObject);
var
  i: Integer;
begin
  SetLength(FData, 1024);
  for i := 0 to Length(FData) - 1 do
  begin
    FData[i]:= Random(141) + 1;
//    FData[i]:= 141;
  end;

//  self.RadioSpectrumChart1.DrawData([
//  1,2,3,4,5,6,7,8,9,10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
//  22, 23, 24, 25, 26, 27, 28, 29, 30, 31
//  ]);

  SignalChart1.DrawData(FData);
  Caption:= IntToStr(SignalChart1.FPS);
  Application.ProcessMessages;

end;



end.
