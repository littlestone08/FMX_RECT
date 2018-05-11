unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    FBmp: TBitmap;
    FAngle: Single;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  System.Math, System.Math.Vectors;

{$R *.fmx}


procedure DrawRotatedBitmap(const Canvas : TCanvas; const Bitmap : TBitmap;
  const PointA, PointB : TPointF; const Offset : TPointF; const Scale : Single);
var
  OldMatrix, TranslationAlongLineMatrix, RotationMatrix, TranslationMatrix,
    ScaleMatrix, FinalMatrix: TMatrix;
  W, H : Single;
  SrcRect, DestRect: TRectF;
  Corner: TPointF;
  LineLength : Single;
  LineAngleDeg : Integer;
begin
  OldMatrix := Canvas.Matrix; // Original, to restore
  try
    {$ifdef DRAW_HELPERS}
      Canvas.Fill.Color := TAlphaColorRec.Black;
      Canvas.DrawLine(PointA, PointB, 0.5);
    {$endif}

    W := PointB.X - PointA.X;
    H := PointA.Y - PointB.Y;
    LineLength := abs(PointA.Distance(PointB));

    // Looking for the middle of the task line
    // and the coordinates of the image left upper angle
    // solving the proportion width/linelength=xo/0.5requireddimensions
    Corner := TPointF.Create((PointB.X + PointA.X) / 2, (PointA.Y + PointB.Y) / 2);// Middle
    {$ifdef DRAW_HELPERS}
      Canvas.Stroke.Color := TAlphaColorRec.Red;
      Canvas.DrawEllipse(TRectF.Create(Corner,2,2),1);
    {$endif}
    Corner.X := Corner.X - Bitmap.Width / 2 * W / LineLength;
    Corner.Y := Corner.Y + Bitmap.Width / 2 * H / LineLength;
    {$ifdef DRAW_HELPERS}
      Canvas.Stroke.Color := TAlphaColorRec.Green;
      Canvas.DrawEllipse(TRectF.Create(Corner,2,2),1);
    {$endif}

    // Account for scale (if the FMX control is scaled / zoomed); translation
    // (the control may not be located at (0, 0) in its parent form, so its canvas
    // is offset) and rotation
    ScaleMatrix := TMatrix.CreateScaling(Scale, Scale);
    TranslationMatrix := TMatrix.CreateTranslation(Offset.X, Offset.Y);
    RotationMatrix := TMatrix.CreateRotation(-ArcTan2(H, W));
    TranslationAlongLineMatrix := TMatrix.CreateTranslation(Corner.X, Corner.Y);
    FinalMatrix := ((RotationMatrix * ScaleMatrix) * TranslationMatrix) * TranslationAlongLineMatrix;

    // If in the top left or top right quadrants, the image will appear
    // upside down. So, rotate the image 180 degrees
    // This is useful when the image contains text, ie is an annotation or similar,
    // or needs to always appear "under" the line
    LineAngleDeg := Round(RadToDeg(-Arctan2(H, W)));
    case LineAngleDeg of
      -180..-90,
      90..180 : FinalMatrix := TMatrix.CreateRotation(DegToRad(180)) * TMatrix.CreateTranslation(Bitmap.Width, 0) * FinalMatrix;
    end;

    Canvas.SetMatrix(FinalMatrix);

    // And finally draw the bitmap
    DestRect := TRectF.Create(PointF(0, 0), Bitmap.Width, Bitmap.Height);
    SrcRect := TRectF.Create(0, 0, Bitmap.Width, Bitmap.Height);
    {$ifdef DRAW_HELPERS}
      Canvas.DrawBitmap(Bitmap, SrcRect, DestRect, 0.5);
    {$else}
      Canvas.DrawBitmap(Bitmap, SrcRect, DestRect, 1);
    {$endif}
  finally
    // Restore the original matrix
    Canvas.SetMatrix(OldMatrix);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FAngle:= FAngle + 1;
  self.Invalidate();
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  da: TDateTime;
begin
  Da:= EncodeDate(2018, 3, 18);
  da:= da + 99;
  caption:= FormatDateTime('MM-DD', da);
  FBmp:= TBitmap.Create;
  FBmp.LoadFromFile('C:\Users\Public\Pictures\Sample Pictures\th.jpg');
//  FBmp.Rotate();
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FBmp.Free;
end;

procedure TForm1.FormPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  OldMatrix: TMatrix;
//  PointA, PointB: TPointF;
  W, H: Single;
  RotationMatrix: TMatrix;
  M2: TMatrix;
  dest: TRectF;
begin
  OldMatrix := Canvas.Matrix; // Original, to restore

//  W := PointB.X - PointA.X;
//  H := PointA.Y - PointB.Y;
  RotationMatrix := TMatrix.CreateRotation(FAngle);
  RotationMatrix := TMatrix.CreateRotation(0);


  M2:= TMatrix.Identity;
//  M2.m31 := width / 2;
  M2.m32 := FAngle;
  RotationMatrix :=RotationMatrix * M2;
  Canvas.SetMatrix(OldMatrix * RotationMatrix);

//  Canvas.DrawBitmap(FBmp, TRectF.Create(Height/ 2, Width / 2, Width, Height), FBmp.Bounds, 1);
  Dest:= FBmp.BoundsF;
  Dest:= FBmp.BoundsF;
  Dest.Offset(Width / 2 , 0);
  Canvas.DrawBitmap(FBmp, FBmp.Bounds, Dest,1);

  Canvas.SetMatrix(OldMatrix);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Button1Click(Button1);
end;

initialization
//GlobalUseGPUCanvas := True;
end.
