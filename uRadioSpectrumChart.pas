unit uRadioSpectrumChart;

interface


uses
  System.Classes, System.SysUtils, System.Types, System.UITypes,
  System.Math, System.DateUtils, System.IOUtils,
  FMX.Objects, FMX.Graphics, FMX.Types;

type

  TFrameCount = Class
  Strict Private
    FStamps: TArray<TDateTime>;
    function GetFPS: Integer;
  Public
    Procedure AddFrame;
    Property FPS: Integer Read GetFPS;
  End;

  TLine = Record
    StartPoint, EndPoint: TPointF;
  Public
    Constructor Create(const A: TPointF; const B: TPointF);
  end;


  TSignalChart = Class;

  TCustomAxis = Class(TPersistent)
  Private
    [weak]FChart: TSignalChart;
    FLines: TArray<TLine>;
    FMinValue: Integer;
    FThinkness: Single;
    FMaxValue: Integer;
    FLineColor: TAlphaColor;
    procedure SetMaxValue(const Value: Integer);
    procedure SetMinValue(const Value: Integer);
    procedure SetThinkness(const Value: Single);
    procedure SetLineColor(const Value: TAlphaColor);
    Procedure DoChanged;
  Protected
    function CalcuLabelR(const Line: TLine; const StdTextRect: TRectF; const offset: TPointF): TRectF;
      Virtual; Abstract;
    function CalcuMaxLabel(PanelBmp: TBitmap; StdTextR: TRectF): Integer;
      Virtual; Abstract;
    function LableSuffix: String; virtual; Abstract;
    Function IsVertical: Boolean; Virtual; Abstract;
    function CalcuLableText(Index: Integer; LabelStep: Single): String;
      Virtual; Abstract;
    function GetHTextAlign: TTextAlign; Virtual; Abstract;
  Public
    Constructor Create(AChart: TSignalChart); Virtual;

    Class function CalcuStep(Range: Integer; N: Integer): Integer;
    Class Function CalcuValueStep(Limits: Single;
      MaxLableCount: Integer): Integer;
    Class Procedure UpdateLinesPos(var Lines: TArray<TLine>;
      MarkValueLimits, WidthLimits, HeightLimits: Integer;
      MaxLineCount: Integer; isVeritcal: Boolean);
    Procedure DrawGrid(Coordinate: TBitmap; MaxLableCount: Integer);
    Procedure DrawLables(const CoordinatePos: TPointF; ACanvas: TCanvas;
      StdTextR: TRectF);

  Published
    Property MinValue: Integer read FMinValue write SetMinValue;
    Property MaxValue: Integer read FMaxValue write SetMaxValue;
    Property Thinkness: Single read FThinkness write SetThinkness;
    Property LineColor: TAlphaColor read FLineColor write SetLineColor;
    Property HTextAlign: TTextAlign Read GetHTextAlign;

  End;

  TLeftAxis = Class(TCustomAxis)
  Protected
    function LableSuffix: String; Override;
    function CalcuLabelR(const Line: TLine; const StdTextRect: TRectF; const offset: TPointF)
      : TRectF; override;
    Function IsVertical: Boolean; Override;
    function CalcuMaxLabel(PanelBmp: TBitmap; StdTextR: TRectF)
      : Integer; Override;
    function CalcuLableText(Index: Integer; LableStep: Single): String;
      Override;
    function GetHTextAlign: TTextAlign; Override;
  Public
    Constructor Create(AChart: TSignalChart); Override;
  end;

  TBottomAxis = Class(TCustomAxis)
  Protected
    function LableSuffix: String; Override;
    function CalcuLabelR(const Line: TLine; const StdTextRect: TRectF; const offset: TPointF)
      : TRectF; override;
    Function IsVertical: Boolean; Override;
    function CalcuMaxLabel(PanelBmp: TBitmap; StdTextR: TRectF)
      : Integer; Override;
    function CalcuLableText(Index: Integer; LabelStep: Single): String;
      Override;
    function GetHTextAlign: TTextAlign; Override;
  Public
    Constructor Create(AChart: TSignalChart); Override;
  end;

  TLinearEquations = Class
  Private
    FConst1st: Single;
    FConst0: Single;
  Public
    Procedure CalcuCoff(X1, Y1, X2, Y2: Single);
    Function CalcuY(X: Single): Single;
  End;



  TAxises = class(TPersistent)
  Strict Private
    FLeft: TCustomAxis;
    FBottom: TCustomAxis;
  Public
    Constructor Create(AChart: TSignalChart);
    Destructor Destroy; Override;
  Published
    Property Left: TCustomAxis Read FLeft Write FLeft;
    Property Bottom: TCustomAxis Read FBottom Write FBottom;
  End;

  TSignalChart = Class(TPaintBox)
  Private
    FEquationBottomIn: TLinearEquations;

    FCoordinate: TBitmap;
    FGridR: TRectF;
    FData: TArray<Single>;
    FAxisesData: TAxises;
    FAxisesView: TAxises;
    FLeftTextR  : TRectF;
    FBottomTextR: TRectF;
  Private
    FBitmap: TBitmap;
  Private
    FFrameCounter: TFrameCount;
    function GetFPS: Integer;
    Procedure UpdateBitmap;
    Procedure UpdateGridRAndStdTextR();
    Procedure PaintData();
    procedure DoCheckSize;
    Procedure FillDesigningTestData;
  Protected
    Procedure DoPaint; Override;
    Procedure Loaded; Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure DrawData(const AData: TArray<Single>);
    Property FPS: Integer Read GetFPS;
  Published
    Property AxisesData: TAxises Read FAxisesData Write FAxisesData;
    Property AxisesView: TAxises Read FAxisesView Write FAxisesView;
  End;

  TTest = Class(TComponent)
  Private
    FSaveStr: String;
    Procedure ReadSave(Reader: TReader);
    Procedure WriteSave(Writer: TWriter);
  Protected
    Procedure DefineProperties(Filer: TFiler); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
  End;
procedure Register;

implementation

uses
  System.Diagnostics;//, CnDebug;

{ TSpectrumChart }

procedure Register;
begin
  RegisterComponents('RadioReceiver', [TSignalChart, TTest]);
end;

procedure TSignalChart.DoPaint;
begin
  inherited;
  FFrameCounter.AddFrame;
  DoCheckSize();
  Canvas.DrawBitmap(FBitmap, FBitmap.BoundsF, FBitmap.BoundsF, 1);
  PaintData();
//  CnDebugger.LogMsg('DoPaint');
end;

function TSignalChart.GetFPS: Integer;
begin
  Result := FFrameCounter.FPS;
end;

procedure TSignalChart.Loaded;
begin
  inherited;

end;

procedure TSignalChart.DoCheckSize;
var
  w, h: Integer;
  Changed: Boolean;
begin
  Changed := False;
  w := Ceil(Width);
  h := Ceil(Height);

  if FBitmap.Width <> w then
  begin
    FBitmap.Width := w;
    Changed := True;
  end;
  if FBitmap.Height <> h then
  begin
    FBitmap.Height := h;
    Changed := True;
  end;

  if Changed and (ComponentState * [csLoading, csReading] = []) then
  begin
    UpdateGridRAndStdTextR();
    UpdateBitmap;
  end;
end;

procedure TSignalChart.PaintData;
var
  HStep, VStep: Single;
  R: TRectF;
  i: Integer;
  ACanvas: TCanvas;
begin
  if ComponentState * [csLoading, csReading] <> [] then
    Exit;
  HStep := FGridR.Width / (Length(FData) - 1);
  VStep := FGridR.Height / (FAxisesData.Left.MaxValue - FAxisesData.Left.MinValue + 1);

  ACanvas := Canvas;
  ACanvas.Stroke.Color := TAlphaColors.Black;
  ACanvas.Stroke.Kind := TBrushKind.Solid;
  ACanvas.Stroke.Thickness := 1;
  ACanvas.Fill.Color := TAlphaColors.Lime;
  ACanvas.Fill.Kind := TBrushKind.Solid;

  for i := 0 to Length(FData) - 1 do
  begin
    R := TRectF.Create(0, FGridR.Height - VStep * FData[i], HStep,
      FGridR.Height - 0);

    R.Offset((i - 0.5) * HStep, 0);

    R.Offset(FGridR.Left, FGridR.Top);
    if R.Left < FGridR.Left then
      R.Left := FGridR.Left;
    if R.Right > FGridR.Right then
      R.Right := FGridR.Right;

    ACanvas.DrawRect(R, 0, 0, [], 1);
    R.Inflate(-0.5, -0.5);
    ACanvas.FillRect(R, 0, 0, [], 1);

  end;
end;


{ TGridLayer }

constructor TSignalChart.Create;
begin
  inherited;
  FAxisesData:= TAxises.Create(Self);
  FAxisesView:= TAxises.Create(Self);
  FFrameCounter := TFrameCount.Create;
  FEquationBottomIn := TLinearEquations.Create;
  FCoordinate := TBitmap.Create;

  FBitmap := TBitmap.Create;
  FillDesigningTestData();
end;


destructor TSignalChart.Destroy;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FCoordinate);
  FreeAndNil(FEquationBottomIn);
  FreeAndNil(FFrameCounter);
  FreeAndNil(FAxisesView);
  FreeAndNil(FAxisesData);
  inherited;
end;

procedure TSignalChart.DrawData(const AData: TArray<Single>);
begin
  if Length(AData) <> Length(FData) then
  begin
    FEquationBottomIn.CalcuCoff(0, 0, Length(AData), FGridR.Width - 1);
  end;
  FData := AData;
  InvalidateRect(ClipRect);
end;

procedure TSignalChart.FillDesigningTestData;
var
  i: Integer;
begin
  if csDesigning in ComponentState then
  begin
    SetLength(FData, FAxisesData.Bottom.MaxValue - FAxisesData.Bottom.MinValue + 1);
    for i := 0 to Length(FData) - 1 do
    begin
      FData[i]:= Random(FAxisesData.Left.FMaxValue - FAxisesData.Left.FMinValue + 2);
    end;
  end;
end;

procedure TSignalChart.UpdateBitmap;
  Procedure DrawFrame(ACanvas: TCanvas);
  const
    CONST_FRAME_COLOR: TAlphaColor = TAlphaColors.Black;
    CONST_FRAME_THICKNESS: Single = 2;
  begin
    ACanvas.Stroke.Dash := TStrokeDash.Dash;
    ACanvas.Stroke.Thickness := CONST_FRAME_THICKNESS;
    ACanvas.Stroke.Color := CONST_FRAME_COLOR;
    ACanvas.DrawRect(LocalRect, 0, 0, [], 100);
  end;

  Procedure DrawGridFrame(ACanvas: TCanvas);
  var
    GridFrameR: TRectF;
  begin
    // 画Grid的外边框
    GridFrameR := FGridR;
    GridFrameR.Inflate(1, 1);
    // ACanvas.Stroke.Color := TAlphaColors.Blue;
    ACanvas.Stroke.Thickness := 2;
    ACanvas.Stroke.Dash := TStrokeDash.Solid;
    ACanvas.DrawRect(GridFrameR, 0, 0, [], 1);
  end;

var
  ACanvas: TCanvas;
begin
  inherited;
//  CnDebugger.LogMsg('UpdateBitmap');
  UpdateGridRAndStdTextR();

  FCoordinate.SetSize(TSize.Create(Ceil(FGridR.Width), Ceil(FGridR.Height)));

  FEquationBottomIn.CalcuCoff(0, 0, Length(FData), FGridR.Width - 1);

  // 画坐标格
  if FCoordinate.HandleAllocated then
  begin
//    CnDebugger.LogMsg('画坐标格');
    ACanvas := FCoordinate.Canvas;

    ACanvas.BeginScene();
    try
      FCoordinate.Clear(TAlphaColors.Null);
      With FAxisesData.Left do
        DrawGrid(FCoordinate, CalcuMaxLabel(FBitmap, FLeftTextR));
      With FAxisesData.Bottom do
        DrawGrid(FCoordinate, CalcuMaxLabel(FBitmap, FBottomTextR));
    finally
      ACanvas.EndScene();
    end;
  end;
//  CnDebugger.LogMsg('UpdateBitmap');
  if FBitmap.HandleAllocated then
  begin
    // 画底图，主要是坐标轴的Label
    ACanvas := FBitmap.Canvas;
    ACanvas.BeginScene();
    try
      ACanvas.Clear(TAlphaColors.Null);
      With FAxisesData.Left do
        DrawLables(FGridR.TopLeft, ACanvas, FLeftTextR);
      With FAxisesData.Bottom do
        DrawLables(FGridR.TopLeft, ACanvas, FBottomTextR);
      DrawGridFrame(ACanvas);
      DrawFrame(ACanvas);
      ACanvas.DrawBitmap(FCoordinate, FCoordinate.BoundsF, FGridR, 1, True);
    finally
      ACanvas.EndScene;
    end;
  end;

  // DrawData(FData);
end;

// procedure TSignalChart.UpdateGridRAndStdTextR;
// var
// i: Integer;
// L_LableMaxW: Single;
// L_LableMaxH: Single;
// L_LableW: Single;
// begin
// L_LableMaxW := 0;
// L_LableMaxH := Canvas.TextHeight(IntToStr(FLeft.MinValue));
//
// for i := FLeft.MinValue to FLeft.MaxValue do
// begin
// L_LableW := Canvas.TextWidth(IntToStr(i) + FLeft.LableSuffix);
// if L_LableMaxW < L_LableW then
// L_LableMaxW := L_LableW;
// end;
//
// FStdTextR := TRectF.Create(0, 0, L_LableMaxW, L_LableMaxH);
// FStdTextR.Inflate(0, 0);
//
// FGridR := LocalRect;
// FGridR.Top := FGridR.Top + FStdTextR.Height / 2;
// FGridR.Left := FGridR.Left + FStdTextR.Width;
// FGridR.Bottom := FGridR.Bottom - FStdTextR.Height * 1.5;
// FGridR.Right := FGridR.Right - FStdTextR.Width;
// end;

procedure TSignalChart.UpdateGridRAndStdTextR;
var
  i: Integer;
  ARect: TRectF;
  MaxRect: TRectF;
begin

  With FAxisesData.Left do
  begin
    MaxRect:= TRectF.Empty;
    for i := MinValue to MaxValue do
    begin
      ARect:= LocalRect;
  //    CnDebugger.LogMsg(IntToStr(i) + FLeft.LableSuffix);
      Canvas.MeasureText(ARect, IntToStr(i) + LableSuffix, False, [],
        TTextAlign.Leading, TTextAlign.Leading);
      MaxRect.Union(ARect);
    end;
    FLeftTextR:= MaxRect;
  end;


  With FAxisesData.Bottom do
  begin
    MaxRect := TRectF.Empty;
    for i := MinValue to MaxValue do
    begin
      ARect:= LocalRect;
  //    CnDebugger.LogMsg(IntToStr(i) + FBottom.LableSuffix);
      Canvas.MeasureText(ARect, IntToStr(i) + LableSuffix, False, [],
        TTextAlign.Leading, TTextAlign.Leading);
      MaxRect.Union(ARect);
    end;
    FBottomTextR:= MaxRect;
  end;



//  FStdTextR.Inflate(2, 0);
//  CnDebugger.LogMsg(Format('W=%f; H=%f', [MaxRect.Width, MaxRect.Height]));
  FGridR := LocalRect;
  FGridR.Top := FGridR.Top + MaxRect.Height / 2;
  FGridR.Left := FGridR.Left + MaxRect.Width;
  FGridR.Bottom := FGridR.Bottom - MaxRect.Height * 1.5;
  FGridR.Right := FGridR.Right - FBottomTextR.Width / 2;
end;


{ TGridAndAxis }

class function TCustomAxis.CalcuStep(Range: Integer; N: Integer): Integer;
  function StepAlpha(A1, A2: Single): Single;
  var
    s: String;
    sa: TArray<String>;
    A: Integer;
    B: Single;
  begin
    // 得到科学计算法的系数, 以0.5为步进进行靠拢

    s := FloatToStrF(A1 / A2, ffExponent, 3, 0);
    sa := s.Split(['E'], 2);
    // sa[0]:= '1.49';
    B := sa[0].ToSingle();
    A := Trunc(B);
    B := frac(B);
    if B < 0.5 then
    begin
      B := 0.5
    end
    else
    begin
      A := A + 1;
      B := 0
    end;
    B := A + B;
    sa[0] := B.ToString;
    s := s.Join('E', sa);
    Result := s.ToSingle();
    // Result:= Ceil(A1 / A2 / 5) * 5;
  end;

begin
{$DEFINE x}
{$IFDEF x}
  Result := Ceil(Range / N / 5) * 5;
{$ELSE}
  Result := Trunc(StepAlpha(FAxisLeft.MaxValue - FAxisLeft.MinValue,
    L_LableCountActual - 1));
{$ENDIF}
end;

Class function TCustomAxis.CalcuValueStep(Limits: Single;
  MaxLableCount: Integer): Integer;
{$IFDEF x}
{$ELSE}
  function test_Step(A1, A2: Single): Single;
  var
    s: String;
    sa: TArray<String>;
    A: Integer;
    B: Single;
  begin
    // 得到科学计算法的系数, 以0.5为步进进行靠拢

    s := FloatToStrF(A1 / A2, ffExponent, 3, 0);
    sa := s.Split(['E'], 2);
    // sa[0]:= '1.49';
    B := sa[0].ToSingle();
    A := Trunc(B);
    B := frac(B);
    if B < 0.5 then
    begin
      B := 0.5
    end
    else
    begin
      A := A + 1;
      B := 0
    end;
    B := A + B;
    sa[0] := B.ToString;
    s := s.Join('E', sa);
    Result := s.ToSingle();
    // Result:= Ceil(A1 / A2 / 5) * 5;
  end;
{$ENDIF}

begin
{$DEFINE x}
{$IFDEF x}
  Result := Ceil(Limits / (MaxLableCount - 2) / 5) * 5;
{$ELSE}
  Result := Trunc(test_Step(FInfo.MaxValue - FInfo.MinValue,
    MaxLableCount - 2));
{$ENDIF}
end;

constructor TCustomAxis.Create(AChart: TSignalChart);
begin
  inherited Create;
  FChart:= AChart;
  FMaxValue := 0;
  FMinValue := -140;
  FThinkness := 1;
  FLineColor := TAlphaColors.Darkslategray;
end;

procedure TCustomAxis.DoChanged;
begin
  if (FChart <> Nil) and ([csReading, csLoading] * FChart.ComponentState = []) then
  begin
    FChart.UpdateBitmap;
    FChart.InvalidateRect(FChart.LocalRect);
  end;

  if csDesigning in FChart.ComponentState then
  begin
    FChart.FillDesigningTestData();
  end;
end;

procedure TCustomAxis.DrawGrid(Coordinate: TBitmap; MaxLableCount: Integer);
var
  i: Integer;
  SaveState: TCanvasSaveState;

begin
  if MaxLableCount < 2 then
    Exit; { TODO: 不符合此条件的以后怎么处理? }

  UpdateLinesPos(FLines, FMaxValue - FMinValue, Coordinate.Width,
    Coordinate.Height, MaxLableCount, IsVertical);

  SaveState := Coordinate.Canvas.SaveState;
  try
    Coordinate.Canvas.Stroke.Color := FLineColor;
    Coordinate.Canvas.Stroke.Thickness := FThinkness;
    for i := 1 to Length(FLines) - 1 do
    begin
      Coordinate.Canvas.DrawLine(FLines[i].StartPoint, FLines[i].EndPoint, 1);
    end;
  finally
    Coordinate.Canvas.RestoreState(SaveState);
  end;
end;

procedure TCustomAxis.DrawLables(const CoordinatePos: TPointF; ACanvas: TCanvas;
  StdTextR: TRectF);
var
  i: Integer;
  LableStep: Single;
  R: TRectF;
  AOffset: TPointF;
begin
  With ACanvas do
  begin
    Font.Family := '宋体';
    Fill.Color := TAlphaColors.Red;

    LableStep := (FMaxValue - FMinValue) / (Length(FLines) - 1);
    for i := 0 to Length(FLines) - 1 do
    begin
      AOffset:= TPointF.Create(CoordinatePos.X, CoordinatePos.Y);
      R:= CalcuLabelR(FLines[i], StdTextR, AOffset);
      FillText(R, CalcuLableText(i, LableStep),
        False, 1, [], HTextAlign, TTextAlign.Center);
//      DrawRect(R, 0, 0, [], 1);
    end;
  end;
end;



procedure TCustomAxis.SetLineColor(const Value: TAlphaColor);
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    DoChanged();
  end;
end;

procedure TCustomAxis.SetMaxValue(const Value: Integer);
begin
  if FMaxValue <> Value then
  begin
    FMaxValue := Value;
    DoChanged();
  end;
end;

procedure TCustomAxis.SetMinValue(const Value: Integer);
begin
  if FMinValue <> Value then
  begin
    FMinValue := Value;
    DoChanged();
  end;
end;

procedure TCustomAxis.SetThinkness(const Value: Single);
begin
  if FThinkness <> Value then
  begin
    FThinkness := Value;
    DoChanged();
  end;
end;

Class procedure TCustomAxis.UpdateLinesPos(var Lines: TArray<TLine>;
  MarkValueLimits, WidthLimits, HeightLimits: Integer; MaxLineCount: Integer;
  isVeritcal: Boolean);
var
  i: Integer;
  Step: Integer;
  L_InternalLineCount: Integer;
  Spacing: Single;
begin
  if MaxLineCount = 2 then
    Step := MarkValueLimits
  else
    Step := CalcuValueStep(MarkValueLimits, MaxLineCount);

  L_InternalLineCount := Trunc((MarkValueLimits) / Step) - 1;

  if isVeritcal then
    Spacing := HeightLimits / (L_InternalLineCount + 1)
  else
    Spacing := WidthLimits / (L_InternalLineCount + 1);

  // 更新线坐标并画线
  SetLength(Lines, L_InternalLineCount + 2);

  if isVeritcal then
  begin
    for i := 0 to L_InternalLineCount + 1 do
    begin
      Lines[i] := TLine.Create(TPointF.Create(0, i * Spacing),
        TPointF.Create(WidthLimits, i * Spacing));
    end;
  end
  else
  begin
    for i := 0 to L_InternalLineCount + 1 do
    begin
      Lines[i] := TLine.Create(TPointF.Create(i * Spacing, 0),
        TPointF.Create(i * Spacing, HeightLimits));
    end;
  end;
end;

{ TLine }

constructor TLine.Create(const A, B: TPointF);
begin
  StartPoint := A;
  EndPoint := B;
end;

{ TLeftAxis }

function TLeftAxis.CalcuLabelR(const Line: TLine;
  const StdTextRect: TRectF;  const offset: TPointF): TRectF;
var
  Posi: TPointF;
begin
  Result := StdTextRect;
  Posi := Line.StartPoint;
  Posi.X:= Posi.X + offset.X - StdTextRect.Width - 4;
  Result.SetLocation(Posi);
end;

function TLeftAxis.CalcuLableText(Index: Integer; LableStep: Single): String;
begin
  Result := IntToStr(FMaxValue - Round(LableStep * Index)) + LableSuffix;
end;

function TLeftAxis.CalcuMaxLabel(PanelBmp: TBitmap; StdTextR: TRectF): Integer;
begin
  Result := Trunc(PanelBmp.Bounds.Height / StdTextR.Height);
end;

constructor TLeftAxis.Create(AChart: TSignalChart);
begin
  inherited;
  FMaxValue := 0;
  FMinValue := -140;
end;

function TLeftAxis.GetHTextAlign: TTextAlign;
begin
  Result := TTextAlign.Trailing;
end;

function TLeftAxis.IsVertical: Boolean;
begin
  Result := True;
end;

function TLeftAxis.LableSuffix: String;
begin
  Result := 'dB';
end;

{ TBottomAxis }

function TBottomAxis.CalcuLabelR(const Line: TLine;
  const StdTextRect: TRectF; const offset: TPointF): TRectF;
var
  Posi: TPointF;
begin
  Result := StdTextRect;
  Posi := Line.EndPoint;
  Posi.Offset(StdTextRect.Width / 2, 2 + Ceil(StdTextRect.Height / 2));
  Result.SetLocation(Posi);
end;

function TBottomAxis.CalcuLableText(Index: Integer; LabelStep: Single): String;
begin
  Result := IntToStr(FMinValue + Round(LabelStep * Index)) + LableSuffix;
end;

function TBottomAxis.CalcuMaxLabel(PanelBmp: TBitmap; StdTextR: TRectF)
  : Integer;
begin
  Result := Trunc(PanelBmp.Bounds.Width / StdTextR.Width);
end;

constructor TBottomAxis.Create(AChart: TSignalChart);
begin
  inherited;
  FMaxValue := 1023;
  FMinValue := 0;
end;

function TBottomAxis.GetHTextAlign: TTextAlign;
begin
  Result := TTextAlign.Center;
end;

function TBottomAxis.IsVertical: Boolean;
begin
  Result := False;
end;

function TBottomAxis.LableSuffix: String;
begin
  Result := 'MHz'
end;

{ TLinearEquations }

procedure TLinearEquations.CalcuCoff(X1, Y1, X2, Y2: Single);
begin
  if X2 <> X1 then
  begin
    FConst1st := (Y2 - Y1) / (X2 - X1);
    FConst0 := Y1 - (FConst1st * X1);
  end;
end;

function TLinearEquations.CalcuY(X: Single): Single;
begin
  Result := FConst1st * X + FConst0
end;

{ TFrameCount }

procedure TFrameCount.AddFrame;
var
  nLen: Integer;
  MoreThanOneSecond: Boolean;
begin
  nLen := Length(FStamps);
  Insert(Now(), FStamps, nLen);

  MoreThanOneSecond := True;
  while (Length(FStamps) >= 2) and MoreThanOneSecond do
  begin
    MoreThanOneSecond := Not WithinPastMilliSeconds
      (FStamps[Length(FStamps) - 1], FStamps[0], 1000);
    if MoreThanOneSecond then
      Delete(FStamps, 0, 1)
  end;
end;

function TFrameCount.GetFPS: Integer;
begin
  Result := Length(FStamps);
end;

{ TAxises }

constructor TAxises.Create(AChart: TSignalChart);
begin
  inherited Create;
  FLeft:= TLeftAxis.Create(AChart);
  FBottom:= TBottomAxis.Create(AChart);
end;

destructor TAxises.Destroy;
begin
  FreeAndNil(FLeft);
  FreeAndNil(FBottom);
  inherited;
end;

{ TTest }

constructor TTest.Create(AOwner: TComponent);
begin
  inherited;
  FSaveStr:= TGUID.NewGuid.ToString
end;

procedure TTest.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('save', ReadSave, WriteSave, True);
end;

procedure TTest.ReadSave(Reader: TReader);
begin
  FSaveStr:= Reader.ReadString;
end;

procedure TTest.WriteSave(Writer: TWriter);
begin
  Writer.WriteString(FSaveStr);
end;

initialization

GlobalUseGPUCanvas := True;

end.
