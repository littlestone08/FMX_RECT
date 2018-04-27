unit uRadioSpectrumChart;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.UITypes,
  System.Math, System.DateUtils, System.IOUtils,
  FMX.Objects, FMX.Graphics, FMX.Types, FMX.Controls, FMX.STdCtrls;

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
    [weak]
    FChart: TSignalChart;
    FLines: TArray<TLine>;
    FMinValue: Integer;
    FThinkness: Single;
    FMaxValue: Integer;
    FLineColor: TAlphaColor;
    FLabelSuffix: String;
    procedure SetMaxValue(const Value: Integer);
    procedure SetMinValue(const Value: Integer);
    procedure SetThinkness(const Value: Single);
    procedure SetLineColor(const Value: TAlphaColor);
    Procedure DoChanged;
    procedure SetLabelSuffix(const Value: String);
  Protected
    function CalcuLabelR(const Line: TLine; const StdTextRect: TRectF;
      const offset: TPointF): TRectF; Virtual; Abstract;
    function CalcuMaxLabel(const GraphicR: TRectF; StdTextR: TRectF): Integer;
      Virtual; Abstract;
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
    Procedure DrawLables(const GridLocation: TPointF; ACanvas: TCanvas;
      StdTextR: TRectF);

  Published
    Property MinValue: Integer read FMinValue write SetMinValue;
    Property MaxValue: Integer read FMaxValue write SetMaxValue;
    Property Thinkness: Single read FThinkness write SetThinkness;
    Property LineColor: TAlphaColor read FLineColor write SetLineColor;
    Property HTextAlign: TTextAlign Read GetHTextAlign;
    Property LabelSuffix: String Read FLabelSuffix Write SetLabelSuffix;
  End;

  TLeftAxis = Class(TCustomAxis)
  Protected
    function CalcuLabelR(const Line: TLine; const StdTextRect: TRectF;
      const offset: TPointF): TRectF; override;
    Function IsVertical: Boolean; Override;
    function CalcuMaxLabel(const GraphicR: TRectF; StdTextR: TRectF)
      : Integer; Override;
    function CalcuLableText(Index: Integer; LableStep: Single): String;
      Override;
    function GetHTextAlign: TTextAlign; Override;
  Public
    Constructor Create(AChart: TSignalChart); Override;
  end;

  TBottomAxis = Class(TCustomAxis)
  Protected
    function CalcuLabelR(const Line: TLine; const StdTextRect: TRectF;
      const offset: TPointF): TRectF; override;
    Function IsVertical: Boolean; Override;
    function CalcuMaxLabel(const GraphicR: TRectF; StdTextR: TRectF)
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

  TSignalDrawer = Class(TComponent)
  Private
    [weak]
    FChart: TSignalChart;
  private
  Protected
    procedure SetChart(const Value: TSignalChart); Virtual;
    Procedure DrawCross(X, Y: Single); Virtual; Abstract;
    Procedure DoSizeChagned(); Virtual; Abstract;
  Public
    Procedure DoDraw; Virtual; Abstract;
  Published
    Property Chart: TSignalChart Read FChart Write SetChart;
  End;

  TSignalChart = Class(TPaintBox)
  Private
    FEquationBottomIn: TLinearEquations;

    FGrid: TBitmap;

    FGraphicRect: TRectF;
    FGraphicGridR: TRectF;
    FGraphicUpdated: Boolean;

    FWaterFallRect: TRectF;
    FWaterFallGridR: TRectF;
    FWaterFallRectUpdated: Boolean;

    FData: TArray<Single>;
    FAxisesData: TAxises;
    FAxisesView: TAxises;
    FLeftTextR: TRectF;
    FBottomTextR: TRectF;
  Private
    FBKGraphic: TBitmap;
    FHint: String;
  Private
    FFrameCounter: TFrameCount;
    FDrawer: TSignalDrawer;
    FBKColor: TAlphaColor;
    FGridBoundColor: TAlphaColor;
    function GetFPS: Integer;
    Procedure UpdateBitmap;
    Procedure UpdateGridRAndStdTextR();
    procedure DoCheckSize;
    Procedure DoRectSizeChagned;
    Procedure FillDesigningTestData;
    procedure SetDrawer(const Value: TSignalDrawer);
    procedure SetBKColor(const Value: TAlphaColor);
    procedure SetGridBoundColor(const Value: TAlphaColor);
  Private
    FCrossOpacity: Single;
    FCrossX: Single;
    FCrossY: Single;
    FMouseInRect: Boolean;
    FShowCross: Boolean;
    FLastUpdateTime: TDateTime;
    // Procedure DrawCross();
  Protected
    Procedure DoPaint; Override;
    Procedure Loaded; Override;
    Procedure DoResized; Override;
  Protected
    Procedure MouseMove(Shift: TShiftState; X, Y: Single); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure DrawData(const AData: TArray<Single>);
    Property FPS: Integer Read GetFPS;
  Public
    function GridToClient(const APoint: TRectF): TRectF; inline;
    function ClientToGrid(const APoint: TRectF): TRectF; inline;
    function GridToGraphic(const APoint: TRectF): TRectF; inline;
    function GraphicToGrid(const APoint: TRectF): TRectF; inline;
    function ClientToGraphic(const APoint: TRectF): TRectF; inline;
    function GraphicToClient(const APoint: TRectF): TRectF; inline;
  Published
    Property AxisesData: TAxises Read FAxisesData Write FAxisesData;
    Property AxisesView: TAxises Read FAxisesView Write FAxisesView;
    Property Drawer: TSignalDrawer read FDrawer write SetDrawer;
    Property BKColor: TAlphaColor read FBKColor write SetBKColor;
    Property GridBoundColor: TAlphaColor read FGridBoundColor
      write SetGridBoundColor;
    Property CrossOpacity: Single Read FCrossOpacity Write FCrossOpacity;
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

  TSignalRectangeDrawer = Class(TSignalDrawer)
  Private
    FPeaks: TArray<Single>;
    FFallOff: TArray<Single>;
  private
    FFalloffDecrement: Single;
    FPeakDecrement: Single;
    FPeakVisible: Boolean;
    FFalloffVisible: Boolean;
    procedure SetFalloffDecrement(const Value: Single);
    procedure SetPeakeDecrement(const Value: Single);
    procedure SetFalloffVisible(const Value: Boolean);
    procedure SetPeakVisible(const Value: Boolean);
  Protected
    Procedure DrawCross(X, Y: Single); Override;
    Procedure DoSizeChagned(); Override;
    Procedure DrawHint; Virtual;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Procedure DoDraw; Override;
  Published
    Property PeakDecrement: Single read FPeakDecrement write SetPeakeDecrement;
    Property FalloffDecrement: Single read FFalloffDecrement
      write SetFalloffDecrement;
    Property FalloffVisible: Boolean read FFalloffVisible
      write SetFalloffVisible;
    Property PeakVisible: Boolean read FPeakVisible write SetPeakVisible;
  End;

  TSplitedDrawer = Class(TSignalRectangeDrawer)
  Private
    FColors: TArray<TAlphaColor>;
    FWaterFallBmp: TBitmap;
    FColorImage: TImage;
  Protected
    Procedure DrawCross(X, Y: Single); Override;
    Procedure DoSizeChagned(); Override;
    procedure SetChart(const Value: TSignalChart); Override;
    Procedure DrawHint; Override;
  Public
    Procedure DoDraw; Override;
    constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
  End;


procedure Register;

implementation

uses
  System.Diagnostics, SpectraLibrary, FMX.PlatForm, CnDebug;

{ TSpectrumChart }

procedure Register;
begin
  RegisterComponents('RadioReceiver', [TSignalChart, TSignalRectangeDrawer,
    TTest, TSplitedDrawer]);
end;

procedure TSignalChart.DoPaint;
begin
  inherited;

  FFrameCounter.AddFrame;
  DoCheckSize();
  Canvas.DrawBitmap(FBKGraphic, FBKGraphic.BoundsF, FBKGraphic.BoundsF, 1);
  if FDrawer <> Nil then
    FDrawer.DoDraw();
  FLastUpdateTime := Now;
end;

procedure TSignalChart.DoResized;
begin
  inherited;
  TThread.Synchronize(Nil,
    procedure()
    begin
      Sleep(10);
      DoPaint();
    end
  );
end;

procedure TSignalChart.DoRectSizeChagned;
begin
  if FDrawer <> Nil then
  begin
    FDrawer.DoSizeChagned();
  end;
end;

function TSignalChart.GetFPS: Integer;
begin
  Result := FFrameCounter.FPS;
end;

function TSignalChart.GraphicToClient(const APoint: TRectF): TRectF;
begin
  Result := APoint;
  Result.offset(-FGraphicRect.Left, -FGraphicRect.Top);
end;

function TSignalChart.GraphicToGrid(const APoint: TRectF): TRectF;
begin
  Result := APoint;
  Result.offset(FGraphicGridR.TopLeft);
end;

function TSignalChart.GridToClient(const APoint: TRectF): TRectF;
begin
  Result := APoint;
  Result.offset(FGraphicGridR.TopLeft);
end;

function TSignalChart.GridToGraphic(const APoint: TRectF): TRectF;
begin
  Result := APoint;
  Result.offset(-FGraphicRect.Left, -FGraphicRect.Top);
end;

procedure TSignalChart.Loaded;
begin
  inherited;

end;

procedure TSignalChart.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  FCrossX := X;
  FCrossY := Y;
  FMouseInRect := PtInRect(FGraphicGridR, TPointF.Create(X, Y));
  begin
    if MilliSecondSpan(Now, FLastUpdateTime) > 30 then
    begin
      InvalidateRect(LocalRect);
    end;
  end;
  // Exit;
  // if MilliSecondSpan(Now, FLastUpdateTime) > 30 then
  // begin
  //
  // // TPaintBox(self).SetFocus
  // InvalidateRect(LocalRect);
  /// /    Exit;
  // // 通过坐标计算是不是在Grid内，再换算成Grid内的轴物理坐标及轴的标称坐标
  // // 如果在Grid内，则画十字光标
  // MouseP := TPointF.Create(X, Y);
  // if PtInRect(FGraphicGridR, MouseP) then
  // begin
  // OffsetP := MouseP;
  // OffsetP.offset(-FGraphicGridR.Left, -FGraphicGridR.Top);
  //
  //
  // // if FTextService.HasMarkedText and TPlatformServices.Current.SupportsPlatformService(IFMXMouseService, MouseService) then
  // // try
  // // MousePos := ScreenToLocal(MouseService.GetMousePos);
  // // ContentRect := Model.ContentBounds;
  // // MousePos.X := EnsureRange(MousePos.X, ContentRect.Left, ContentRect.Right);
  // // MousePos.Y := EnsureRange(MousePos.Y, ContentRect.Top, ContentRect.Bottom);
  // // MousePos.Offset(-ContentRect.TopLeft);
  // // LCaretPosition := FLineObjects.GetPointPosition(MousePos, False);
  // // FTextService.CaretPosition := TPoint.Create(LCaretPosition.Pos, LCaretPosition.Line);
  // // IMEStateUpdated;
  // // finally
  // // MouseService := nil;
  // // end;
  //
  /// /      TPlatformServices.Current.SupportsPlatformService(IFMXMouseService,
  /// /        MouseSrv);
  /// /      begin
  /// /        GlobalP := MouseSrv.GetMousePos;
  /// /      end;
  /// /      GMapC := ScreenToLocal(GlobalP);
  /// /      CnDebugger.LogFmt
  /// /        ('在范围内: %.2f, %.2f, GlobalPos: %.2f, %.2f, Map: %.2f, %.2f, Mouse: %.2f, %.2f',
  /// /        [OffsetP.X, OffsetP.Y, GlobalP.X, GlobalP.Y, GMapC.X, GMapC.Y, X, Y]);
  // end
  // else
  // begin
  /// /       CnDebugger.LogMsg('不在范围内');
  // end;
  // end;
end;

procedure TSignalChart.DoCheckSize;
  Procedure UpdateGraphicRect;
  begin
    FGraphicRect := LocalRect;
    FGraphicRect.Top := FGraphicRect.Top + 2;
    FGraphicRect.Bottom := FGraphicRect.Bottom / 2;
    FWaterFallRect := LocalRect;
    FWaterFallRect.Top := FGraphicRect.Bottom;
    FWaterFallRect.Bottom := LocalRect.Bottom - 2
  end;

var
  Changed: Boolean;
begin
  Changed := False;

  UpdateGraphicRect;

  if FBKGraphic.Width <> Ceil(LocalRect.Width) then
  begin
    FBKGraphic.Width := Ceil(LocalRect.Width);
    Changed := True;
  end;
  if FBKGraphic.Height <> Ceil(LocalRect.Height) then
  begin
    FBKGraphic.Height := Ceil(LocalRect.Height);
    Changed := True;
  end;

  if Changed and (ComponentState * [csLoading, csReading] = []) then
  begin
    UpdateGridRAndStdTextR();
    UpdateBitmap;
    DoRectSizeChagned();
  end;
end;

procedure TSignalChart.SetBKColor(const Value: TAlphaColor);
begin
  if FBKColor <> Value then
  begin
    FBKColor := Value;
    UpdateBitmap;
    InvalidateRect(ClipRect);
  end;
end;

procedure TSignalChart.SetGridBoundColor(const Value: TAlphaColor);
begin
  if FGridBoundColor <> Value then
  begin
    FGridBoundColor := Value;
    UpdateBitmap;
    InvalidateRect(ClipRect);
  end;
end;

procedure TSignalChart.SetDrawer(const Value: TSignalDrawer);
var
  LastDrawer: TSignalDrawer;
begin
  LastDrawer := FDrawer;
  if FDrawer <> Value then
  begin
    FDrawer := Value;
    if (LastDrawer <> Nil) then
    begin
      if LastDrawer.Chart <> Nil then
      begin
        LastDrawer.Chart := Nil;
      end;
    end;
  end;

  if Value <> Nil then
    if Value.Chart <> self then
      Value.Chart := self;
end;

{ TGridLayer }


function TSignalChart.ClientToGraphic(const APoint: TRectF): TRectF;
begin
  Result := APoint;
  Result.offset(FGraphicRect.TopLeft);
end;

function TSignalChart.ClientToGrid(const APoint: TRectF): TRectF;
begin
  Result := APoint;
  Result.offset(-FGraphicGridR.Left, -FGraphicGridR.Top);
end;

constructor TSignalChart.Create;
begin
  inherited;
  FGridBoundColor := TAlphaColors.Darkslategray;
  FAxisesData := TAxises.Create(self);
  FAxisesView := TAxises.Create(self);
  FFrameCounter := TFrameCount.Create;
  FEquationBottomIn := TLinearEquations.Create;
  FGrid := TBitmap.Create;

  FBKGraphic := TBitmap.Create;
  FillDesigningTestData();
  FCrossOpacity := 0.3;
  FShowCross := True;
end;

destructor TSignalChart.Destroy;
begin
  FreeAndNil(FBKGraphic);
  FreeAndNil(FGrid);
  FreeAndNil(FEquationBottomIn);
  FreeAndNil(FFrameCounter);
  FreeAndNil(FAxisesView);
  FreeAndNil(FAxisesData);
  inherited;
end;
//
// procedure TSignalChart.DrawCross;
// var
// MouseSrv: IFMXMouseService;
// MousePos: TPointF;
// MousePosInClient: TPointF;
//
// begin
// TPlatformServices.Current.SupportsPlatformService(IFMXMouseService, MouseSrv);
// begin
// MousePos := MouseSrv.GetMousePos;
// MousePosInClient := ScreenToLocal(MousePos);;
// end;
// FMouseInRect:= PtInRect(FGraphicGridR, MousePosInClient);
// if FMouseInRect then
// begin
// MousePosInClient := ScreenToLocal(MousePos);;
// Canvas.Stroke.Color := TAlphaColors.Black;
// Canvas.Stroke.Thickness := 2;
// Canvas.Stroke.Dash := TStrokeDash.Dash;
// Canvas.Stroke.Kind := TBrushKind.Solid;
//
// Canvas.DrawLine(TPointF.Create(FGraphicGridR.Left, MousePosInClient.Y),
// TPointF.Create(FGraphicGridR.Right, MousePosInClient.Y), CrossOpacity);
//
// Canvas.DrawLine(TPointF.Create(MousePosInClient.X, FGraphicGridR.Top),
// TPointF.Create(MousePosInClient.X, FGraphicGridR.Bottom), CrossOpacity);
//
// Canvas.DrawLine(TPointF.Create(MousePosInClient.X, FWaterFallGridR.Top),
// TPointF.Create(MousePosInClient.X, FWaterFallGridR.Bottom), CrossOpacity);
/// /    CnDebugger.LogMsg('Draw Line');
// end
// else
// begin
/// /    CnDebugger.LogMsg('Out of Range');
// end;
//
// // CnDebugger.LogFmt
// // ('在范围内: %.2f, %.2f, GlobalPos: %.2f, %.2f, Map: %.2f, %.2f, Mouse: %.2f, %.2f',
// // [OffsetP.X, OffsetP.Y, GlobalP.X, GlobalP.Y, GMapC.X, GMapC.Y, X, Y]);
// end;

procedure TSignalChart.DrawData(const AData: TArray<Single>);
begin
  if Length(AData) <> Length(FData) then
  begin
    FEquationBottomIn.CalcuCoff(0, 0, Length(AData), FGraphicGridR.Width - 1);
  end;
  FData := AData;
  InvalidateRect(ClipRect);
end;

procedure TSignalChart.FillDesigningTestData;
var
  i: Integer;
  nCount: Integer;
begin
  Exit;
  if csDesigning in ComponentState then
  begin
    nCount := FAxisesData.Bottom.MaxValue - FAxisesData.Bottom.MinValue + 1;
    if nCount > 512 then
      nCount := 512;
    if nCount < 0 then
      nCount := 10;

    SetLength(FData, nCount);
    for i := 0 to nCount - 1 do
    begin
      FData[i] := Random(FAxisesData.Left.FMaxValue -
        FAxisesData.Left.FMinValue + 2);
    end;
  end;
end;

procedure TSignalChart.UpdateBitmap;
  Procedure DrawControlBound(ACanvas: TCanvas);
  const
    CONST_FRAME_COLOR: TAlphaColor = TAlphaColors.Black;
    CONST_FRAME_THICKNESS: Single = 2;
  begin
    ACanvas.Stroke.Dash := TStrokeDash.Dash;
    ACanvas.Stroke.Thickness := CONST_FRAME_THICKNESS;
    ACanvas.Stroke.Color := CONST_FRAME_COLOR;
    ACanvas.DrawRect(LocalRect, 0, 0, [], 1);
  end;

  Procedure DrawGraphicGridBound(ACanvas: TCanvas);
  var
    GridFrameR: TRectF;
  begin
    // 画Grid的外边框
    GridFrameR := FGraphicGridR;
    GridFrameR.Inflate(1, 1);
    // ACanvas.Stroke.Color := TAlphaColors.Blue;
    ACanvas.Stroke.Thickness := 2;
    ACanvas.Stroke.Color := FGridBoundColor;
    ACanvas.Stroke.Dash := TStrokeDash.Solid;
    ACanvas.DrawRect(GridFrameR, 0, 0, [], 1);
  end;

  Procedure DrawGraphicBound(ACanvas: TCanvas);
  const
    CONST_FRAME_COLOR: TAlphaColor = TAlphaColors.Lime;
    CONST_FRAME_THICKNESS: Single = 2;
  begin
    ACanvas.Stroke.Dash := TStrokeDash.Dash;
    ACanvas.Stroke.Thickness := CONST_FRAME_THICKNESS;
    ACanvas.Stroke.Color := CONST_FRAME_COLOR;
    ACanvas.DrawRect(FGraphicRect, 0, 0, [], 1);
  end;

  Procedure DrawWaterFallBound(ACanvas: TCanvas);
  const
    CONST_FRAME_COLOR: TAlphaColor = TAlphaColors.Red;
    CONST_FRAME_THICKNESS: Single = 2;
  begin
    ACanvas.Stroke.Dash := TStrokeDash.Dash;
    ACanvas.Stroke.Thickness := CONST_FRAME_THICKNESS;
    ACanvas.Stroke.Color := CONST_FRAME_COLOR;
    ACanvas.DrawRect(FWaterFallRect, 0, 0, [], 1);
  end;

  Procedure DrawWaterfallGridBound(ACanvas: TCanvas);
  var
    GridFrameR: TRectF;
  begin
    // 画Grid的外边框
    GridFrameR := FWaterFallGridR;
    GridFrameR.Inflate(1, 1);
    // ACanvas.Stroke.Color := TAlphaColors.Blue;
    ACanvas.Stroke.Thickness := 2;
    ACanvas.Stroke.Color := FGridBoundColor;
    ACanvas.Stroke.Dash := TStrokeDash.Solid;
    ACanvas.DrawRect(FWaterFallGridR, 0, 0, [], 1);
  end;

var
  ACanvas: TCanvas;
  Clips: TClipRects;
begin
  inherited;
  // CnDebugger.LogMsg('UpdateBitmap');
  UpdateGridRAndStdTextR();

  FGrid.SetSize(TSize.Create(Ceil(FGraphicGridR.Width),
    Ceil(FGraphicGridR.Height)));

  FEquationBottomIn.CalcuCoff(0, 0, Length(FData), FGraphicGridR.Width - 1);

  // 画坐标格
  if FGrid.HandleAllocated then
  begin
    // CnDebugger.LogMsg('画坐标格');
    ACanvas := FGrid.Canvas;

    if ACanvas.BeginScene() then
      try
        FGrid.Canvas.Clear(FBKColor);
        With FAxisesData.Left do
          DrawGrid(FGrid, CalcuMaxLabel(FGraphicRect, FLeftTextR));
        With FAxisesData.Bottom do
          DrawGrid(FGrid, CalcuMaxLabel(FGraphicRect, FBottomTextR));
      finally
        ACanvas.EndScene();
      end;
  end;
  // CnDebugger.LogMsg('UpdateBitmap');
  if FBKGraphic.HandleAllocated then
  begin
    // 画底图，主要是坐标轴的Label
    ACanvas := FBKGraphic.Canvas;
    Insert(LocalRect, Clips, 0);
    if ACanvas.BeginScene(@Clips, 0) then
      try
        ACanvas.Clear(FBKColor);
        With FAxisesData.Left do
          DrawLables(FGraphicGridR.Location, ACanvas, FLeftTextR);
        With FAxisesData.Bottom do
          DrawLables(FGraphicGridR.Location, ACanvas, FBottomTextR);

        DrawGraphicGridBound(ACanvas);
        DrawWaterfallGridBound(ACanvas);

        // DrawGraphicBound(ACanvas);
        // DrawWaterFallBound(ACanvas);

        DrawControlBound(ACanvas);

        ACanvas.DrawBitmap(FGrid, FGrid.BoundsF, FGraphicGridR, 1, True);
      finally
        ACanvas.EndScene;
      end;
  end;
  // DrawData(FData);
end;

procedure TSignalChart.UpdateGridRAndStdTextR;
  function MeasureMaxRect(ValueFrom, ValueTo: Integer; Suffix: String): TRectF;
  var
    i: Integer;
    ARect: TRectF;
    Step: Integer;
  begin
    Step := 1;
    if Abs(ValueTo - ValueFrom) + 1 > 500 then
    begin
      Step := Ceil((ValueTo - ValueFrom + 1) / 50)
    end;
    Result := TRectF.Empty;

    i := ValueFrom;
    ARect := FGraphicRect;
    while i < ValueTo do
    begin
      Canvas.MeasureText(ARect, IntToStr(i) + Suffix, False, [],
        TTextAlign.Leading, TTextAlign.Leading);
      Result.Union(ARect);
      Inc(i, Step);
    end;

    ARect := LocalRect;
    Canvas.MeasureText(ARect, IntToStr(ValueTo) + Suffix, False, [],
      TTextAlign.Leading, TTextAlign.Leading);
    Result.Union(ARect);
  end;

begin
  With FAxisesData.Left do
    FLeftTextR := MeasureMaxRect(MinValue, MaxValue, LabelSuffix);

  With FAxisesData.Bottom do
    FBottomTextR := MeasureMaxRect(MinValue, MaxValue, LabelSuffix);

  FGraphicGridR := FGraphicRect;
  FGraphicGridR.Top := FGraphicGridR.Top + FLeftTextR.Height * 0.5;
  FGraphicGridR.Left := FGraphicGridR.Left + FBottomTextR.Width;
  FGraphicGridR.Bottom := FGraphicGridR.Bottom - FLeftTextR.Height * 0.5 -
    FBottomTextR.Height;
  FGraphicGridR.Right := FGraphicGridR.Right - FBottomTextR.Width * 0.5;
  FGraphicUpdated := True;

  FWaterFallGridR := FGraphicGridR;
  FWaterFallGridR.Top := FWaterFallRect.Top;
  FWaterFallGridR.Bottom := FWaterFallRect.Bottom - FBottomTextR.Bottom / 2;
  FWaterFallRectUpdated := True;
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
  FChart := AChart;
  FMaxValue := 0;
  FMinValue := -140;
  FThinkness := 1;
  FLineColor := TAlphaColors.Darkslategray;
end;

procedure TCustomAxis.DoChanged;
begin
  if FChart <> Nil then
  begin
    if csDesigning in FChart.ComponentState then
    begin
      FChart.FillDesigningTestData();
    end;

    if [csReading, csLoading] * FChart.ComponentState = [] then
    begin
      FChart.UpdateBitmap;
      FChart.InvalidateRect(FChart.LocalRect);
    end;
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

procedure TCustomAxis.DrawLables(const GridLocation: TPointF; ACanvas: TCanvas;
  StdTextR: TRectF);
var
  i: Integer;
  LableStep: Single;
  R: TRectF;
begin
  With ACanvas do
  begin
    Font.Family := '宋体';
    Fill.Color := TAlphaColors.Red;

    LableStep := (FMaxValue - FMinValue) / (Length(FLines) - 1);
    for i := 0 to Length(FLines) - 1 do
    begin
      R := CalcuLabelR(FLines[i], StdTextR, GridLocation);
      FillText(R, CalcuLableText(i, LableStep), False, 1, [], HTextAlign,
        TTextAlign.Center);
    end;
  end;
end;

procedure TCustomAxis.SetLabelSuffix(const Value: String);
begin
  if FLabelSuffix <> Value then
  begin
    FLabelSuffix := Value;
    DoChanged;
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

function TLeftAxis.CalcuLabelR(const Line: TLine; const StdTextRect: TRectF;
  const offset: TPointF): TRectF;
var
  Posi: TPointF;
begin
  Result := StdTextRect;
  Posi := Line.StartPoint;
  Posi.X := Posi.X + offset.X - StdTextRect.Width - 4;
  Result.SetLocation(Posi);
end;

function TLeftAxis.CalcuLableText(Index: Integer; LableStep: Single): String;
begin
  Result := IntToStr(FMaxValue - Round(LableStep * Index)) + LabelSuffix;
end;

function TLeftAxis.CalcuMaxLabel(const GraphicR: TRectF;
  StdTextR: TRectF): Integer;
begin
  Result := Trunc(GraphicR.Height / StdTextR.Height);
end;

constructor TLeftAxis.Create(AChart: TSignalChart);
begin
  inherited;
  FMaxValue := 0;
  FMinValue := -140;
  FLabelSuffix := 'dB'
end;

function TLeftAxis.GetHTextAlign: TTextAlign;
begin
  Result := TTextAlign.Trailing;
end;

function TLeftAxis.IsVertical: Boolean;
begin
  Result := True;
end;

{ TBottomAxis }

function TBottomAxis.CalcuLabelR(const Line: TLine; const StdTextRect: TRectF;
  const offset: TPointF): TRectF;
var
  Posi: TPointF;
begin
  Result := StdTextRect;
  Posi := Line.EndPoint;
  Posi.offset(StdTextRect.Width / 2, 2 + Ceil(StdTextRect.Height / 2));
  Result.SetLocation(Posi);
end;

function TBottomAxis.CalcuLableText(Index: Integer; LabelStep: Single): String;
begin
  Result := IntToStr(FMinValue + Round(LabelStep * Index)) + LabelSuffix;
end;

function TBottomAxis.CalcuMaxLabel(const GraphicR: TRectF;
  StdTextR: TRectF): Integer;
begin
  Result := Trunc(GraphicR.Width / StdTextR.Width);
end;

constructor TBottomAxis.Create(AChart: TSignalChart);
begin
  inherited;
  FMaxValue := 1023;
  FMinValue := 0;
  FLabelSuffix := 'MHz'
end;

function TBottomAxis.GetHTextAlign: TTextAlign;
begin
  Result := TTextAlign.Center;
end;

function TBottomAxis.IsVertical: Boolean;
begin
  Result := False;
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
  FLeft := TLeftAxis.Create(AChart);
  FBottom := TBottomAxis.Create(AChart);
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
  FSaveStr := TGUID.NewGuid.ToString
end;

procedure TTest.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('save', ReadSave, WriteSave, True);
end;

procedure TTest.ReadSave(Reader: TReader);
begin
  FSaveStr := Reader.ReadString;
end;

procedure TTest.WriteSave(Writer: TWriter);
begin
  Writer.WriteString(FSaveStr);
end;

{ TSignalChartDrawer }

{ TSignalRectangeDrawer }

constructor TSignalRectangeDrawer.Create(AOwner: TComponent);
begin
  inherited;
  FFalloffDecrement := 0.005;
  FPeakDecrement := 0.001;
  FPeakVisible := True;
  FFalloffVisible := True;

end;

procedure TSignalRectangeDrawer.DoDraw;
  function GetHint: String;
  var
    ptPosInGrid: TPointF;
    RatioX, RatioY: Single;
    DataIndex: Integer;
    LabelX, LabelY: String;
    PeakY: String;
  begin
    Result:= '';
    With Chart do
    begin
      ptPosInGrid:= TPointF.Create(FCrossX - FGraphicGridR.Left, FCrossY - FGraphicGridR.Top);
      RatioX:= ptPosInGrid.X / FGraphicGridR.Width;
      RatioY:= ptPosInGrid.Y / FGraphicGridR.Height;
      With Chart.FAxisesData.Bottom do
      begin
//      DataIndex:= Ceil(FGraphicGridR.Width * RatioX) * (FMaxValue - FMinValue + 1);
//        DataIndex:= Ceil((FMaxValue - FMinValue + 1+0.5) * RatioX) ;
//        DataIndex:= Ceil(Length(FData) * Min((RatioX + 0.005), 1));
        DataIndex:= Round(Length(FData) * Min(RatioX, 1));
      end;

      Result:= Format('GridR (%.2f, %.2f)'#$D#$A
                      +'Cross: (%.2f, %.2f)=>(%.2f, %.2f)'#$D#$A
                      +'Cross Percent: (%.2f%%, %.2f%%)'#$D#$A,
                      [FGraphicGridR.Left,FGraphicGridR.Top,
                      FCrossX, FCrossY,
                      ptPosInGrid.X, ptPosInGrid.Y,
                      RatioX * 100 , RatioY * 100
                      ]);
      if Length(FData) > 0 then
      begin
        Result:= Result + Format('Data[%d] = %.2f'#$D#$A,
                        [DataIndex, FPeaks[DataIndex]
                        ]);
        With Chart.FAxisesData.Bottom do
          LabelX:= Format('%.2f' + FLabelSuffix, [
                FMinValue + (FMaxValue - FMinValue + 1) * RatioX]);

        With Chart.FAxisesData.Left do
                LabelY:= Format('%.2f' + FLabelSuffix, [
                      FMaxValue - (FMaxValue - FMinValue + 1) * RatioY]);
        With Chart.FAxisesData.Left do
                PeakY:= Format('%.2f' + FLabelSuffix, [
                      FMinValue + (FMaxValue - FMinValue + 1) * FPeaks[DataIndex]]);

        Result:= Result + Format('Labled Value: (%s, %s)'#$D#$A, [LabelX, LabelY]);
        Result:= Result + Format('Peak Value: (%s, %s)', [LabelX, PeakY]);


      end;
    end;
  end;
var
  HStep: Single;
  FalloffR: TRectF;
  PeakR: TRectF;
  i: Integer;
  ACanvas: TCanvas;
  di: Single;
  nCount: Integer;
begin
  With Chart do
  begin
    if ComponentState * [csLoading, csReading] <> [] then
      Exit;

    nCount := Length(FData);
    if Length(FPeaks) < nCount then
      SetLength(FPeaks, nCount);
    if Length(FFallOff) < nCount then
      SetLength(FFallOff, nCount);
    for i := 0 to nCount - 1 do
    begin
      di := FData[i];
      if di >= FPeaks[i] then
      begin
        FPeaks[i] := di
      end
      else
      begin
        if FPeaks[i] > FPeakDecrement then
          FPeaks[i] := FPeaks[i] - FPeakDecrement
        else
          FPeaks[i] := 0;
      end;

      if di >= FFallOff[i] then
      begin
        FFallOff[i] := di
      end
      else
      begin
        if FFallOff[i] > FFalloffDecrement then
          FFallOff[i] := FFallOff[i] - FFalloffDecrement
        else
          FFallOff[i] := 0;
      end;
    end;
    HStep := FGraphicGridR.Width / (Length(FData) - 1);

    ACanvas := Canvas;
    ACanvas.Stroke.Color := TAlphaColors.Black;
    ACanvas.Stroke.Kind := TBrushKind.Solid;
    ACanvas.Stroke.Thickness := 1;
    ACanvas.Fill.Color := TAlphaColors.Lime;
    ACanvas.Fill.Kind := TBrushKind.Solid;

    for i := 0 to nCount - 1 do
    begin
      FalloffR := TRectF.Create(0, FGraphicGridR.Height * (1 - FFallOff[i]),
        HStep, FGraphicGridR.Height - 0);

      FalloffR.offset((i - 0.5) * HStep, 0);
      FalloffR := Chart.GridToClient(FalloffR);
      // FalloffR.offset(FGridR.Left, FGridR.Top);

      if FalloffR.Left < FGraphicGridR.Left then
        FalloffR.Left := FGraphicGridR.Left;
      if FalloffR.Right > FGraphicGridR.Right then
        FalloffR.Right := FGraphicGridR.Right;

      PeakR := FalloffR;
      if FalloffVisible then
      begin
        if (FalloffR.Width > 2) then
        begin
          ACanvas.DrawRect(FalloffR, 0, 0, [], 1);
          FalloffR.Inflate(-0.5, -0.5);
        end;
        ACanvas.FillRect(FalloffR, 0, 0, [], 1);
      end;
      if PeakVisible then
      begin
        PeakR.Bottom := FGraphicGridR.Height * (1 - FPeaks[i]) +
          FGraphicGridR.Top;
        PeakR.Top := PeakR.Bottom - 1;
        ACanvas.FillRect(PeakR, 0, 0, [], 1);
      end;
    end;
  end;
  if Chart.FMouseInRect then
  begin
    Chart.FHint:= GetHint();
    if Chart.FShowCross then
    begin
      DrawCross(FChart.FCrossX, FChart.FCrossY);
    end;
    DrawHint();
  end;

end;

procedure TSignalRectangeDrawer.DoSizeChagned;
begin
  // dummy
end;

procedure TSignalRectangeDrawer.DrawCross(X, Y: Single);
begin
  With Chart.Canvas, Chart do
  begin
    Stroke.Color := TAlphaColors.Black;
    Stroke.Thickness := 2;
    Stroke.Dash := TStrokeDash.Dash;
    Stroke.Kind := TBrushKind.Solid;

    DrawLine(TPointF.Create(FGraphicGridR.Left, FCrossY),
      TPointF.Create(FGraphicGridR.Right, FCrossY), CrossOpacity);

    Canvas.DrawLine(TPointF.Create(FCrossX, FGraphicGridR.Top),
      TPointF.Create(FCrossX, FGraphicGridR.Bottom), CrossOpacity);

    // Canvas.DrawLine(TPointF.Create(MousePosInClient.X, FWaterFallGridR.Top),
    // TPointF.Create(MousePosInClient.X, FWaterFallGridR.Bottom), CrossOpacity);
  end;

end;

procedure TSignalRectangeDrawer.DrawHint;
begin
//dummy
end;

procedure TSignalRectangeDrawer.SetFalloffDecrement(const Value: Single);
begin
  FFalloffDecrement := Value;
end;

procedure TSignalRectangeDrawer.SetFalloffVisible(const Value: Boolean);
begin
  FFalloffVisible := Value;
end;

procedure TSignalRectangeDrawer.SetPeakeDecrement(const Value: Single);
begin
  FPeakDecrement := Value;
end;

procedure TSignalRectangeDrawer.SetPeakVisible(const Value: Boolean);
begin
  FPeakVisible := Value;
end;

{ TSignalDrawer }


procedure TSignalDrawer.SetChart(const Value: TSignalChart);
var
  LastChart: TSignalChart;
begin
  LastChart := FChart;
  if FChart <> Value then
  begin
    FChart := Value;

    if LastChart <> Nil then
    begin
      if LastChart.Drawer <> Nil then
        LastChart.Drawer := Nil;
    end;
  end;
  if FChart <> Nil then
    if FChart.Drawer <> self then
      FChart.Drawer := self;
end;
{ TSplitedDrawer }

/// /Input a value 0 to 255 to get a color value.
/// /The colours are a transition b - g - r
// uint32_t Wheel(byte WheelPos)
// {
// if (WheelPos < 85 ) {
// return Color(0,0, WheelPos * 3);
// } else if (WheelPos < 170) {
// WheelPos -= 85;
// return Color(0, WheelPos * 3 , 255 - WheelPos * 3);
// } else {
// WheelPos -= 170;
// return Color(WheelPos * 3, 255 - WheelPos * 3, 0);
// }
// }
//
function Wheel(Value: Byte): TAlphaColor;
begin
  With TAlphaColorRec(Result) do
  begin
    A := $FF;
    if Value < 85 then
    begin
      B := 0;
      G := 0;
      R := Value * 3;
    end
    else if Value < 170 then
    begin
      Dec(Value, 85);
      B := 0;
      G := Value * 3;
      R := 255 - Value * 3;
    end
    else
    begin
      Dec(Value, 170);
      B := Value * 3;
      G := 255 - Value * 3;
      R := 0;
    end;
  end;
end;

constructor TSplitedDrawer.Create(AOwner: TComponent);
  Procedure InitColors;
  var
    AColor: TAlphaColor;
    wavelen: Integer;
  begin
    for wavelen := WavelengthMinimum to WavelengthMaximum do
    begin
      With TAlphaColorRec(AColor) do
      begin
        A := $FF;
        WavelengthToRGB(wavelen, R, G, B);
      end;
      Insert(AColor, FColors, Length(FColors));
    end;
  end;

var
  tmp: TBitmap;
begin
  inherited;
  InitColors();
  FWaterFallBmp := TBitmap.Create;

  FColorImage := TImage.Create(Self);
  tmp := TBitmap.Create(1, 1);
  try
    FColorImage.Bitmap := tmp;
  finally
    tmp.Free;
  end;
end;

destructor TSplitedDrawer.Destroy;
begin
//  FreeAndNil(FColorImage);
  FreeAndNil(FWaterFallBmp);
  inherited;
end;

procedure TSplitedDrawer.DoDraw;
var
  HStep: Single;
  PointStart: TPointF;
  PointEnd: TPointF;
  i: Integer;
  ColorIndex: Cardinal;
var
  BmpData: TBitmapData;
  MoveBytes: Integer;
  AColor: TAlphaColor;
  Clip: TClipRects;
begin
  With Chart do
  begin
    if ComponentState * [csLoading, csReading] <> [] then
      Exit;
//    if Length(FData) = 0 then
//      Exit;

    if FWaterFallRectUpdated then
    begin
      FWaterFallBmp.Width := Ceil(FWaterFallGridR.Width);
      FWaterFallBmp.Height := Ceil(FWaterFallGridR.Height);
      // FWaterFallBmpOld.Assign(FWaterFallBmp);
      FWaterFallRectUpdated := False;
    end;


    // -----------------
    with FWaterFallBmp do
    begin
      if Map(TMapAccess.ReadWrite, BmpData) then
        try
          // 移动图像向下一像素 ,要每行移动，不要多行同时移动，否则会出错，
          // 而且对速度性能益处有限
          MoveBytes := Width * BmpData.BytesPerPixel;
          for i := Height - 2 Downto 0 do
            System.Move(BmpData.GetPixelAddr(0, 0 + i)^,
              BmpData.GetPixelAddr(0, 1 + i)^, MoveBytes);

          FWaterFallBmp.Canvas.Stroke.Kind := TBrushKind.Solid;
          FWaterFallBmp.Canvas.Fill.Kind := TBrushKind.Solid;
        finally
          Unmap(BmpData);
        end;
    end;

    if FWaterFallBmp.HandleAllocated then
    With FWaterFallBmp.Canvas do
    begin

      FWaterFallBmp.Canvas.Stroke.Kind := TBrushKind.Solid;
      FWaterFallBmp.Canvas.Fill.Kind := TBrushKind.Solid;

      // GPU方式和Direct2D方式，两种情况下面BITMAP的TOP座标似乎是不同的，
      // 需要用以下的方式来区分
      if GlobalUseGPUCanvas then
        PointStart := TPointF.Create(0, 1)
      else
        PointStart := TPointF.Create(0, 0);

      Insert(FWaterFallGridR, Clip, 0);
      Clip[0].Inflate(0, 2);

      // if BeginScene(@Clip, 0) then
      if BeginScene() then
        try
          HStep := FWaterFallGridR.Width / (Length(FData) - 1);

          for i := 0 to Length(FData) - 1 do
          begin
            if i > 0 then
              PointStart := PointEnd;

            PointEnd := PointStart;

            PointEnd.X := PointEnd.X + HStep;

            ColorIndex := Trunc(FData[i] * Length(FColors));
            if ColorIndex > Length(FColors) - 1 then
              AColor := TAlphaColors.White
            else
              AColor := FColors[ColorIndex];

            Stroke.Color := AColor;

            DrawLine(PointStart, PointEnd, 1);
          end;
        finally
          EndScene();
        end;
    end;

    Canvas.DrawBitmap(FWaterFallBmp, FWaterFallBmp.Bounds,
      FWaterFallGridR, 1, True);
  end;
  inherited;
end;

procedure TSplitedDrawer.DoSizeChagned;
var
  i: Integer;
  Step: Single;
  ColorRect: TRectF;
begin
  inherited;
  if FChart = Nil then
    Exit;
  FColorImage.Width := 20;
  FColorImage.Height := FChart.FWaterFallGridR.Height;

  FColorImage.Bitmap.Width := Ceil(FColorImage.Width);
  FColorImage.Bitmap.Height := Ceil(FColorImage.Height);

  FColorImage.Position.X := FChart.FWaterFallGridR.TopLeft.X - 30;
  FColorImage.Position.Y := FChart.FWaterFallGridR.TopLeft.Y;

  if FColorImage.Bitmap.HandleAllocated then
    With FColorImage.Bitmap.Canvas do
    begin
      Step := FColorImage.Bitmap.Height / Length(FColors);
      With FColorImage.Bitmap.Canvas do
      begin
        Fill.Kind := TBrushKind.Solid;
        if BeginScene(Nil, 0) then
          try
            ColorRect := TRectF.Create(0, 0, FColorImage.Bitmap.Width, Step);
            for i := Length(FColors) - 1 Downto 0 do
            begin
              Fill.Color := FColors[i];
              FillRect(ColorRect, 0, 0, [], 1);
              ColorRect.offset(0, Step);
            end;
          finally
            EndScene();
          end;
      end;
    end;

end;

procedure TSplitedDrawer.DrawCross(X, Y: Single);
begin
  inherited;
  With Chart do
  begin
    Canvas.Stroke.Color:= TAlphaColors.White;
    Canvas.DrawLine(TPointF.Create(FCrossX, FWaterFallGridR.Top),
      TPointF.Create(FCrossX, FWaterFallGridR.Bottom), CrossOpacity);
  end;
end;

procedure TSplitedDrawer.DrawHint;
var
  TextRect: TRectF;
begin
  inherited;
  With FChart do
  begin
    Canvas.Stroke.Color:= TAlphaColors.Red;
    Canvas.Fill.Color:= TAlphaColors.Red;
    Canvas.Stroke.Kind:= TBrushKind.Solid;
    Canvas.Fill.Kind:= TBrushKind.Solid;
    TextRect:= FGraphicGridR;
    TextRect.Offset(10, 10);
    Canvas.FillText(TextRect, FHint, True, 1, [], TTextAlign.Leading, TTextAlign.Leading);
  end;
end;

procedure TSplitedDrawer.SetChart(const Value: TSignalChart);
begin
  inherited;
  if Not (csDesigning in ComponentState) then
  begin
    if FColorImage <> Nil then
      FColorImage.Parent := Value;
  end;
end;





initialization

GlobalUseGPUCanvas := True;

// GlobalUseDX10Software:= True;
end.
