unit uRadioSpectrumChart;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.UITypes, //System.Contnrs,
  System.Math, System.DateUtils, System.IOUtils, System.Generics.Collections,
  FMX.Objects, FMX.Graphics, FMX.Types, FMX.Controls, FMX.STdCtrls,
  uSpectrumSelection;

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
  TAbstractSignalDrawer = Class;
  TSpectrumDrawer = Class;
  TSelectionManager = Class;

  TCustomAxis = Class(TPersistent)
  Private
    FMin: Integer;
    FMax: Integer;
    FViewMin: Integer;
    FViewMax: Integer;
    FViewRatioFrom: Single;
    FViewRatioTo: Single;
    FViewDataIdxFrom: Integer;
    FViewDataIdxTo: Integer;
    Procedure UpdateViewRatio;
  Private
    [weak]
    FDrawer: TAbstractSignalDrawer;
    FLines: TArray<TLine>;
    FThinkness: Single;
    FLineColor: TAlphaColor;

    FUnitStr: String;
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetThinkness(const Value: Single);
    procedure SetLineColor(const Value: TAlphaColor);
    Procedure DoChanged;
    procedure SetUnitStr(const Value: String);
    function FChart: TSignalChart;
    Procedure CheckViewRange;
    procedure SetViewMax(const Value: Integer);
    procedure SetViewMin(const Value: Integer);
    Function TransRatio2Mark(L, H: Single; PercentValue: Single): Single; inline;
    Function TransMark2Ratio(MarkValue: Single; L, H: Single): Single; inline;
  Private
    FSelectoinManager: TSelectionManager;
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
    Constructor Create(ADrawer: TAbstractSignalDrawer); Virtual;
    Destructor Destroy; Override;

    Class function CalcuStep(Range: Integer; N: Integer): Integer;
    Class Function CalcuValueStep(Limits: Single;
      MaxLableCount: Integer): Integer;
    Class Procedure UpdateLinesPos(var Lines: TArray<TLine>;
      MarkValueLimits, WidthLimits, HeightLimits: Integer;
      MaxLineCount: Integer; isVeritcal: Boolean);
    Procedure DrawGrid(Coordinate: TBitmap; MaxLableCount: Integer);
    Procedure DrawLables(const GridLocation: TPointF; ACanvas: TCanvas;
      StdTextR: TRectF);
    Procedure Zoom(Ratio: Single; ViewCenter: Single);
    Property ViewRatioFrom: Single Read FViewRatioFrom;
    Property ViewRatioTo: Single Read FViewRatioTo;
    Property ViewDataIdxFrom: Integer Read FViewDataIdxFrom;
    Property ViewDataIdxTo: Integer Read FViewDataIdxTo;
    Property SelectionManager: TSelectionManager Read FSelectoinManager;
  Published
    Property Min: Integer read FMin write SetMin;
    Property Max: Integer read FMax write SetMax;
    Property ViewMin: Integer read FViewMin write SetViewMin;
    Property ViewMax: Integer read FViewMax write SetViewMax;
    Property Thinkness: Single read FThinkness write SetThinkness;
    Property LineColor: TAlphaColor read FLineColor write SetLineColor;
    Property HTextAlign: TTextAlign Read GetHTextAlign;
    Property UnitStr: String Read FUnitStr Write SetUnitStr;
  End;

  TAxisSelection = Class
  Public type
    TSelectionAnchor = Record
      Left, Right: Single;
    End;
  Private Type
    TSelectionUI2 = Class(TSelectionUI)
    Protected
      Procedure DoTrack(); Override;
      procedure MoveHandle(AX, AY: Single); Override;
    Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    End;
  Strict Private
    [weak]FAxis: TCustomAxis;
    ////以轴的标称值表示的锚点

    FAnchor: TSelectionAnchor; //数值为轴的Mark值
    function GetAnchorLeft: Single;
    procedure SetAnchorRight(const Value: Single);
    function GetAnchorRight: Single;
    procedure SetAnchorLeft(const Value: Single);
    function GetAnchorRatioLeft: Single;
    function GetAnchorRatioRight: Single;
    procedure SetAnchorRatioLeft(const Value: Single);
    procedure SetAnchorRatioRight(const Value: Single);
  Private
    Procedure CheckUIBound;
  Protected
    FUI: TSelectionUI;
    Procedure DoAnchorChange(); Virtual;
  Public
    Constructor Create(Axis: TCustomAxis);
    Destructor Destroy; Override;
    Function TransAnchorToChart(AAnchor: TSelectionAnchor; var X1: Single; var X2: Single): Boolean;
    Function TransChartToAnchor(X1: Single; X2: Single; var AAnchor: TSelectionAnchor): Boolean;
    function GetDrawerAndChart(out Drawer: TSpectrumDrawer; out Chart: TSignalChart): Boolean;
    Property AnchorLeft: Single Read GetAnchorLeft Write SetAnchorLeft;
    Property AnchorRight: Single Read GetAnchorRight Write SetAnchorRight;
    Property AnchorRatioLeft: Single Read GetAnchorRatioLeft Write SetAnchorRatioLeft;
    Property AnchorRatioRight: Single Read GetAnchorRatioRight Write SetAnchorRatioRight;
    Property Anchor: TSelectionAnchor Read FAnchor;
  End;


  TSelectionManager = Class(TObjectList<TAxisSelection>)
  Private
  [Weak]
    FAxis: TCustomAxis;
  Public
    Function AddSelection: TAxisSelection;
    Property Axis: TCustomAxis Read FAxis Write FAxis;
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
    Constructor Create(ADrawer: TAbstractSignalDrawer); Override;
  end;

  TBottomAxis = Class(TCustomAxis)
  Private

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
    Constructor Create(ADrawer: TAbstractSignalDrawer); Override;
    Destructor Destroy; Override;

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
    Constructor Create(ADrawer: TAbstractSignalDrawer);
    Destructor Destroy; Override;
  Published
    Property Left: TCustomAxis Read FLeft Write FLeft;
    Property Bottom: TCustomAxis Read FBottom Write FBottom;
  End;

  TSignalChart = Class(TPaintBox)
  Private
    FBKGraphic: TBitmap;
    FHint, FHint2: String;
    FNeedUpdateBG: Boolean;
  Private
    FFrameCounter: TFrameCount;
    FDrawer: TAbstractSignalDrawer;

    function GetFPS: Integer;
    Procedure UpdateBitmap;
    procedure DoCheckSize;
    procedure SetDrawer(const Value: TAbstractSignalDrawer);
  Private
    FLastUpdateTime: TDateTime;
  Protected
    Procedure DoPaint; Override;
    Procedure Loaded; Override;
    Procedure DoResized; Override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer;
      var Handled: Boolean); Override;
  Protected
    Procedure MouseMove(Shift: TShiftState; X, Y: Single); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure DrawData(const AData: TArray<Single>);
    Procedure InvalidBackGround;
    Property FPS: Integer Read GetFPS;
  Published
    Property Drawer: TAbstractSignalDrawer read FDrawer write SetDrawer;
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

  TAbstractSignalDrawer = Class(TComponent)
  Private
    [weak]
    FChart: TSignalChart;
  Protected
    Procedure Internal_SetChart(const Value: TSignalChart); Virtual;
    procedure SetChart(const Value: TSignalChart); Virtual;
    Procedure DrawCross(X, Y: Single); Virtual; Abstract;
    Procedure AfterChangeSize(); Virtual; Abstract;
    Procedure UpdateGraphicRect; Virtual; Abstract;
    Procedure UpdateGridRAndStdTextR(); Virtual; Abstract;
    Procedure UpdateViewRangeIndex(Axis: TCustomAxis); Virtual; Abstract;
  Protected
    Procedure CheckUpdateBackGround;
  Public
    Procedure ChartSinkMouseMove(Chart: TSignalChart; Shift: TShiftState;
      X, Y: Single); Virtual; Abstract;
    Procedure ChartSinkMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      var Handled: Boolean); Virtual; Abstract;
    Procedure ChartSinkCheckSize(); Virtual; Abstract;
    Procedure ChartSinkDrawData(const AData: TArray<Single>); Virtual; Abstract;
    Procedure ChartSinkUpdateBitmap(BK: TBitmap); Virtual; Abstract;
  Public
    Procedure DoDraw(); Virtual; Abstract;
  Published
    Property Chart: TSignalChart Read FChart Write SetChart;
  End;

  TSpectrumDrawer = Class(TAbstractSignalDrawer)
  Private Type
    { TODO: 界面拖动时需要更新LeftPosPercent, RightPosPercent }
    { TODO: 拖动时FPS会增加，如何处理？ }
  Private
//    Procedure ResettleSelectoin(Value: TLocatedSelection);
    Procedure ResettleSelections;

  Private
    FGrid: TBitmap;
    // corss cursor
    FCrossOpacity: Single;
    FCrossX: Single;
    FCrossY: Single;
    FCursorInGraphicGrid: Boolean;
    FShowCross: Boolean;
    //
    FDataCount: Integer;
    FData: TArray<Single>;
    //
    FAxisesData: TAxises;
    // FAxisesView: TAxises;
    FLeftTextR: TRectF;
    FBottomTextR: TRectF;
    //
    FBKColor: TAlphaColor;
    FGridBoundColor: TAlphaColor;
    FSectionBrush: TBrush;
    Procedure FillDesigningTestData;
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
    procedure SetBKColor(const Value: TAlphaColor);
    procedure SetGridBoundColor(const Value: TAlphaColor);
  Protected
    FGraphicRect: TRectF;
    FGraphicGridR: TRectF;
    Procedure DrawCross(X, Y: Single); Override;
    Procedure AfterChangeSize(); Override;
    Procedure DrawHint; Virtual;
    Procedure UpdateGridRAndStdTextR(); Override;
    Procedure UpdateGraphicRect; Override;
    Procedure UpdateViewRangeIndex(Axis: TCustomAxis); Override;
  Protected
    Procedure Internal_SetChart(const Value: TSignalChart); Override;
  Protected
    Procedure Internal_DrawGraphicBound(ACanvas: TCanvas); Virtual;
    Procedure Internal_DrawGraphicFrame(ACanvas: TCanvas); Virtual;
    Procedure Internal_DrawViewData(const AData: TArray<Single>;
      ViewIdxFrom, ViewIdxTo: Integer); Virtual;
    Procedure Internal_UnSelectionMask;
  Public
    Procedure ChartSinkMouseMove(Chart: TSignalChart; Shift: TShiftState;
      X, Y: Single); Override;
    Procedure ChartSinkDrawData(const AData: TArray<Single>); Override;
    Procedure ChartSinkUpdateBitmap(BK: TBitmap); Override;
    Procedure ChartSinkCheckSize(); Override;
    Procedure ChartSinkMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      var Handled: Boolean); Override;
  Public
//    Procedure DeleteSelection(ASelection: TLocatedSelection);
    function AddSelectionViaPercent(AnchorFrom, AnchorTo: Single): TAxisSelection;
    Procedure ClearSection();
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure DoDraw; Override;
  Public //坐标计算　
    function CursorXRatio: Single; inline;
    function CursorYRatio: Single; inline;
    function CursorViewIndex: Single; inline;
    function CursorMarkX(): Single; inline;
    function CursorMarkY(): Single; inline;

    function Trans_RatioX2GridRCoordinal(RatioX: Single): Single; //inline;
    function Trans_RatioY2GridRCoordinal(RatioY: Single): Single; inline;
    function Trans_MarkX2GridRCoordinal(X: Single): Single; //inline;
    function Trans_MarkY2GridRCoordinal(Y: Single): Single; inline;

    function Trans_RatioX2Mark(Percent: Single): Single; inline;
    function Trans_RatioY2Mark(Percent: Single): Single; inline;
    function Trans_MarkX2Ratio(Mark: Single): Single; inline;
    function Trans_MarkY2Ratio(Mark: Single): Single; inline;

    function Trans_GridRCoordinalX2Ratio(X: Single): Single; inline;
    function Trans_GridRCoordinalY2Ratio(Y: Single): Single; inline;
    function Trans_GridRCoordinalX2Mark(X: Single): Single; inline;
    function Trans_GridRCoordinalY2Mark(Y: Single): Single; inline;
    Property CursorInGraphicGrid: Boolean Read FCursorInGraphicGrid;
  Published
    Property PeakDecrement: Single read FPeakDecrement write SetPeakeDecrement;
    Property FalloffDecrement: Single read FFalloffDecrement
      write SetFalloffDecrement;
    Property FalloffVisible: Boolean read FFalloffVisible
      write SetFalloffVisible;
    Property PeakVisible: Boolean read FPeakVisible write SetPeakVisible;
    Property CrossOpacity: Single Read FCrossOpacity Write FCrossOpacity;
    Property AxisesData: TAxises Read FAxisesData Write FAxisesData;
    Property BKColor: TAlphaColor read FBKColor write SetBKColor;
    Property GridBoundColor: TAlphaColor read FGridBoundColor
      write SetGridBoundColor;
  End;

  TWaterFallDrawer = Class(TSpectrumDrawer)
  Private Type
    TColorBar = Class(TImage)
    Strict Private
      FColorsLen: Integer;
      FColors: TArray<TAlphaColor>;
    Public
      Constructor Create(AOwner: TComponent); Override;
      Procedure InitColors(Gradient: TGradient);
      function InterpolateColor(offset: Single): TAlphaColor;
      Procedure UpdateBitmap;
    End;
  Private
    FWaterFallRect: TRectF;
    FWaterFallGridR: TRectF;
    FWaterFallRectUpdated: Boolean;

    FWaterFallBuf: TBitmap;
    FWaterFallBufChanging: Boolean;

    FWaterFallBmpStart: Integer;

    FColorBar: TColorBar;
    FShowWaterfall: Boolean;
    FLargeBuf: Boolean;
    FOnColorBarClick: TNotifyEvent;
    FColorBarGradient: TGradient;
    procedure SetShowWaterfall(const Value: Boolean);
    procedure DockColorBar();
    procedure SetLargeBuf(const Value: Boolean);
    procedure SetOnColorBarClick(const Value: TNotifyEvent);
    Procedure HandleColorBarClickProc(Sender: TObject);
    procedure SetColorBarGradient(const Value: TGradient);
    Procedure InitGradientRainBow(const AGradient: TGradient);
  Protected
    Procedure DrawHint; Override;
    Procedure DrawCross(X, Y: Single); Override;
    Procedure AfterChangeSize(); Override;
    procedure SetChart(const Value: TSignalChart); Override;
    Procedure UpdateGridRAndStdTextR(); Override;
    Procedure UpdateGraphicRect; Override;
    Procedure Internal_DrawGraphicBound(ACanvas: TCanvas); Override;
    Procedure Internal_DrawGraphicFrame(ACanvas: TCanvas); Override;
    Procedure Internal_DrawViewData(const AData: TArray<Single>;
      ViewIdxFrom, ViewIdxTo: Integer); Override;
  Public
    constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
  Published
    Property ShowWaterfall: Boolean read FShowWaterfall write SetShowWaterfall;
    Property LargeBuf: Boolean read FLargeBuf write SetLargeBuf;
    Property OnColorBarClick: TNotifyEvent read FOnColorBarClick
      write SetOnColorBarClick;
    Property ColorBarGradient: TGradient read FColorBarGradient
      write SetColorBarGradient;
  End;

procedure Register;

implementation

uses
  System.Diagnostics, SpectraLibrary, FMX.PlatForm{$IFDEF MSWINDOWS}, CnDebug
{$ENDIF};

{ TSpectrumChart }

procedure Register;
begin
  RegisterComponents('RadioReceiver', [TSignalChart, TSpectrumDrawer, TTest,
    TWaterFallDrawer, TSelection6P, TSelectionUI]);
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
    end);
end;

function TSignalChart.GetFPS: Integer;
begin
  Result := FFrameCounter.FPS;
end;

procedure TSignalChart.InvalidBackGround;
begin
  FNeedUpdateBG := True;;
  InvalidateRect(LocalRect);
end;

// function TSignalChart.GraphicToClient(const APoint: TRectF): TRectF;
// begin
// Result := APoint;
// Result.offset(-FGraphicRect.Left, -FGraphicRect.Top);
// end;
//
// function TSignalChart.GraphicToGrid(const APoint: TRectF): TRectF;
// begin
// Result := APoint;
// Result.offset(FGraphicGridR.TopLeft);
// end;
//
// function TSignalChart.GridToClient(const APoint: TRectF): TRectF;
// begin
// Result := APoint;
// Result.offset(FGraphicGridR.TopLeft);
// end;
//
// function TSignalChart.GridToGraphic(const APoint: TRectF): TRectF;
// begin
// Result := APoint;
// Result.offset(-FGraphicRect.Left, -FGraphicRect.Top);
// end;

procedure TSignalChart.Loaded;
begin
  inherited;

end;

procedure TSignalChart.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  if FDrawer <> Nil then
  begin
    FDrawer.ChartSinkMouseMove(Self, Shift, X, Y);

    if MilliSecondSpan(Now, FLastUpdateTime) > 30 then
    begin
      InvalidateRect(LocalRect);
    end;
  end;
end;

procedure TSignalChart.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
var Handled: Boolean);
begin
  if Self.FDrawer <> Nil then
  begin
    FDrawer.ChartSinkMouseWheel(Shift, WheelDelta, Handled);
  end;
  if Not Handled then
    inherited;
end;

procedure TSignalChart.DoCheckSize;
begin
  if FDrawer <> Nil then
    FDrawer.ChartSinkCheckSize();
end;

procedure TSignalChart.SetDrawer(const Value: TAbstractSignalDrawer);
var
  LastDrawer: TAbstractSignalDrawer;
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
    if Value.Chart <> Self then
    begin
      Value.Chart := Self;
      // redraw with the new drawer
      if FDrawer <> Nil then
        FDrawer.UpdateGraphicRect();
      UpdateBitmap;
      InvalidateRect(LocalRect);
    end;
end;

{ TGridLayer }


// function TSignalChart.ClientToGraphic(const APoint: TRectF): TRectF;
// begin
// Result := APoint;
// Result.offset(FGraphicRect.TopLeft);
// end;
//
// function TSignalChart.ClientToGrid(const APoint: TRectF): TRectF;
// begin
// Result := APoint;
// Result.offset(-FGraphicGridR.Left, -FGraphicGridR.Top);
// end;

constructor TSignalChart.Create;
begin
  inherited;
  FFrameCounter := TFrameCount.Create;
  // FEquationBottomIn := TLinearEquations.Create;
  FBKGraphic := TBitmap.Create;
end;

destructor TSignalChart.Destroy;
begin
  if FDrawer <> Nil then
    FDrawer.SetChart(Nil);
  FreeAndNil(FBKGraphic);
  // FreeAndNil(FEquationBottomIn);
  FreeAndNil(FFrameCounter);
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
  // if Length(AData) <> Length(FData) then
  // begin
  // FEquationBottomIn.CalcuCoff(0, 0, Length(AData), FGraphicGridR.Width - 1);
  // end;
  if FDrawer <> Nil then
  begin
    FDrawer.ChartSinkDrawData(AData);
    InvalidateRect(ClipRect);
  end;
end;

procedure TSignalChart.UpdateBitmap;
begin
  if FDrawer <> Nil then
    FDrawer.ChartSinkUpdateBitmap(FBKGraphic);
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

procedure TCustomAxis.CheckViewRange;
begin
  if FViewMin > FViewMax then
  begin
    FViewMin := FViewMin xor FViewMax;
    FViewMax := FViewMax xor FViewMin;
    FViewMin := FViewMin xor FViewMax;
  end;
  if FViewMin < FMin then
    FViewMin := FMin;
  if FViewMax > FMax then
    FViewMax := FMax;

  UpdateViewRatio();
end;

constructor TCustomAxis.Create(ADrawer: TAbstractSignalDrawer);
begin
  inherited Create;
  FDrawer := ADrawer;
  FMax := 0;
  FMin := -140;
  FThinkness := 1;
  FLineColor := TAlphaColors.Darkslategray;
  FViewMax := 0;
  FViewMin := -140;
  UpdateViewRatio;

  FSelectoinManager:= TSelectionManager.Create(True);
  FSelectoinManager.FAxis:= Self;
end;

destructor TCustomAxis.Destroy;
begin
  FSelectoinManager.Free;
  inherited;
end;

procedure TCustomAxis.DoChanged;
begin
  if FChart <> Nil then
  begin
    if [csReading, csLoading] * FChart.ComponentState = [] then
    begin
      if FDrawer <> Nil then
        FDrawer.UpdateGraphicRect();
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

  UpdateLinesPos(FLines, FViewMax - FViewMin, Coordinate.Width,
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

    LableStep := (FViewMax - FViewMin) / (Length(FLines) - 1);
    for i := 0 to Length(FLines) - 1 do
    begin
      R := CalcuLabelR(FLines[i], StdTextR, GridLocation);
      FillText(R, CalcuLableText(i, LableStep), False, 1, [], HTextAlign,
        TTextAlign.Center);
    end;
  end;
end;

function TCustomAxis.FChart: TSignalChart;
begin
  Result := FDrawer.FChart;
end;

function TCustomAxis.TransMark2Ratio(MarkValue, L, H: Single): Single;
begin
  Result:= 0;
  if (H <> L ) then
  begin
    Result:= (MarkValue - L ) / (H - L);
  end;
end;

function TCustomAxis.TransRatio2Mark(L, H, PercentValue: Single): Single;
begin
  Result:= (H - L) * PercentValue + L;
//  Result:= H-(H - L) * PercentValue;
end;

procedure TCustomAxis.SetUnitStr(const Value: String);
begin
  if FUnitStr <> Value then
  begin
    FUnitStr := Value;
    DoChanged;
  end;
end;

procedure TCustomAxis.SetViewMax(const Value: Integer);
begin
  if FViewMax <> Value then
  begin
    FViewMax := Value;
    CheckViewRange();
  end;
end;

procedure TCustomAxis.SetViewMin(const Value: Integer);
begin
  if FViewMin <> Value then
  begin
    FViewMin := Value;
    CheckViewRange();
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

procedure TCustomAxis.SetMax(const Value: Integer);
begin
  if FMax <> Value then
  begin
    if FViewMax = FMax then
      FViewMax := Value;

    FMax := Value;
    CheckViewRange();
    DoChanged();
  end;
end;

procedure TCustomAxis.SetMin(const Value: Integer);
begin
  if FMin <> Value then
  begin
    if FViewMin = FMin then
      FViewMin := Value;

    FMin := Value;
    CheckViewRange();
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

procedure TCustomAxis.UpdateViewRatio;
var
  Total: Single;
begin
  Total := FMax - FMin; // 网络数，而不是线条数
  FViewRatioFrom := (FViewMin - FMin) / Total;
  FViewRatioTo := (FViewMax - FMin) / Total;

  if FDrawer <> Nil then
  begin
    FDrawer.UpdateViewRangeIndex(Self);
  end;

end;

procedure TCustomAxis.Zoom(Ratio, ViewCenter: Single);
var
  BoundL, BoundH: Single;
  BoundH_Int, BoundL_Int: Integer;
  L0, H0: Single;
begin
  H0 := FViewMax;
  L0 := FViewMin;
  if InRange(ViewCenter, L0, H0) then
  begin

    if H0 <> L0 then
    begin
      BoundL := (H0 - ViewCenter) * ViewCenter - (ViewCenter - L0) *
        ((H0 - L0) * Ratio - ViewCenter);
      BoundL := BoundL / (H0 - L0);
      BoundH := (H0 - L0) * Ratio + BoundL;

      BoundH_Int := EnsureRange(Ceil(BoundH), FMin, FMax);
      BoundL_Int := EnsureRange(Floor(BoundL), FMin, FMax);
      if BoundH > BoundL then
      begin
        ViewMax := BoundH_Int;
        ViewMin := BoundL_Int;
      end;
    end;
  end;


  if FDrawer.Chart <> Nil then
    FDrawer.Chart.InvalidBackGround;
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
  Result := IntToStr(FViewMax - Round(LableStep * Index)) + UnitStr;
end;

function TLeftAxis.CalcuMaxLabel(const GraphicR: TRectF;
StdTextR: TRectF): Integer;
begin
  Result := Trunc(GraphicR.Height / StdTextR.Height);
end;

constructor TLeftAxis.Create(ADrawer: TAbstractSignalDrawer);
begin
  inherited;
  FMax := 0;
  FMin := -140;
  FUnitStr := 'dB'
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
  Result := IntToStr(FViewMin + Round(LabelStep * Index)) + UnitStr;
end;

function TBottomAxis.CalcuMaxLabel(const GraphicR: TRectF;
StdTextR: TRectF): Integer;
begin
  Result := Trunc(GraphicR.Width / StdTextR.Width);
end;

constructor TBottomAxis.Create(ADrawer: TAbstractSignalDrawer);
begin
  inherited;
  FMax := 1023;
  FMin := 0;
  FUnitStr := 'MHz';

end;

destructor TBottomAxis.Destroy;
begin
  inherited;
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

constructor TAxises.Create(ADrawer: TAbstractSignalDrawer);
begin
  inherited Create;
  FLeft := TLeftAxis.Create(ADrawer);
  FBottom := TBottomAxis.Create(ADrawer);
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

procedure TSpectrumDrawer.ChartSinkCheckSize;
var
  Changed: Boolean;
begin
  Changed := False;

  UpdateGraphicRect;

  With Chart do
  begin
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
    FNeedUpdateBG := FNeedUpdateBG or Changed;
    CheckUpdateBackGround();
  end;
end;

procedure TSpectrumDrawer.ChartSinkDrawData(const AData: TArray<Single>);
var
  nCount: Integer;
begin
  FData := AData;
  nCount := Length(FData);
  if FDataCount <> nCount then
  begin
    FAxisesData.Bottom.CheckViewRange;
    FAxisesData.Left.CheckViewRange;
    FDataCount := nCount;
  end;
end;

procedure TSpectrumDrawer.ChartSinkMouseMove(Chart: TSignalChart;
Shift: TShiftState; X, Y: Single);
begin
  inherited;
  FCrossX := X;
  FCrossY := Y;
  FCursorInGraphicGrid := PtInRect(FGraphicGridR, TPointF.Create(X, Y));

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

procedure TSpectrumDrawer.ChartSinkMouseWheel(Shift: TShiftState;
WheelDelta: Integer; var Handled: Boolean);
begin
  if CursorInGraphicGrid then
  begin
    if WheelDelta < 0 then
      AxisesData.Bottom.Zoom(abs(WheelDelta) / 120, CursorViewIndex)
    else if WheelDelta > 0 then
      AxisesData.Bottom.Zoom(120 / abs(WheelDelta), CursorViewIndex);
  end;
end;

procedure TSpectrumDrawer.ChartSinkUpdateBitmap(BK: TBitmap);
  Procedure DrawControlBound(ACanvas: TCanvas);
  const
    CONST_FRAME_COLOR: TAlphaColor = TAlphaColors.Black;
    CONST_FRAME_THICKNESS: Single = 2;
  begin
    ACanvas.Stroke.Dash := TStrokeDash.Dash;
    ACanvas.Stroke.Thickness := CONST_FRAME_THICKNESS;
    ACanvas.Stroke.Color := CONST_FRAME_COLOR;
    ACanvas.DrawRect(Chart.LocalRect, 0, 0, [], 1);
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

  // FEquationBottomIn.CalcuCoff(0, 0, Length(FData), FGraphicGridR.Width - 1);

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
  if BK.HandleAllocated then
  begin
    // 画底图，主要是坐标轴的Label
    ACanvas := BK.Canvas;
    Insert(Chart.LocalRect, Clips, 0);
    if ACanvas.BeginScene(@Clips, 0) then
      try
        ACanvas.Clear(FBKColor);
        With FAxisesData.Left do
          DrawLables(FGraphicGridR.Location, ACanvas, FLeftTextR);
        With FAxisesData.Bottom do
          DrawLables(FGraphicGridR.Location, ACanvas, FBottomTextR);

        // Internal_DrawGraphicBound(ACanvas);
        Internal_DrawGraphicFrame(ACanvas);

        DrawControlBound(ACanvas);

        ACanvas.DrawBitmap(FGrid, FGrid.BoundsF, FGraphicGridR, 1, True);
      finally
        ACanvas.EndScene;
      end;
  end;
  // DrawData(FData);
end;

procedure TSpectrumDrawer.ClearSection;
begin
//  FSelections.Clear;
end;

constructor TSpectrumDrawer.Create(AOwner: TComponent);
begin
  inherited;
//  FSelections := TObjectList<TLocatedSelection>.Create(True);
  FSectionBrush := TBrush.Create(TBrushKind.Solid, TAlphaColorRec.Gray);
  FGrid := TBitmap.Create;
  FAxisesData := TAxises.Create(Self);
  // FAxisesView := TAxises.Create(self);

  FFalloffDecrement := 0.005;
  FPeakDecrement := 0.001;
  FPeakVisible := True;
  FFalloffVisible := True;
  FCrossOpacity := 0.3;
  FShowCross := True;

  FGridBoundColor := TAlphaColors.Darkslategray;
  if csDesigning in ComponentState then
    FillDesigningTestData();
end;

//procedure TSpectrumDrawer.DeleteSelection(ASelection: TLocatedSelection);
//var
//  Index: Integer;
//begin
//  Index := FSelections.IndexOf(ASelection);
//  if Index > 0 then
//    FSelections.Delete(Index);
//end;

destructor TSpectrumDrawer.Destroy;
begin
  // FreeAndNil(FAxisesView);
  FreeAndNil(FAxisesData);
  FreeAndNil(FGrid);
  FreeAndNil(FSectionBrush);

//  FreeAndNil(FSelections);
  inherited;
end;

procedure TSpectrumDrawer.DoDraw;
  function GetHint: String;
  var
    RatioX, RatioY: Single;
    DataIndex: Integer;
    LabelX, LabelY: String;
    PeakY: String;
  begin
    Result := '';

    With Chart do
    begin
      RatioX := CursorXRatio;
      RatioY := CursorYRatio;

      With FAxisesData.Bottom do
      begin
        DataIndex := Round((ViewDataIdxTo - ViewDataIdxFrom) *
          System.Math.Min(RatioX, 1)) + ViewDataIdxFrom;
      end;

      //Chart坐标
      Result := Format('GridR : (%.2f, %.2f)'#$D#$A +
                       'Cursor: (%.2f, %.2f)=>'#$D#$A +
                       '        (%.2f, %.2f)=>'#$D#$A +
                       '        (%.2f%%, %.2f%%)'#$D#$A,
        [FGraphicGridR.Left, FGraphicGridR.Top, FCrossX, FCrossY,
        FCrossX - FGraphicGridR.Left, FCrossY - FGraphicGridR.Top,  RatioX * 100, RatioY * 100]);

      //根据光标位置计算的的XY MARK值
      With FAxisesData.Bottom do
        LabelX := Format('%.2f' + FUnitStr, [CursorMarkX()]);
      With FAxisesData.Left do
        LabelY := Format('%.2f' + FUnitStr, [CursorMarkY()] );
      Result := Result + Format('Labled Value: (%s, %s)', [LabelX, LabelY]);

      //根据光标得到当前指向的数据值
      if Length(FData) > 0 then
      begin
        Result := Result + Format(#$D#$A'Data[%d] = %.2f',
          [DataIndex, FPeaks[DataIndex]]);


        With FAxisesData.Left do
          PeakY := Format('%.2f' + FUnitStr,
            [FViewMin + (FViewMax - FViewMin) * FPeaks[DataIndex]]);

        Result := Result + Format(#$D#$A'Peak Value: %s', [PeakY]);

      end;
    end;
  end;

var
  i: Integer;
  di: Single;
  nCount: Integer;
  // nViewCount: Integer;
begin
  if Chart.ComponentState * [csLoading, csReading] <> [] then
    Exit;

  Internal_UnSelectionMask();

  With FAxisesData.Bottom do
  begin
    nCount := Length(FData);

    if Length(FPeaks) < nCount then
      SetLength(FPeaks, nCount);
    if Length(FFallOff) < nCount then
      SetLength(FFallOff, nCount);
    for i := 0 to Length(FData) - 1 do
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


    // nViewCount := ViewIdxTo - ViewIdxFrom + 1;
    // if (nCount = 0) or (ViewIdxTo >= nCount) or (ViewIdxTo < 0) then
    // Exit;

    if (nCount > 0) and InRange(ViewDataIdxTo, 0, nCount - 1) then
      // Internal_DrawViewData(FData, ViewIdxFrom, ViewIdxTo);
      Internal_DrawViewData(FFallOff, ViewDataIdxFrom, ViewDataIdxTo);
  end;

  if FCursorInGraphicGrid then
  begin
    Chart.FHint := GetHint();
    if FShowCross then
    begin
      DrawCross(FCrossX, FCrossY);
    end;
    DrawHint();
  end;
end;

function TSpectrumDrawer.AddSelectionViaPercent(AnchorFrom, AnchorTo: Single)
  : TAxisSelection;
begin
  Result:= FAxisesData.Bottom.SelectionManager.AddSelection;
  With  Result do
  begin
    AnchorRatioRight:= AnchorTo;
    AnchorRatioLeft:= AnchorFrom;
    DoAnchorChange();
  end;
end;

procedure TSpectrumDrawer.AfterChangeSize;
begin
  // dummy
end;

procedure TSpectrumDrawer.DrawCross(X, Y: Single);
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

procedure TSpectrumDrawer.DrawHint;
var
  TextRect: TRectF;
begin
  // Property ViewRatioFrom: Single Read FViewRatioFrom;
  // Property ViewRatioTo: Single Read FViewRatioTo;
  // Property ViewIdxFrom: Integer Read FViewIdxFrom;
  // Property ViewIdxTo: Integer Read FViewIdxTo;
  With FAxisesData.Bottom do
  begin
    FChart.FHint2 := Format('Ratio:[%.2f, %.2f], Idx: [%d, %d], Zoom: %.2f%%',
      [ViewRatioFrom, ViewRatioTo, Min, Max,
      (ViewRatioTo - ViewRatioFrom) * 100]);
    FChart.FHint2 := FChart.FHint2 + #$D#$A;
    FChart.FHint2 := FChart.FHint2 +
      Format('Data Count: %d, Data Idx: [%d, %d]',
      [Length(FData), ViewDataIdxFrom, ViewDataIdxTo]);

    FChart.FHint2 := FChart.FHint2 + #$D#$A;
    FChart.FHint2 := FChart.FHint2 +
      Format('CURSOR==RatioX = %.2f, RatioY = %.2f, ViewIndex = %.2f',
      [Self.CursorXRatio, Self.CursorYRatio, CursorViewIndex]);
  end;

  With FChart do
  begin
    Canvas.Stroke.Color := TAlphaColors.Red;
    Canvas.Fill.Color := TAlphaColors.Red;
    Canvas.Stroke.Kind := TBrushKind.Solid;
    Canvas.Fill.Kind := TBrushKind.Solid;
    TextRect := FGraphicGridR;
    TextRect.Right := FGraphicGridR.Width / 3 + FGraphicGridR.Left;
    TextRect.offset(10, 10);
    Canvas.FillText(TextRect, FHint, True, 1, [], TTextAlign.Leading,
      TTextAlign.Leading);
    TextRect.offset(TextRect.Width, 0);
    Canvas.FillText(TextRect, FHint2, True, 1, [], TTextAlign.Leading,
      TTextAlign.Leading);
  end;
end;

procedure TSpectrumDrawer.FillDesigningTestData;
var
  i: Integer;
  nCount: Integer;
begin
  // Exit;
  if csDesigning in ComponentState then
  begin
    nCount := FAxisesData.Bottom.Max - FAxisesData.Bottom.Min + 1;
    if nCount > 100 then
      nCount := 100;
    if nCount < 0 then
      nCount := 10;

    SetLength(FData, nCount);
    for i := 0 to nCount - 1 do
    begin
      FData[i] := (cos(2 * pi * i / (nCount - 1)) + 1) / 2
    end;
  end;
end;

procedure TSpectrumDrawer.Internal_DrawViewData(const AData: TArray<Single>;
ViewIdxFrom, ViewIdxTo: Integer);
  function GetHint: String;
  var
    ptPosInGrid: TPointF;
    RatioX, RatioY: Single;
    DataIndex: Integer;
    LabelX, LabelY: String;
    PeakY: String;
  begin
    Result := '';
    With Chart do
    begin
      ptPosInGrid := TPointF.Create(FCrossX - FGraphicGridR.Left,
        FCrossY - FGraphicGridR.Top);
      RatioX := ptPosInGrid.X / FGraphicGridR.Width;
      RatioY := ptPosInGrid.Y / FGraphicGridR.Height;
      With FAxisesData.Bottom do
      begin
        // DataIndex:= Ceil(FGraphicGridR.Width * RatioX) * (FMaxValue - FMinValue + 1);
        // DataIndex:= Ceil((FMaxValue - FMinValue + 1+0.5) * RatioX) ;
        // DataIndex:= Ceil(Length(FData) * Min((RatioX + 0.005), 1));
        DataIndex := Round(Length(AData) * System.Math.Min(RatioX, 1));

      end;

      Result := Format('GridR (%.2f, %.2f)'#$D#$A +
        'Cross: (%.2f, %.2f)=>(%.2f, %.2f)'#$D#$A +
        'Cross Percent: (%.2f%%, %.2f%%)'#$D#$A,
        [FGraphicGridR.Left, FGraphicGridR.Top, FCrossX, FCrossY, ptPosInGrid.X,
        ptPosInGrid.Y, RatioX * 100, RatioY * 100]);

      if Length(AData) > 0 then
      begin
        Result := Result + Format('Data[%d] = %.2f'#$D#$A,
          [DataIndex, AData[DataIndex]]);
        With FAxisesData.Bottom do
          LabelX := Format('%.2f' + FUnitStr,
            [FViewMin + (FViewMax - FViewMin + 1) * RatioX]);

        With FAxisesData.Left do
          LabelY := Format('%.2f' + FUnitStr,
            [FViewMax - (FViewMax - FViewMin + 1) * RatioY]);
        With FAxisesData.Left do
          PeakY := Format('%.2f' + FUnitStr,
            [FViewMin + (FViewMax - FViewMin + 1) * FPeaks[DataIndex]]);

        Result := Result + Format('Labled Value: (%s, %s)'#$D#$A,
          [LabelX, LabelY]);
        Result := Result + Format('Peak Value: (%s, %s)', [LabelX, PeakY]);

      end;
    end;
  end;

var
  HStep: Single;
  FalloffR: TRectF;
  PeakR: TRectF;
  i: Integer;
  ACanvas: TCanvas;
  nViewCount: Integer;
begin
  With Chart do
  begin
    nViewCount := ViewIdxTo - ViewIdxFrom + 1;

    HStep := FGraphicGridR.Width / (nViewCount - 1);

    ACanvas := Canvas;
    ACanvas.Stroke.Color := TAlphaColors.Black;
    ACanvas.Stroke.Kind := TBrushKind.Solid;
    ACanvas.Stroke.Thickness := 1;
    ACanvas.Fill.Color := TAlphaColors.Lime;
    ACanvas.Fill.Kind := TBrushKind.Solid;

    for i := 0 to nViewCount - 1 do
    begin
      FalloffR := TRectF.Create(0, FGraphicGridR.Height *
        (1 - AData[i + ViewIdxFrom]), HStep, FGraphicGridR.Height - 0);

      FalloffR.offset((i - 0.5) * HStep, 0);
      FalloffR.offset(FGraphicGridR.TopLeft);
      FalloffR.Left := EnsureRange(FalloffR.Left, FGraphicGridR.Left,
        FGraphicGridR.Right);
      FalloffR.Right := EnsureRange(FalloffR.Right, FGraphicGridR.Left,
        FGraphicGridR.Right);

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
        PeakR.Bottom := FGraphicGridR.Height * (1 - FPeaks[i + ViewIdxFrom]) +
          FGraphicGridR.Top;
        PeakR.Top := PeakR.Bottom - 1;
        ACanvas.FillRect(PeakR, 0, 0, [], 1);
      end;
    end;
  end;

end;

procedure TSpectrumDrawer.Internal_SetChart(const Value: TSignalChart);
  Procedure SetSelectUIParentNil(List: TSelectionManager);
  var
    i: Integer;
  begin
    for i := 0 to List.Count - 1 do
    begin
      List.Items[i].FUI.Parent:= Nil;
    end;
  end;
begin
  inherited;
  SetSelectUIParentNil(self.FAxisesData.Bottom.SelectionManager);
  SetSelectUIParentNil(self.FAxisesData.Left.SelectionManager);
end;

procedure TSpectrumDrawer.Internal_UnSelectionMask;
var
  // ARect: TRectF;
  // ExcludeRect: TRectF;
//  iSection: TLocatedSelection;
  Save: TCanvasSaveState;
begin
  Exit;
//  // ARect:=  TRectF.Create(FGraphicGridR.TopLeft, 100, FGraphicGridR.Height);
//
//  // ExcludeRect:= TRectF.Create(100, 0, 200, FGraphicGridR.Height);
//  // ExcludeRect.Offset(FGraphicGridR.TopLeft);
//
//  if Not GlobalUseGPUCanvas then
//  begin
//    if FSelections.Count > 0 then
//    begin
//      Save := Chart.Canvas.SaveState;
//      try
//        // Chart.Canvas.ExcludeClipRect(ExcludeRect);
//        for iSection in FSelections do
//        begin
//          Chart.Canvas.ExcludeClipRect(iSection.BoundsRect);
//        end;
//
//        Chart.Canvas.FillRect(Self.FGraphicGridR, 0, 0, [], 0.3, FSectionBrush);
//      finally
//        Chart.Canvas.RestoreState(Save);
//      end;
//    end;
//  end;
end;

function TSpectrumDrawer.CursorXRatio: Single;
begin
  Result:= Trans_GridRCoordinalX2Ratio(FCrossX - FGraphicGridR.Left);
end;

function TSpectrumDrawer.CursorYRatio: Single;
begin
  Result:= Trans_GridRCoordinalY2Ratio(FCrossY - FGraphicGridR.Top);
end;

procedure TSpectrumDrawer.ResettleSelections;
//var
//  iSelection: TLocatedSelection;
begin
//  for iSelection in FSelections do
//  begin
//    ResettleSelectoin(iSelection);
//  end;
end;

//procedure TSpectrumDrawer.ResettleSelectoin(Value: TLocatedSelection);
//var
//  ALeft: Single;
//  ARight: Single;
//begin
//  With Value do
//  begin
//    ALeft := FGraphicGridR.Width * LeftPosPercent;
//    ARight := FGraphicGridR.Width * RightPosPercent;
//
//    Width := ARight - ALeft;
//    Height := FGraphicGridR.Height;
//
//    Parent := FChart;
//
//    Position.X := ALeft + FGraphicGridR.Left;
//    Position.Y := FGraphicGridR.Top;
//  end;
//end;

procedure TSpectrumDrawer.Internal_DrawGraphicBound(ACanvas: TCanvas);
const
  CONST_FRAME_COLOR: TAlphaColor = TAlphaColors.Lime;
  CONST_FRAME_THICKNESS: Single = 2;

begin
  ACanvas.Stroke.Dash := TStrokeDash.Dash;
  ACanvas.Stroke.Thickness := CONST_FRAME_THICKNESS;
  ACanvas.Stroke.Color := CONST_FRAME_COLOR;
  ACanvas.DrawRect(FGraphicRect, 0, 0, [], 1);

end;

procedure TSpectrumDrawer.Internal_DrawGraphicFrame(ACanvas: TCanvas);
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

procedure TSpectrumDrawer.SetBKColor(const Value: TAlphaColor);
begin
  if FBKColor <> Value then
  begin
    FBKColor := Value;
    Chart.UpdateBitmap;
    Chart.InvalidateRect(Chart.ClipRect);
  end;
end;

procedure TSpectrumDrawer.SetFalloffDecrement(const Value: Single);
begin
  FFalloffDecrement := Value;
end;

procedure TSpectrumDrawer.SetFalloffVisible(const Value: Boolean);
begin
  FFalloffVisible := Value;
end;

procedure TSpectrumDrawer.SetGridBoundColor(const Value: TAlphaColor);
begin
  if FGridBoundColor <> Value then
  begin
    FGridBoundColor := Value;
    Chart.UpdateBitmap;
    Chart.InvalidateRect(Chart.ClipRect);
  end;
end;

procedure TSpectrumDrawer.SetPeakeDecrement(const Value: Single);
begin
  FPeakDecrement := Value;
end;

procedure TSpectrumDrawer.SetPeakVisible(const Value: Boolean);
begin
  FPeakVisible := Value;
end;

function TSpectrumDrawer.Trans_GridRCoordinalX2Mark(X: Single): Single;
begin
  Result:= Trans_RatioX2Mark(Trans_GridRCoordinalX2Ratio(X));
end;

function TSpectrumDrawer.Trans_GridRCoordinalX2Ratio(X: Single): Single;
begin
  Result := (X + 1) / (FGraphicGridR.Width);
end;

function TSpectrumDrawer.Trans_GridRCoordinalY2Mark(Y: Single): Single;
begin
  Result:= Trans_RatioY2Mark(Trans_GridRCoordinalY2Ratio(Y));
end;

function TSpectrumDrawer.Trans_GridRCoordinalY2Ratio(Y: Single): Single;
begin
  Result := (Y + 1) / (FGraphicGridR.Height);
end;

function TSpectrumDrawer.Trans_MarkX2GridRCoordinal(X: Single): Single;
var
  ARatio: Single;
begin
  ARatio:= Trans_MarkX2Ratio(X);
  Result:= Trans_RatioX2GridRCoordinal(ARatio);
end;

function TSpectrumDrawer.Trans_MarkX2Ratio(Mark: Single): Single;
begin
  Result:= FAxisesData.Bottom.TransMark2Ratio(Mark,
    FAxisesData.Bottom.ViewMin,
    FAxisesData.Bottom.ViewMax);
end;

function TSpectrumDrawer.Trans_MarkY2GridRCoordinal(Y: Single): Single;
var
  APercent: Single;
begin
  APercent:= Trans_MarkY2Ratio(Y);
  Result:= Trans_RatioY2GridRCoordinal(APercent);
end;



function TSpectrumDrawer.Trans_MarkY2Ratio(Mark: Single): Single;
begin
  Result:= FAxisesData.Left.TransMark2Ratio(
    Mark,
    FAxisesData.Left.ViewMax,
    FAxisesData.Left.ViewMin);
end;

function TSpectrumDrawer.Trans_RatioX2Mark(Percent: Single): Single;
begin
  Result:= FAxisesData.Bottom.TransRatio2Mark(
    FAxisesData.Bottom.ViewMin,
    FAxisesData.Bottom.ViewMax,
    Percent);
end;

function TSpectrumDrawer.Trans_RatioY2Mark(Percent: Single): Single;
begin
  Result:= FAxisesData.Left.TransRatio2Mark(
    FAxisesData.Left.ViewMax,
    FAxisesData.Left.ViewMin,
    Percent)
end;


function TSpectrumDrawer.Trans_RatioX2GridRCoordinal(RatioX: Single): Single;
begin
  Result := RatioX * FGraphicGridR.Width;// + FGraphicGridR.Left - 1
end;

function TSpectrumDrawer.Trans_RatioY2GridRCoordinal(RatioY: Single): Single;
begin
  Result:= RatioY * FGraphicGridR.Width;// + FGraphicGridR.Height - 1;
end;

procedure TSpectrumDrawer.UpdateGraphicRect;
begin
  With Chart do
  begin
    FGraphicRect := LocalRect;
    FGraphicRect.Top := FGraphicRect.Top + 2;
    FGraphicRect.Bottom := FGraphicRect.Bottom - 2;
  end;
end;

procedure TSpectrumDrawer.UpdateGridRAndStdTextR;
  function MeasureMaxRect(ValueFrom, ValueTo: Integer; Suffix: String): TRectF;
  var
    i: Integer;
    ARect: TRectF;
    Step: Integer;
  begin
    Step := 1;
    if abs(ValueTo - ValueFrom) + 1 > 500 then
    begin
      Step := Ceil((ValueTo - ValueFrom + 1) / 50)
    end;
    Result := TRectF.Empty;

    i := ValueFrom;
    ARect := FGraphicRect;
    while i < ValueTo do
    begin
      Chart.Canvas.MeasureText(ARect, IntToStr(i) + Suffix, False, [],
        TTextAlign.Leading, TTextAlign.Leading);
      Result.Union(ARect);
      Inc(i, Step);
    end;

    ARect := Chart.LocalRect;
    Chart.Canvas.MeasureText(ARect, IntToStr(ValueTo) + Suffix, False, [],
      TTextAlign.Leading, TTextAlign.Leading);
    Result.Union(ARect);
  end;

begin
  With FAxisesData.Left do
    FLeftTextR := MeasureMaxRect(FViewMin, FViewMax, UnitStr);

  With FAxisesData.Bottom do
    FBottomTextR := MeasureMaxRect(FViewMin, FViewMax, UnitStr);

  FGraphicGridR := FGraphicRect;
  FGraphicGridR.Top := FGraphicGridR.Top + FLeftTextR.Height * 0.5;
  FGraphicGridR.Left := FGraphicGridR.Left + FLeftTextR.Width * 1.2;
  FGraphicGridR.Bottom := FGraphicGridR.Bottom - FLeftTextR.Height * 0.5 -
    FBottomTextR.Height;
  FGraphicGridR.Right := FGraphicGridR.Right - FBottomTextR.Width * 0.5;
  // ===========================================================================
  ResettleSelections();
end;

procedure TSpectrumDrawer.UpdateViewRangeIndex(Axis: TCustomAxis);
var
  nCount: Integer;
begin
  nCount := Length(FData);
  // nCount := Axis.FViewMax - Axis.FViewMin + 1;
  Axis.FViewDataIdxFrom := Trunc(nCount * Axis.ViewRatioFrom);
  Axis.FViewDataIdxTo := Round((nCount - 1) * Axis.ViewRatioTo);
end;

function TSpectrumDrawer.CursorMarkX: Single;
begin
  Result:= Trans_RatioX2Mark(CursorXRatio)
end;

function TSpectrumDrawer.CursorMarkY: Single;
begin
  Result:= Trans_RatioY2Mark(CursorYRatio)
end;

function TSpectrumDrawer.CursorViewIndex: Single;
begin
  With FAxisesData.Bottom do
  begin
    Result := Round((FViewMax - FViewMin) * EnsureRange(CursorXRatio, 0, 1))
      + FViewMin;
  end;
end;

{ TSignalDrawer }

procedure TAbstractSignalDrawer.Internal_SetChart(const Value: TSignalChart);
begin
  FChart := Value;
end;

procedure TAbstractSignalDrawer.SetChart(const Value: TSignalChart);
var
  LastChart: TSignalChart;
begin
  LastChart := FChart;
  if FChart <> Value then
  begin
    Self.Internal_SetChart(Value);

    if LastChart <> Nil then
    begin
      if LastChart.Drawer <> Nil then
        LastChart.Drawer := Nil;
    end;
  end;
  if FChart <> Nil then
    if FChart.Drawer <> Self then
      FChart.Drawer := Self;

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

constructor TWaterFallDrawer.Create(AOwner: TComponent);
begin
  inherited;
  FColorBarGradient := TGradient.Create;
  FWaterFallBuf := TBitmap.Create(1, 1);
  FColorBar := TColorBar.Create(Self);
  FColorBar.SetSize(1, 1);
  FColorBar.OnClick := HandleColorBarClickProc;

  InitGradientRainBow(FColorBarGradient);
  FColorBar.InitColors(FColorBarGradient);
end;

destructor TWaterFallDrawer.Destroy;
begin
  FreeAndNil(FWaterFallBuf);
  FreeAndNil(FColorBarGradient);
  inherited;
end;

procedure TWaterFallDrawer.DockColorBar;
begin
  inherited;
  if FChart = Nil then
    Exit;
  FColorBar.Visible := FShowWaterfall;
  if FShowWaterfall then
  begin
    FColorBar.Width := 20;
    FColorBar.Height := FWaterFallGridR.Height;

    FColorBar.Bitmap.Width := Ceil(FColorBar.Width);
    FColorBar.Bitmap.Height := Ceil(FColorBar.Height);

    FColorBar.Position.X := FWaterFallGridR.TopLeft.X - 30;
    FColorBar.Position.Y := FWaterFallGridR.TopLeft.Y;

    FColorBar.UpdateBitmap();
  end
end;

procedure TWaterFallDrawer.HandleColorBarClickProc(Sender: TObject);
begin
  if Assigned(Self.FOnColorBarClick) then
    FOnColorBarClick(Sender);
end;

procedure TWaterFallDrawer.AfterChangeSize;
begin
  DockColorBar();
end;

procedure TWaterFallDrawer.DrawCross(X, Y: Single);
begin
  inherited;
  if FShowWaterfall then
  begin
    With Chart do
    begin
      Canvas.Stroke.Color := TAlphaColors.White;
      Canvas.DrawLine(TPointF.Create(FCrossX, FWaterFallGridR.Top),
        TPointF.Create(FCrossX, FWaterFallGridR.Bottom), CrossOpacity);
    end;
  end;
end;

procedure TWaterFallDrawer.DrawHint;
begin
  Chart.FHint := Chart.FHint + Format(#$D#$A'Bitmap Buf Start: %d',
    [FWaterFallBmpStart]);
  inherited;
end;

procedure TWaterFallDrawer.Internal_DrawGraphicBound(ACanvas: TCanvas);
const
  CONST_FRAME_COLOR: TAlphaColor = TAlphaColors.Red;
  CONST_FRAME_THICKNESS: Single = 2;
begin
  inherited;
  if Self.FShowWaterfall then
  begin
    ACanvas.Stroke.Dash := TStrokeDash.Dash;
    ACanvas.Stroke.Thickness := CONST_FRAME_THICKNESS;
    ACanvas.Stroke.Color := CONST_FRAME_COLOR;
    ACanvas.DrawRect(FWaterFallRect, 0, 0, [], 1);
  end;
end;

procedure TWaterFallDrawer.Internal_DrawGraphicFrame(ACanvas: TCanvas);
var
  GridFrameR: TRectF;
begin
  inherited;
  if Self.FShowWaterfall then
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
end;

procedure TWaterFallDrawer.Internal_DrawViewData(const AData: TArray<Single>;
ViewIdxFrom, ViewIdxTo: Integer);
  Procedure DrawCurrLine(const YPos: Integer);
  var
    i: Integer;
    PointStart: TPointF;
    PointEnd: TPointF;
    ViewStart, ViewEnd: TPointF;
    AColor: TAlphaColor;
    HStep: Single;
    ViewCount: Integer;
    Clip: TClipRects;
  begin
    if FWaterFallBuf.HandleAllocated then
      With FWaterFallBuf.Canvas do
      begin
        FWaterFallBuf.Canvas.Stroke.Kind := TBrushKind.Solid;
        FWaterFallBuf.Canvas.Fill.Kind := TBrushKind.Solid;

        PointStart := TPointF.Create(0, YPos);

        Insert(FWaterFallGridR, Clip, 0);
        Clip[0].Inflate(0, 2);
        ViewCount := ViewIdxTo - ViewIdxFrom + 1;
        // if BeginScene(@Clip, 0) then
        if BeginScene() then
          try
            HStep := FWaterFallGridR.Width / (ViewCount - 1);

            for i := 0 to ViewCount - 1 do
            begin
              if i > 0 then
                PointStart := PointEnd;

              PointEnd := PointStart;

              PointEnd.X := PointEnd.X + HStep;

              // ColorIndex :=
              // Trunc(AData[i + ViewIdxFrom] * Length(FRainBowColors));
              // if ColorIndex > Length(FRainBowColors) - 1 then
              // AColor := TAlphaColors.White
              // else
              // AColor := FRainBowColors[ColorIndex];
              AColor := FColorBar.InterpolateColor(AData[i + ViewIdxFrom]);

              Stroke.Color := AColor;

              ViewStart := PointStart;
              ViewEnd := PointEnd;
              ViewStart.offset(-HStep / 2, 0);
              ViewEnd.offset(-HStep / 2, 0);
              if ViewStart.X < 0 then
                ViewStart.X := 0;

              // DrawLine(PointStart, PointEnd, 1);
              DrawLine(ViewStart, ViewEnd, 1);

            end;
          finally
            EndScene();
          end;
      end;
  end;

var
  i: Integer;
  BmpData: TBitmapData;
  MoveBytes: Integer;
  BMP_TOP_INDEX: Integer;
  // MovedH: Integer;
  offset: Integer;
begin
  // if Not FWaterFallBmp.HandleAllocated then Exit;
  // GPU方式和Direct2D方式，两种情况下面BITMAP的TOP座标似乎是不同的，
  // GPU方式顶坐标从1开始
  if GlobalUseGPUCanvas then
    BMP_TOP_INDEX := 1
  else
    BMP_TOP_INDEX := 0;

  if FShowWaterfall then
    With Chart do
    begin
      if FWaterFallRectUpdated then
      begin
        FWaterFallBuf.Width := Ceil(FWaterFallGridR.Width);
        if Not FLargeBuf then
        begin
          FWaterFallBuf.Height := Ceil(FWaterFallGridR.Height);
        end;
        FWaterFallRectUpdated := False;
      end;

      // -----------------
      if FLargeBuf then
      begin
        if FWaterFallBmpStart < BMP_TOP_INDEX then
          with FWaterFallBuf do
          begin
            // MovedH := Ceil(FWaterFallGridR.Height);
            if Map(TMapAccess.ReadWrite, BmpData) then
            begin
              try
                MoveBytes := Width * BmpData.BytesPerPixel;
                offset := Ceil(FWaterFallBuf.Height -
                  FWaterFallGridR.Height + 1);
                // 移动图像向下一像素 ,要每行移动，不要多行同时移动，否则会出错，
                // 而且对速度性能益处有限
                for i := (Ceil(FWaterFallGridR.Height) - 2) Downto 0 do
                begin // 移动Height-1行到底部,舍掉最下一行
                  System.Move(BmpData.GetPixelAddr(0, BMP_TOP_INDEX + i)^,
                    BmpData.GetPixelAddr(0, i + offset)^, MoveBytes);
                end;
                // FWaterFallBmpStart := Offset - 1;
                // FWaterFallBmpStart := Offset - 1 + BMP_TOP_INDEX;
                FWaterFallBmpStart := offset;

                FWaterFallBuf.Canvas.Stroke.Kind := TBrushKind.Solid;
                FWaterFallBuf.Canvas.Fill.Kind := TBrushKind.Solid;
              finally
                Unmap(BmpData);
              end;
            end;
          end;

        if FWaterFallBuf.HandleAllocated then
        begin
          With FWaterFallBuf.Canvas do
          begin
            if BeginScene(Nil) then
              try
                DrawCurrLine(FWaterFallBmpStart);
                Dec(FWaterFallBmpStart);
              finally
                EndScene();
              end;
          end;
        end;

        Canvas.DrawBitmap(FWaterFallBuf, TRectF.Create(0, FWaterFallBmpStart,
          FWaterFallGridR.Width, FWaterFallGridR.Height + FWaterFallBmpStart),
          FWaterFallGridR, 1, False);
        // FWaterFallBuf.SaveToFile('d:\1.png');
      end
      else
      begin
        with FWaterFallBuf do
        begin
          if Map(TMapAccess.ReadWrite, BmpData) then
          begin
            try
              // 移动图像向下一像素 ,要每行移动，不要多行同时移动，
              // 否则有改变大小时会出错，而且对速度性能益处有限
              MoveBytes := Width * BmpData.BytesPerPixel;
              for i := Height - 2 Downto 0 do
                System.Move(BmpData.GetPixelAddr(0, 0 + i)^,
                  BmpData.GetPixelAddr(0, 1 + i)^, MoveBytes);

              Canvas.Stroke.Kind := TBrushKind.Solid;
              Canvas.Fill.Kind := TBrushKind.Solid;
            finally
              Unmap(BmpData);
            end;
          end;
        end;
        DrawCurrLine(BMP_TOP_INDEX);
        Canvas.DrawBitmap(FWaterFallBuf, FWaterFallBuf.Bounds,
          FWaterFallGridR, 1, True);
      end;
    end;
  inherited;
end;

procedure TWaterFallDrawer.InitGradientRainBow(const AGradient: TGradient);
  Procedure AddGradientPoint(AGradient: TGradient; Aoffset, AR, AG, AB: Single);
  var
    pt: TGradientPoint;
  begin
    With AGradient do
    begin
      pt := TGradientPoint(Points.Add);
      pt.offset := Aoffset;
      pt.Color := TAlphaColorF.Create(AR, AG, AB).ToAlphaColor;
    end;
  end;

begin
  With AGradient do
  begin
    // wavelen			                  R	  G	  B
    // 380	    0	      0	            1	  0	  1
    // 440	    60	    0.226415094	  0	  0	  1
    // 490	    110	    0.41509434	  0	  1	  1
    // 510	    130	    0.490566038	  0	  1	  0
    // 580	    200	    0.754716981	  1	  1	  0
    // 645	    265	    1	            1	  0	  0
    Points.BeginUpdate;
    try
      Points.Clear;
      AddGradientPoint(AGradient, 0, 0, 0, 0);
      AddGradientPoint(AGradient, 0.05, 1, 0, 1);
      AddGradientPoint(AGradient, 0.226, 0, 0, 1);
      AddGradientPoint(AGradient, 0.415, 0, 1, 1);
      AddGradientPoint(AGradient, 0.491, 0, 1, 0);
      AddGradientPoint(AGradient, 0.755, 1, 1, 0);
      AddGradientPoint(AGradient, 0.800, 1, 0, 0);
      AddGradientPoint(AGradient, 0.950, 1, 1, 1);
    finally
      Points.EndUpdate();
    end;
    Change;
  end;
end;

procedure TWaterFallDrawer.SetChart(const Value: TSignalChart);
begin
  inherited;
  if Not(csDesigning in ComponentState) then
  begin
    if FColorBar <> Nil then
      FColorBar.Parent := Value;
  end;
end;

procedure TWaterFallDrawer.SetColorBarGradient(const Value: TGradient);
begin
  FColorBarGradient.Assign(Value);
  FColorBar.InitColors(FColorBarGradient);
end;

procedure TWaterFallDrawer.SetLargeBuf(const Value: Boolean);
begin
  if FLargeBuf <> Value then
  begin
    FWaterFallBufChanging := True;
    try
      FLargeBuf := Value;
      if FLargeBuf then
      begin
        FWaterFallBuf.Height := FWaterFallBuf.Canvas.GetAttribute
          (TCanvasAttribute.MaxBitmapSize);
        FWaterFallBmpStart := FWaterFallBuf.Height - 1;
      end
      else
      begin
        FWaterFallBuf.Height := Ceil(FWaterFallGridR.Height);
        FWaterFallBmpStart := 0;
      end;
    finally
      FWaterFallBufChanging := False;
    end;
  end;

end;

procedure TWaterFallDrawer.SetOnColorBarClick(const Value: TNotifyEvent);
begin
  FOnColorBarClick := Value;
end;

procedure TWaterFallDrawer.SetShowWaterfall(const Value: Boolean);
begin
  if FShowWaterfall <> Value then
  begin
    FShowWaterfall := Value;

    if Not FShowWaterfall then
    begin
      FWaterFallBuf.SetSize(1, 1); // 节省内存开销
    end;
    if Chart <> Nil then
    begin
      UpdateGraphicRect();
      Chart.UpdateBitmap;
      Chart.InvalidateRect(Chart.LocalRect);
      AfterChangeSize(); // 重新定位彩条图像
    end
    else
    begin

    end;

  end;
end;

procedure TWaterFallDrawer.UpdateGraphicRect;
begin
  if FShowWaterfall then
    With Chart do
    begin
      FGraphicRect := LocalRect;
      FGraphicRect.Top := FGraphicRect.Top + 2;
      FGraphicRect.Bottom := FGraphicRect.Bottom / 2;
      FWaterFallRect := LocalRect;
      FWaterFallRect.Top := FGraphicRect.Bottom;
      FWaterFallRect.Bottom := LocalRect.Bottom - 2;
    end
  else
  begin
    inherited;
  end;
end;

procedure TWaterFallDrawer.UpdateGridRAndStdTextR;
begin
  inherited;
  if FShowWaterfall then
    With Chart do
    begin
      FWaterFallGridR := FGraphicGridR;
      FWaterFallGridR.Top := FWaterFallRect.Top;
      FWaterFallGridR.Bottom := FWaterFallRect.Bottom - FBottomTextR.Bottom / 2;
      FWaterFallRectUpdated := True;
    end;
end;

procedure TAbstractSignalDrawer.CheckUpdateBackGround;
begin
  if Chart <> Nil then
  begin
    if Chart.FNeedUpdateBG and (ComponentState * [csLoading, csReading] = [])
    then
    begin
      UpdateGridRAndStdTextR();
      Chart.UpdateBitmap;
      AfterChangeSize();
      Chart.FNeedUpdateBG := False;
    end;
  end;
end;

{ TWaterFallDrawer.TColorBar }

constructor TWaterFallDrawer.TColorBar.Create(AOwner: TComponent);
begin
  inherited;
  FColorsLen := 1000 + 1;
end;

procedure TWaterFallDrawer.TColorBar.InitColors(Gradient: TGradient);
var
  i: Integer;
  AColor: TAlphaColor;
begin
  SetLength(FColors, 0);
  for i := 0 to FColorsLen - 1 do
  begin
    AColor := Gradient.InterpolateColor(i / (FColorsLen - 1));
    Insert(AColor, FColors, Length(FColors));
  end;
  UpdateBitmap();
end;

function TWaterFallDrawer.TColorBar.InterpolateColor(offset: Single)
  : TAlphaColor;
begin
  offset := EnsureRange(offset, 0, 1);
  Result := FColors[Round(offset * (FColorsLen - 1))];
end;

procedure TWaterFallDrawer.TColorBar.UpdateBitmap;
var
  i: Integer;
  Step: Single;
  ColorRect: TRectF;
begin
  if (FColorsLen > 0) and (Bitmap.HandleAllocated) then
  begin
    Step := Bitmap.Height / FColorsLen;
    With Bitmap.Canvas do
    begin
//      Stroke.Kind:= TBrushKind.Solid;
//      Stroke.Color:= TAlphaColorRec.Black;

      Fill.Kind := TBrushKind.Solid;
      if BeginScene(Nil, 0) then
        try
          ColorRect := TRectF.Create(0, 0, Bitmap.Width, Step);
          for i := Length(FColors) - 1 Downto 0 do
          begin
            Fill.Color := FColors[i];
            FillRect(ColorRect, 0, 0, [], 1);
            ColorRect.offset(0, Step);
          end;
//           ColorRect := TRectF.Create(0, 1, Bitmap.Width, Bitmap.Height);
//          ColorRect.Inflate(-1, -1);
//          DrawRect(ColorRect, 0, 0, [], 1);
//          FillRect(LocalRect, 0, 0, [], 1);

        finally
          EndScene();
        end;
    end;
  end;
end;


{ TSelectionData }


constructor TAxisSelection.Create(Axis: TCustomAxis);
begin
  inherited Create;
  FAxis:= Axis;
  FUI:= TSelectionUI2.Create(Nil);
  FUI.Tag:= NativeInt(Self);
  self.DoAnchorChange;
end;

destructor TAxisSelection.Destroy;
begin
  FUI.Free;
  inherited;
end;

procedure TAxisSelection.DoAnchorChange;
var
  ADrawer: TSpectrumDrawer;
  AChart: TSignalChart;
  ARightPos: Single;
begin
  GetDrawerAndChart(ADrawer, AChart);
  if FUI.Parent <> AChart then
  begin
    FUI.Parent:= AChart;
  end;

  if AChart <> Nil then
  begin
    ARightPos:= ADrawer.Trans_MarkX2GridRCoordinal(FAnchor.Right) + ADrawer.FGraphicGridR.Left;
    FUI.Position.X:= ADrawer.Trans_MarkX2GridRCoordinal(FAnchor.Left) + ADrawer.FGraphicGridR.Left;
    FUI.Width:= ARightPos - FUI.Position.X;
    CheckUIBound();
  end;
end;

function TAxisSelection.GetAnchorLeft: Single;
begin
  Result:= FAnchor.Left;
end;

function TAxisSelection.GetAnchorRatioLeft: Single;
var
  ADrawer: TSpectrumDrawer;
  AChart: TSignalChart;
begin
  GetDrawerAndChart(ADrawer, AChart);
  Result:= ADrawer.Trans_MarkX2Ratio(FAnchor.Left);
end;

function TAxisSelection.GetAnchorRatioRight: Single;
var
  ADrawer: TSpectrumDrawer;
  AChart: TSignalChart;
begin
  GetDrawerAndChart(ADrawer, AChart);
  Result:= ADrawer.Trans_MarkX2Ratio(FAnchor.Right);
end;

function TAxisSelection.GetAnchorRight: Single;
begin
  Result:= FAnchor.Right;
end;

function TAxisSelection.GetDrawerAndChart(out Drawer: TSpectrumDrawer;
  out Chart: TSignalChart): Boolean;
begin
  Result := False;

  Chart := Nil;
  Drawer := Nil;

  if (FAxis <> Nil) and (FAxis.FDrawer <> Nil) then
  begin
    Drawer:= FAxis.FDrawer as TSpectrumDrawer;
    Chart:= Drawer.FChart;
    Result:= True;
  end;
end;


procedure TAxisSelection.SetAnchorLeft(const Value: Single);
var
  NewValue: Single;
begin
  if FAxis <> Nil then
  begin
    NewValue:= EnsureRange(Value, FAxis.FMin, Min(FAxis.FMax, FAnchor.Right));
    if FAnchor.Left <> NewValue then
    begin
      FAnchor.Left:= NewValue;
      DoAnchorChange();
    end;
  end;
end;


procedure TAxisSelection.SetAnchorRatioLeft(const Value: Single);
var
  ADrawer: TSpectrumDrawer;
  AChart: TSignalChart;
begin
  GetDrawerAndChart(ADrawer, AChart);
  AnchorLeft:= ADrawer.Trans_RatioX2Mark(Value);
  //FUI.Position.X:= ADrawer.Trans_RatioX2GridRCoordinal(Value) + ADrawer.FGraphicGridR.Left;
end;

procedure TAxisSelection.SetAnchorRatioRight(const Value: Single);
var
  ADrawer: TSpectrumDrawer;
  AChart: TSignalChart;
begin
  GetDrawerAndChart(ADrawer, AChart);
  AnchorRight:= ADrawer.Trans_RatioX2Mark(Value);
end;

procedure TAxisSelection.SetAnchorRight(const Value: Single);
var
  NewValue: Single;
begin
  if FAxis <> Nil then
  begin
    NewValue:= EnsureRange(Value,  Max(FAxis.FMin, FAnchor.Left), FAxis.FMax);
    if FAnchor.Right <> NewValue then
    begin
      FAnchor.Right:= NewValue;
      DoAnchorChange();
    end;
  end;
end;


function TAxisSelection.TransAnchorToChart(AAnchor: TSelectionAnchor; var X1,
  X2: Single): Boolean;
begin
  Result:= False;

end;

function TAxisSelection.TransChartToAnchor(X1, X2: Single;
  var AAnchor: TSelectionAnchor): Boolean;
begin
  Result:= False;
end;


procedure TAxisSelection.CheckUIBound;
var
  ADrawer: TSpectrumDrawer;
  AChart: TSignalChart;
begin
  if GetDrawerAndChart(ADrawer, AChart) then
  begin
    // fix the selection position

    FUI.Position.Y := ADrawer.FGraphicGridR.Top;
    FUI.Height:= ADrawer.FGraphicGridR.Height;

    FUI.Position.X:= EnsureRange(FUI.Position.X, ADrawer.FGraphicGridR.Left, ADrawer.FGraphicGridR.Right - FUI.Width);



//    //Update L, R Mark
//    AAxisSelection.FAnchor.Left:= ADrawer.Trans_GridRCoordinalX2Mark(Position.X);
//    AAxisSelection.FAnchor.Right:= ADrawer.Trans_GridRCoordinalX2Mark(Position.X + Width);
  end;
end;

{ TSelectionManager }

function TSelectionManager.AddSelection: TAxisSelection;
begin
  Result:= TAxisSelection.Create(FAxis);
  Add(Result);
end;

{ TAxisSelection.TSelection2 }

constructor TAxisSelection.TSelectionUI2.Create(AOwner: TComponent);
begin
  inherited;
  Handles:= [LeftCenter, RightCenter];
end;

destructor TAxisSelection.TSelectionUI2.Destroy;
begin

  inherited;
end;

procedure TAxisSelection.TSelectionUI2.DoTrack;
var
  AAxisSelection: TAxisSelection;
  ADrawer: TSpectrumDrawer;
  AChart: TSignalChart;
begin
  AAxisSelection:= TAxisSelection(self.Tag);

  if AAxisSelection.GetDrawerAndChart(ADrawer, AChart) then
  begin
    AAxisSelection.CheckUIBound();

    //Update L, R Mark
    AAxisSelection.FAnchor.Left:= ADrawer.Trans_GridRCoordinalX2Mark(Position.X);
    AAxisSelection.FAnchor.Right:= ADrawer.Trans_GridRCoordinalX2Mark(Position.X + Width);
  end;
  inherited;
end;


procedure TAxisSelection.TSelectionUI2.MouseMove(Shift: TShiftState; X, Y: Single);
var
  AAxisSelection: TAxisSelection;
  AChart: TSignalChart;
  ADrawer: TSpectrumDrawer;
  px: TPointF;
begin
  AAxisSelection:= TAxisSelection(Tag);
  if AAxisSelection.GetDrawerAndChart(ADrawer, AChart) then
  begin
    px := Self.LocalToScreen(TPointF.Create(X, Y));
    px := AChart.ScreenToLocal(px);
    AChart.MouseMove(Shift, px.X, px.Y);
    AChart.InvalidateRect(ADrawer.FGraphicGridR);
  end;
  inherited;
end;


procedure TAxisSelection.TSelectionUI2.MoveHandle(AX, AY: Single);
var
  ASelection: TAxisSelection;
  ADrawer: TSpectrumDrawer;
  AChart: TSignalChart;
  OldWidth: Single;
  ///OldPosition: TPosition;
begin
  OldWidth:= Width;
  ///OldPosition:= Position;

  inherited;

  ASelection:= TAxisSelection(self.Tag);
  if ASelection.GetDrawerAndChart(ADrawer, AChart) then
  begin
    if Position.X < ADrawer.FGraphicGridR.Left then
    begin
      Position.X:= ADrawer.FGraphicGridR.Left;
      Width:= OldWidth;
    end;
    if Position.X + Width > ADrawer.FGraphicGridR.Right then
    begin
      Width:= OldWidth;
      Position.X:= ADrawer.FGraphicGridR.Right - OldWidth;
    end;
  end;
end;

initialization

GlobalUseGPUCanvas := True;

// GlobalUseDX10Software:= True;
// GlobalUseDirect2D:= True;
end.
