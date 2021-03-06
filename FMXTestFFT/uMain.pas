unit uMain;
{.$DEFINE ANDROID}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Math,
  System.DateUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Effects,
  FMX.Filter.Effects, FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Layouts, FMX.ExtCtrls, uRadioSpectrumChart,  FMX.PlatForm,
  {$IFDEF MSWINDOWS}
  Bass,
  {$ENDIF}
  FMX.Edit, FMX.EditBox,
  FMX.SpinBox, uSpectrumSelection, FMX.Ani;

type

  TForm3 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Panel1: TPanel;
    sbMax: TSpinBox;
    sbMin: TSpinBox;
    SignalChart1: TViewPanSignalChart;
    SignalRectangeDrawer1: TSpectrumDrawer;
    SplitedDrawer1: TWaterFallDrawer;
    Test1: TTest;
    Timer1: TTimer;
    Switch1: TSwitch;
    btnZoomOut: TButton;
    rbZoomIn: TRadioButton;
    rbZoomOut: TRadioButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Switch2: TSwitch;
    Timer2: TTimer;
    swL: TSwitch;
    swC: TSwitch;
    swR: TSwitch;
    BitmapAnimation1: TBitmapAnimation;
    procedure Button3Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure SignalChart1Resized(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormCreate(Sender: TObject);
    procedure Panel1Paint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure SignalChart1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Single);
    procedure SignalChart1MouseEnter(Sender: TObject);
    procedure SignalChart1MouseLeave(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Switch1Switch(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure SignalChart1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure Button9Click(Sender: TObject);
    procedure SpectrumSelection1Track(Sender: TObject);
    procedure SplitedDrawer1ColorBarClick(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Switch2Switch(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure swLSwitch(Sender: TObject);
  private
    { Private declarations }
    FData: TArray<Single>;
    FStop: Boolean;
    Procedure InitRainbow();
    Procedure InitSpectralColors();
    Procedure InitSpectralColors2();
    Procedure InitSpectralColors3();
  Private
    {$IFDEF MSWINDOWS}
    hs: HSTREAM;
    {$ENDIF}
    FFTData: TArray<Single>;
    FColors: TArray<TAlphaColor>;
    FRainbowColors: TArray<TAlphaColor>;
    FSpectralColors: TArray<TAlphaColor>;
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

uses
  SpectraLibrary, FMX.Printer{$IFDEF MSWINDOWS}, CnDebug, FMX.Design.Brush{$ENDIF},
  ufrmMask, uFrmLog;

function MakeScaleScreenshot(Sender: TControl): TBitmap; Overload;
var
  fScreenScale: Single;
  function GetScreenScale: Single;
  var
    ScreenService: IFMXScreenService;
  begin
    Result := 1;
    if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService,
      IInterface(ScreenService)) then
    begin
      Result := ScreenService.GetScreenScale;
    end;
  end;

begin
  fScreenScale := GetScreenScale;
  Result := TBitmap.Create(Round(Sender.Width * fScreenScale),
    Round(Sender.Height * fScreenScale));
  Result.Clear(0);
  if Result.Canvas.BeginScene then
    try
      Sender.PaintTo(Result.Canvas, RectF(0, 0, Result.Width, Result.Height));
    finally
      Result.Canvas.EndScene;
    end;
end;

function MakeScaleScreenshot(Sender: TForm): TBitmap; Overload;
var
  fScreenScale: Single;
  function GetScreenScale: Single;
  var
    ScreenService: IFMXScreenService;
  begin
    Result := 1;
    if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService,
      IInterface(ScreenService)) then
    begin
      Result := ScreenService.GetScreenScale;
    end;
  end;

begin
  fScreenScale := GetScreenScale;
  Result := TBitmap.Create(Round(Sender.Width * fScreenScale),
    Round(Sender.Height * fScreenScale));
  Result.Clear(0);
  if Result.Canvas.BeginScene then
    try
//      Sender.PaintTo(Result.Canvas, RectF(0, 0, Result.Width, Result.Height));
      Sender.PaintTo(Result.Canvas);
    finally
      Result.Canvas.EndScene;
    end;
end;
// Procedure spectrum_to_rgb(wavelength: double; r, g, b: PDouble); stdcall;
// external 'specrum32.dll' name '_spectrum_to_rgb@20';

Procedure spectral_color(var r, g, b: double; L: double);
// RGB <0,1> <- lambda l <400,700> [nm]
var
  t: double;
begin
  r := 0;
  g := 0;
  b := 0;
  if ((L >= 400.0) and (L < 410.0)) then
  begin
    t := (L - 400.0) / (410.0 - 400.0);
    r := +(0.33 * t) - (0.20 * t * t);
  end
  else if ((L >= 410.0) and (L < 475.0)) then
  begin
    t := (L - 410.0) / (475.0 - 410.0);
    r := 0.14 - (0.13 * t * t);
  end
  else if ((L >= 545.0) and (L < 595.0)) then
  begin
    t := (L - 545.0) / (595.0 - 545.0);
    r := +(1.98 * t) - (t * t);
  end
  else if ((L >= 595.0) and (L < 650.0)) then
  begin
    t := (L - 595.0) / (650.0 - 595.0);
    r := 0.98 + (0.06 * t) - (0.40 * t * t);
  end
  else if ((L >= 650.0) and (L < 700.0)) then
  begin
    t := (L - 650.0) / (700.0 - 650.0);
    r := 0.65 - (0.84 * t) + (0.20 * t * t);
  end;

  if ((L >= 415.0) and (L < 475.0)) then
  begin
    t := (L - 415.0) / (475.0 - 415.0);
    g := +(0.80 * t * t);
  end
  else if ((L >= 475.0) and (L < 590.0)) then
  begin
    t := (L - 475.0) / (590.0 - 475.0);
    g := 0.8 + (0.76 * t) - (0.80 * t * t);
  end
  else if ((L >= 585.0) and (L < 639.0)) then
  begin
    t := (L - 585.0) / (639.0 - 585.0);
    g := 0.84 - (0.84 * t);
  end;

  if ((L >= 400.0) and (L < 475.0)) then
  begin
    t := (L - 400.0) / (475.0 - 400.0);
    b := +(2.20 * t) - (1.50 * t * t);
  end
  else if ((L >= 475.0) and (L < 560.0)) then
  begin
    t := (L - 475.0) / (560.0 - 475.0);
    b := 0.7 - (t) + (0.30 * t * t);
  end
end;

procedure TForm3.btnZoomOutClick(Sender: TObject);
begin
  if rbZoomIn.IsChecked then
  begin
    self.SplitedDrawer1.AxisesData.Bottom.Zoom(0.9, 60);
  end
  else if rbZoomOut.IsChecked then
  begin
    self.SplitedDrawer1.AxisesData.Bottom.Zoom(1.1, 60);
  end;
end;

procedure TForm3.Button10Click(Sender: TObject);
begin
  self.SplitedDrawer1.AddSelectionViaPercent(0.4, 0.6);
end;

procedure TForm3.Button11Click(Sender: TObject);
begin
  self.SplitedDrawer1.ClearSection();
end;

procedure TForm3.Button12Click(Sender: TObject);
  function GetObjectInfo(const obj: TFmxObject): String;
  var
    c: TControl;
  begin
    if obj = NIl then
    begin
      Result:= 'object is Nil'
    end
    else
    begin
      Result:= Result + obj.ToString();

//      Result:= Format('object:{name: %s, ClassName: %s',
//          [obj.Name, obj.ClassName]);
//      if obj is TControl then
//      begin
//        c:= TControl(c);
//        Result:= Result + ', IsFocused: ' + c.IsFocused.ToString(True);
//      end;
      Result:= Result + '}';
    end;
  end;
var
  s: String;
  ARoot: IRoot;
  obj: TFmxObject;
  i: Integer;
  ASele: TAxisSelection;
begin
  ARoot:= self.SignalChart1.Root;
  if (ARoot <> Nil) then
  begin
    obj:= ARoot.GetObject;
    s:= 'Root: '#$D#$A;
    s:= s + '     GetObject:'#$D#$A;
    s:= s + '           ' + GetObjectInfo(obj)+#$D#$A;
//      s:= 'Root of SignalChart is ' + Root.Name;
    s:= s + '     GetActiveControl:'#$D#$A;
    if ARoot.GetActiveControl = Nil then
    begin
      s:= s + '           ' + 'Nil'+#$D#$A;
    end
    else
    begin
      s:= s + '           ' + GetObjectInfo(Aroot.GetActiveControl.GetObject)+#$D#$A;
    end;
    s:= s + '     Captured:'#$D#$A;
    if ARoot.Captured = Nil then
    begin
      s:= s + '           ' + 'Nil'+#$D#$A;
    end
    else
    begin
      s:= s + '           ' + GetObjectInfo(Aroot.Captured.GetObject)+#$D#$A;
    end;
    s:= s + '     Focused:'#$D#$A;
    if ARoot.Focused = Nil then
    begin
      s:= s + '           ' + 'Nil'+#$D#$A;
    end
    else
    begin
      s:= s + '           ' + GetObjectInfo(Aroot.Focused.GetObject)+#$D#$A;
    end;
  end
  else
  begin
    s:= 'Root of SignalChart is Nil';
  end;
  frmLog.Label1.Text:= s;
  //
//  s:= '';
//  for ASele in SplitedDrawer1.AxisesData.Bottom.SelectionManager do
//  begin
////    s:= s + Format('(%.2f, %.2f, %.2f)', [ASele.AnchorLeft, (ASele.AnchorLeft + ASele.AnchorRight) / 2, ASele.AnchorRight]);
//    s:= s + '1';
////    s:= s + #$D#$A;
////    s:= DateTimeToStr(Now );// + #$D#$A;
//  end;

  if SplitedDrawer1.AxisesData.Bottom.SelectionManager.Count > 0 then
  begin
    s:= '';
    for i := 0 to SplitedDrawer1.AxisesData.Bottom.SelectionManager.Count - 1 do
    begin
      ASele:= SplitedDrawer1.AxisesData.Bottom.SelectionManager[i];
      s:= s + Format('(%.2f, %.2f, %.2f)'#$D#$A, [ASele.AnchorLeft, (ASele.AnchorLeft + ASele.AnchorRight) / 2, ASele.AnchorRight]);
    end;
  end
  else
  begin
    s:= ' ';
  end;

  frmLog.Label2.Text:= s;
  frmLog.Label2.Change;
  frmLog.Memo1.Text:= frmLog.Label2.Text;
  frmLog.Memo1.Lines.Add(
    frmLog.Label2.BoundsRect.Width.ToString + ', ' + frmLog.Label2.BoundsRect.Height.ToString);
  //frmLog.Label2.Text:= '123';
end;

procedure TForm3.Button1Click(Sender: TObject);
begin
  FStop := True;
  {$IFDEF MSWINDOWS}
  BASS_Stop();
  {$ENDIF}
  Timer1.Enabled := False;
end;

procedure TForm3.Button2Click(Sender: TObject);
var
  X: TBitmap;
begin

//  SplitedDrawer1.AxisesData.Left.MinValue :=
//    SplitedDrawer1.AxisesData.Left.MinValue - 1;
  SignalChart1.InvalidateRect(SignalChart1.ClipRect);

  SignalChart1.Repaint;
  // RadioSpectrumChart1.InvalidateRect(RadioSpectrumChart1.BoundsRect);
  X := TBitmap.Create;
  try
    X.Width := 100;
    X.Height := 100;
    X.Canvas.Stroke.Color := TAlphaColors.Red;
    X.Canvas.Stroke.Kind := TBrushKind.Solid;
    X.Canvas.DrawLine(TPointF.Create(0, 0), TPointF.Create(100, 100), 1);
    X.SaveToFile('d:\1.png');
  finally
    X.Free;
  end;
end;

procedure TForm3.Button3Click(Sender: TObject);
{$IFDEF MSWINDOWS}
const
  // SoneFile: AnsiString = 'C:\Users\mei\Desktop\AUDIO\song1.wav';
  SoneFile: AnsiString =
    'C:\Users\mei\Desktop\AUDIO\铁血丹心 (1997 Digital Remaster)_罗文_罗文 - Master Sonic.mp3';
  // SoneFile: AnsiString =
  // '.\铁血丹心 (1997 Digital Remaster)_罗文_罗文 - Master Sonic.mp3';
begin
  BASS_StreamFree(hs);
  hs := BASS_StreamCreateFile(False, PAnsiChar(SoneFile), 0, 0, 0);
  if hs < BASS_ERROR_ENDED then
    MessageDlg('打开失败', TmsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0)
  else
  begin
    // Text := string(Mp3Path);
    BASS_ChannelPlay(hs, False);
    Timer1.Enabled := True;
  end;
end;
{$ELSE}
begin
  Timer1.Enabled := True;
end;
{$ENDIF}

procedure TForm3.Button4Click(Sender: TObject);
var
  aForm: TForm;
begin
  // InvalidateRect(Bounds);
  // self.SignalChart1.InvalidateRect(SignalChart1.LocalRect);
  With MakeScaleScreenshot(Self) do
  begin
    SaveToFile('d:\1.png');
    DisposeOf;
  end;
//  aForm:= TForm.CreateNew(Self);
//  aForm.BorderStyle:= TFmxFormBorderStyle.None;
//  aForm.Transparency:= True;
//  aForm.WindowState:= TWindowState.wsMaximized;
//  aform.show;
//  With MakeScaleScreenshot(aform) do
//  begin
//    SaveToFile('d:\2.png');
//    DisposeOf;
//  end;
//  aform.DisposeOf;
end;

procedure TForm3.Button5Click(Sender: TObject);
begin
  SplitedDrawer1.ShowWaterfall:= Not SplitedDrawer1.ShowWaterfall;
end;

procedure TForm3.Button6Click(Sender: TObject);
begin
  SplitedDrawer1.AxisesData.Bottom.ViewMin:= 0;
  SplitedDrawer1.AxisesData.Bottom.ViewMax:= 11025;
  SplitedDrawer1.Chart.InvalidBackGround;
end;

procedure TForm3.Button7Click(Sender: TObject);
begin
  SplitedDrawer1.AxisesData.Bottom.ViewMin:= 0;
  SplitedDrawer1.AxisesData.Bottom.ViewMax:= 22050;
  SplitedDrawer1.Chart.InvalidBackGround;
end;

procedure TForm3.Button8Click(Sender: TObject);
begin
  SplitedDrawer1.AxisesData.Bottom.ViewMax:= Trunc(sbMax.Value);
  SplitedDrawer1.AxisesData.Bottom.ViewMin:= Trunc(sbMin.Value);

  SplitedDrawer1.Chart.InvalidBackGround;
end;

procedure TForm3.Button9Click(Sender: TObject);
begin
//  ShowGradientDialog(self.SplitedDrawer1.ColorBar.Fill.Gradient);
//  ShowBrushDialog(self.SplitedDrawer1.ColorBar.Fill,
//    [TBrushKind.Gradient,
//    TBrushKind.Solid,
//    TBrushKind.Bitmap]);
//  ShowGradientDialog(self.Fill.Gradient);

//  ShowBrushDialog(self.Rectangle1.Fill,
//    [TBrushKind.Gradient,
//    TBrushKind.Solid,
//    TBrushKind.Bitmap]);
//  ShowBrushDialog(self.Fill, [TBrushKind.Solid, TBrushKind.Gradient, TBrushKind.Resource]);
  frmMask.Show;
end;

constructor TForm3.Create(AOwner: TComponent);
begin
  inherited;

  SetLength(FFTData, 2048);
  {$IFDEF MSWINDOWS}
  if (BASS_GetVersion shr 16) <> BASSVERSION then
    MessageDlg('"Bass.dll" 文件版本不合适! ', TmsgDlgType.mtError,
      [TMsgDlgBtn.mbOK], 0);

  if not BASS_Init(-1, 44100, 0, 0, nil) then
    MessageDlg('初始化错误', TmsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  {$ENDIF}
end;

destructor TForm3.Destroy;
begin

  inherited;
end;

procedure TForm3.FormCreate(Sender: TObject);
var
  bmp: TBitmap;
  bs: TGradient;
BEGIN
  InitRainbow();
  InitSpectralColors3();
//  self.Fill.Color := TAlphaColors.Yellowgreen;
  self.Fill.Color := TAlphaColors.Black;
//  self.Fill.Color := TAlphaColors.Gray;
//  self.Fill.Kind := TBrushKind.Solid;
  self.SignalChart1.Drawer:= SplitedDrawer1;

  self.sbMin.Value:= SplitedDrawer1.AxisesData.Bottom.ViewMin;
  self.sbMax.Value:= SplitedDrawer1.AxisesData.Bottom.ViewMax;

end;

procedure TForm3.FormPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
  Procedure TestText;
  var
    ARect: TRectF;
  begin
    // ARect:= TRectF.Create(0, 0, 10, Canvas.TextHeight(' '));
    ARect := TRectF.Create(0, 0, 200, 200);
    Canvas.MeasureText(ARect, Caption, False, [], TTextAlign.Leading,
      TTextAlign.Leading);
    // Canvas.FillRect(ARect, 0, 0, [], 1);
    Canvas.FillText(ARect, Caption, False, 1, [], TTextAlign.Center,
      TTextAlign.Center);
    Canvas.Stroke.Kind := TBrushKind.Solid;
    Canvas.DrawRect(ARect, 0, 0, [], 1);
  end;

  procedure test();
  var
    X: TBitmap;
  begin
    // RadioSpectrumChart1.InvalidateRect(RadioSpectrumChart1.BoundsRect);
    X := TBitmap.Create;
    try
      X.Width := 100;
      X.Height := 100;
      // x.Canvas.Stroke.Color:= TAlphaColors.Red;
      // x.Canvas.Stroke.Kind:= TBrushKind.Solid;
      // x.Canvas.DrawLine(TPointF.Create(0, 0), TPointF.Create(100, 100), 1);
      // x.SaveToFile('d:\1.png');
      // x.LoadFromFile('C:\Users\Public\Pictures\Sample Pictures\202830_1383931710.png');

      X.Canvas.Stroke.Color := TAlphaColors.Red;
      X.Canvas.Stroke.Kind := TBrushKind.Solid;
      X.Canvas.Stroke.Thickness := 10;

      X.Canvas.Fill.Color := TAlphaColors.Lime;
      X.Canvas.Fill.Kind := TBrushKind.Solid;

      X.Canvas.FillRect(TRectF.Create(2, 2, 200, 200), 0, 0, [], 1);
      // x.Canvas.DrawRect(TRectF.Create(1, 1, 201, 201), 0, 0, [], 1);

      X.Canvas.BeginScene();
      X.Canvas.DrawLine(TPointF.Create(2, 2), TPointF.Create(200, 200), 1);

      // Canvas.Stroke.Color:= TAlphaColors.Lime;
      // Canvas.Stroke.Kind:= TBrushKind.Solid;
      // Canvas.DrawRect(TRectF.Create(2, 2, 100, 200), 0, 0, [], 1);
      X.Canvas.EndScene;
      Canvas.BeginScene();
      Canvas.DrawBitmap(X, X.Bounds, self.ClientRect, 1);
      Canvas.EndScene;

    finally
      X.Free;
    end;

  end;
  Procedure Test2;
  var
    s: TCanvasSaveState;
  begin
    s:= Canvas.SaveState;
    Canvas.Fill.Color:= TAlphaColorRec.Blue;
    Canvas.Fill.Kind:= TBrushKind.Solid;

    Canvas.FillRect(SignalChart1.BoundsRect, 0, 0, [], 1);
    Canvas.Fill.Color:= TAlphaColorRec.Lime;
    Canvas.FillEllipse(SignalChart1.BoundsRect, 1);

    Canvas.RestoreState(s);
  end;
begin
  // test;
//  TestText;
//  test2;

end;

procedure TForm3.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  StartWindowDrag;
end;

procedure TForm3.Panel1Paint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  i: Integer;
  pa, pb: TPoint;
begin
  Canvas.Stroke.Kind := TBrushKind.Solid;
  pa := TPoint.Create(0, 0);
  pb := TPoint.Create(0, 0);
  for i := 0 to Length(FColors) - 1 do
  begin
    pa.X := i;
    pb.X := i;
    pb.Y := Trunc(Panel1.Height);
    Panel1.Canvas.Stroke.Color := FColors[i];
    Panel1.Canvas.DrawLine(pa, pb, 1);
  end;
end;

procedure TForm3.SignalChart1MouseEnter(Sender: TObject);
begin
//  CnDebugger.LogMsg('MouseEnter');

end;

procedure TForm3.SignalChart1MouseLeave(Sender: TObject);
begin
//  CnDebugger.LogMsg('MouseLeave');
end;

procedure TForm3.SignalChart1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
begin
//  Exit;
//  CnDebugger.LogMsg('MouseMove' + Format('X: %.2f, Y: %.2f', [X, Y]));
end;

procedure TForm3.SignalChart1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  if SplitedDrawer1.CursorInGraphicGrid then
  begin
    if WheelDelta < 0 then
      SplitedDrawer1.AxisesData.Bottom.Zoom(abs(WheelDelta) / 100, SplitedDrawer1.CursorViewIndex)
    else  if WheelDelta > 0 then
      SplitedDrawer1.AxisesData.Bottom.Zoom(100 / abs(WheelDelta), SplitedDrawer1.CursorViewIndex);
  end;
end;

procedure TForm3.SignalChart1Resized(Sender: TObject);
begin
  // CnDebugger.LogMsg('Resized');
  self.Fill.Color
end;

procedure TForm3.SpectrumSelection1Track(Sender: TObject);
begin
//
end;

procedure TForm3.SplitedDrawer1ColorBarClick(Sender: TObject);
var
  AGradient: TGradient;
begin
  AGradient:= TGradient.Create;
  try
    AGradient.Assign(SplitedDrawer1.ColorBarGradient);
    {$IFDEF MSWINDOWS}
    ShowGradientDialog(AGradient);
    {$ENDIF}
    SplitedDrawer1.ColorBarGradient:= AGradient;
  finally
    AGradient.Free;
  end;

end;

procedure TForm3.Switch1Switch(Sender: TObject);
begin
  SplitedDrawer1.LargeBuf:= self.Switch1.IsChecked;
end;

procedure TForm3.Switch2Switch(Sender: TObject);
begin
  frmLog.Visible:= Switch2.IsChecked;
end;

procedure TForm3.swLSwitch(Sender: TObject);
var
  i: Integer;
  ASele: TAxisSelection;
begin
  if SplitedDrawer1.AxisesData.Bottom.SelectionManager.Count > 0 then
  begin
    for i := 0 to SplitedDrawer1.AxisesData.Bottom.SelectionManager.Count - 1 do
    begin
      ASele:= SplitedDrawer1.AxisesData.Bottom.SelectionManager[i];
      if Sender = swL then
        ASele.FUI.DontDrawLeftEdge:= TSwitch(Sender).IsChecked
      else if Sender = swC then
        ASele.FUI.DontDrawCenterLine:= TSwitch(Sender).IsChecked
      else if Sender = swR then
        ASele.FUI.DontDrawRightEdge:= TSwitch(Sender).IsChecked
//      ASele.Destroy:= s + Format('(%.2f, %.2f, %.2f)'#$D#$A, [ASele.AnchorLeft, (ASele.AnchorLeft + ASele.AnchorRight) / 2, ASele.AnchorRight]);
    end;
  end

end;

procedure TForm3.InitRainbow;
const
  RGBValues: array [0 .. 24 * 3 - 1] of byte = (182, 182, 255, 198, 182, 255,
    218, 182, 255, 234, 182, 255, 255, 182, 255, 255, 182, 234, 255, 182, 218,
    255, 182, 198, 255, 182, 182, 255, 198, 182, 255, 218, 182, 255, 234, 182,
    255, 255, 182, 234, 255, 182, 218, 255, 182, 198, 255, 182, 182, 255, 182,
    182, 255, 198, 182, 255, 218, 182, 255, 234, 182, 255, 255, 182, 234, 255,
    182, 218, 255, 182, 198, 255);
var
  i: Integer;
begin
  SetLength(FRainbowColors, 24);
  for i := 0 to Length(FRainbowColors) do
  begin
    With TAlphaColorRec(FRainbowColors[i]) do
    begin
      A := $FF;
      r := RGBValues[i * 3 + 0];
      g := RGBValues[i * 3 + 1];
      b := RGBValues[i * 3 + 2];
    end;
  end;
end;

procedure TForm3.InitSpectralColors;
var
  i: Integer;
  rr, gg, bb: double;
  AColor: TAlphaColor;
begin
  {$IFDEF MSWINDOWS}
  for i := 400 to 700 do
  begin
    if i = 678 then
      ShowMessage('123');
    spectral_color(rr, gg, bb, i);
    With TAlphaColorRec(AColor) do
    begin
      r := Trunc(rr * 255);
      g := Trunc(gg * 255);
      b := Trunc(bb * 255);
      A := $FF;
    end;
    Insert(AColor, FSpectralColors, Length(FSpectralColors));
  end;
  {$ENDIF}
end;

procedure TForm3.InitSpectralColors2;
var
  w: Integer;
  rr, gg, bb: double;
  AColor: TAlphaColor;
begin
  for w := 380 to 780 do
  begin
    if (w >= 380) and (w < 440) then
    begin
      rr := -(w - 440) / (440 - 380);
      gg := 0.0;
      bb := 1.0;
    end
    else if (w >= 440) and (w < 490) then
    begin
      rr := 0.0;
      gg := (w - 440) / (490 - 440);
      bb := 1.0;
    end
    else if (w >= 490) and (w < 510) then
    begin
      rr := 0.0;
      gg := 1.0;
      bb := -(w - 510) / (510 - 490);
    end
    else if (w >= 510) and (w < 580) then
    begin
      rr := (w - 510) / (580 - 510);
      gg := 1.0;
      bb := 0.0;
    end
    else if (w >= 580) and (w < 645) then
    begin
      rr := 1.0;
      gg := -(w - 645) / (645 - 580);
      bb := 0.0
    end
    else if (w >= 645) and (w <= 780) then
    begin
      rr := 1.0;
      gg := 0.0;
      bb := 0.0;
    end
    else
    begin
      rr := 0.0;
      gg := 0.0;
      bb := 0.0;
    end;

    With TAlphaColorRec(AColor) do
    begin
      r := Trunc(rr * 255);
      g := Trunc(gg * 255);
      b := Trunc(bb * 255);
      A := $FF;
    end;

    Insert(AColor, FSpectralColors, Length(FSpectralColors));
  end;
end;

procedure TForm3.InitSpectralColors3;
var
  w: Integer;
  rr, gg, bb: double;
  AColor: TAlphaColor;
begin
  for w := 380 to 780 do
  begin

    With TAlphaColorRec(AColor) do
    begin
      WavelengthToRGB(w, r, g, b);
      A := $FF;
    end;

    Insert(AColor, FSpectralColors, Length(FSpectralColors));
  end;
end;



var
  Phase: Single;
procedure TForm3.Timer1Timer(Sender: TObject);
{$IFDEF ANDROID}
{$DEFINE _A_}
{$ENDIF}
{$IFDEF _A_}
var
  nCount: Integer;
  i: Integer;
begin
  with Self.SplitedDrawer1 do
  begin
    nCount := AxisesData.Bottom.Max - AxisesData.Bottom.Min + 1;
    if nCount > 100 then
      nCount := 100;
    if nCount < 0 then
      nCount := 10;

    SetLength(FData, nCount);
    for i := 0 to nCount - 1 do
    begin
      FData[i] := (cos(2 * pi * i / (nCount - 1) + Phase) + 1) / 2
//      FData[i] := i / 100;
    end;
    Phase:= Phase + 2 * pi / 360;
  end;
  SignalChart1.DrawData(FData);
  Caption := 'FPS: ' + IntToStr(SignalChart1.FPS);
end;
{$ELSE}

var
  i: Integer;
  di: Integer;
  MYFFT: TArray<Single>;
begin
  {$IFDEF MSWINDOWS}
  if BASS_ChannelIsActive(hs) <> BASS_ACTIVE_PLAYING then
    Exit;

  BASS_ChannelGetData(hs, FFTData, BASS_DATA_FFT2048);
  SetLength(MYFFT, Length(FFTData) div 2);
  {$ENDIF}
  for i := 0 to Length(MYFFT) - 1 do
  begin
    MYFFT[i] := FFTData[i];
  end;
  SignalChart1.DrawData(MYFFT);
  Caption := 'FPS: ' + IntToStr(SignalChart1.FPS);

end;


{$ENDIF}
// var
// i: Integer;
// begin
// SetLength(FData, 1024);
// for i := 0 to Length(FData) - 1 do
// begin
// FData[i]:= Random(141) + 1;
/// /    FData[i]:= 141;
// end;
//
/// /  self.RadioSpectrumChart1.DrawData([
/// /  1,2,3,4,5,6,7,8,9,10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
/// /  22, 23, 24, 25, 26, 27, 28, 29, 30, 31
/// /  ]);
//
// SignalChart1.DrawData(FData);
// Caption:= IntToStr(SignalChart1.FPS);
// Application.ProcessMessages;
//
// end;


end.
