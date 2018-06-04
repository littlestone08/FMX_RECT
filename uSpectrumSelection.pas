unit uSpectrumSelection;

interface

uses
  System.Classes, System.Types, System.UITypes, System.SysUtils,
  FMX.Types, FMX.Controls, FMX.Graphics;

type

  TSelection6P = class(TControl)
  public const
    DefaultColor = $FF1072C5;
  public type
    TGrabHandle = (None, LeftTop, RightTop, LeftBottom, RightBottom, LeftCenter, RightCenter);
    TGrabHandles = Set Of TGrabHandle;
  private
    FParentBounds: Boolean;
    FOnChange: TNotifyEvent;
    FMinSize: Integer;
    FOnTrack: TNotifyEvent;
    FProportional: Boolean;
    FGripSize: Single;
    FRatio: Single;
    FActiveHandle: TGrabHandle;
    FHotHandle: TGrabHandle;
    FDownPos: TPointF;
    FShowHandles: Boolean;
    FColor: TAlphaColor;
    FHandles: TGrabHandles;
    FCanMove: Boolean;
    procedure SetMinSize(const Value: Integer);
    procedure SetGripSize(const Value: Single);
    procedure ResetInSpace(const ARotationPoint: TPointF; ASize: TPointF);
    function GetProportionalSize(const ASize: TPointF): TPointF;
    function GetHandleForPoint(const P: TPointF): TGrabHandle;
    procedure GetTransformLeftTop(AX, AY: Single; var NewSize: TPointF;
      var Pivot: TPointF);
    procedure GetTransformLeftBottom(AX, AY: Single; var NewSize: TPointF;
      var Pivot: TPointF);
    procedure GetTransformRightTop(AX, AY: Single; var NewSize: TPointF;
      var Pivot: TPointF);
    procedure GetTransformRightBottom(AX, AY: Single; var NewSize: TPointF;
      var Pivot: TPointF);
    procedure GetTransformLeftCenter(AX, AY: Single; var NewSize: TPointF;
      var Pivot: TPointF);
    procedure GetTransformRightCenter(AX, AY: Single; var NewSize: TPointF;
      var Pivot: TPointF);
    procedure SetShowHandles(const Value: Boolean);
    procedure SetColor(const Value: TAlphaColor);
    procedure SetHandles(const Value: TGrabHandles);
    procedure SetCanMove(const Value: Boolean);
  protected
    procedure DrawFrame(const Canvas: TCanvas; const Rect: TRectF); virtual;
    procedure DrawHandles(R: TRectF; AHandles: TGrabHandles); virtual;
  Protected
    procedure MoveHandle(AX, AY: Single); Virtual;
    function DoGetUpdateRect: TRectF; override;
    Procedure PaintSelectoin(Rect: TRectF); virtual;
    Procedure CalcuPaintRect(var Rect: TRectF); Virtual;
    procedure Paint; override;
    /// <summary>Draw grip handle</summary>
    procedure DrawHandle(const Canvas: TCanvas; const Handle: TGrabHandle;
      const Rect: TRectF); virtual;
    /// <summary>Draw frame rectangle</summary>

    Procedure DoTrack(); virtual;
  public
    function PointInObjectLocal(X, Y: Single): Boolean; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure DoMouseLeave; override;
    /// <summary>Grip handle where mouse is hovered</summary>
    property HotHandle: TGrabHandle read FHotHandle;
    Property CanMove: Boolean read FCanMove write SetCanMove;
  published
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    /// <summary>Selection frame and handle's border color</summary>
    property Color: TAlphaColor read FColor write SetColor default DefaultColor;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property GripSize: Single read FGripSize write SetGripSize;
    property Locked default False;
    property Height;
    property HitTest default True;
    property Padding;
    property MinSize: Integer read FMinSize write SetMinSize default 15;
    property Opacity;
    property Margins;
    property ParentBounds: Boolean read FParentBounds write FParentBounds
      default True;
    property Proportional: Boolean read FProportional write FProportional;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    /// <summary>Indicates visibility of handles</summary>
    property ShowHandles: Boolean read FShowHandles write SetShowHandles;
    property Visible default True;
    property Width;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Mouse events }
    property OnClick;
    property OnDblClick;

    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnResized;
    property OnTrack: TNotifyEvent read FOnTrack write FOnTrack;
    Property Handles: TGrabHandles read FHandles write SetHandles;
  end;

  TSelectionUI = Class(TSelection6P)
  private
    procedure SetCenterLinePen(const Value: TStrokeBrush);
  Protected
    FCenterLinePen: TStrokeBrush;
    Procedure DrawCenterLine(const Canvas: TCanvas; const Rect: TRectF); virtual;
    Procedure PaintSelectoin(Rect: TRectF); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
  Published
    Property CenterLinePen: TStrokeBrush read FCenterLinePen write SetCenterLinePen;
  End;
implementation

uses
  System.Math.Vectors, System.UIConsts;

{ TSpectrumSelection }

procedure TSelection6P.CalcuPaintRect(var Rect: TRectF);
begin
  Rect := LocalRect;
  Rect.Inflate(-0.5, -0.5);
end;

constructor TSelection6P.Create(AOwner: TComponent);
begin
  inherited;

  FHandles:= [TGrabHandle.LeftTop,
              TGrabHandle.RightTop,
              TGrabHandle.LeftBottom,
              TGrabHandle.RightBottom,
              TGrabHandle.LeftCenter,
              TGrabHandle.RightCenter];
  FCanMove:= True;
  AutoCapture := True;
  ParentBounds := False;
//  CanFocus:= True;

  FColor := DefaultColor;
  FShowHandles := True;
  FMinSize := 15;
  FGripSize := 3;
  SetAcceptsControls(False);
end;

destructor TSelection6P.Destroy;
begin
  inherited;
end;

function TSelection6P.GetProportionalSize(const ASize: TPointF): TPointF;
begin
  Result := ASize;
  if FRatio * Result.Y > Result.X then
  begin
    if Result.X < FMinSize then
      Result.X := FMinSize;
    Result.Y := Result.X / FRatio;
    if Result.Y < FMinSize then
    begin
      Result.Y := FMinSize;
      Result.X := FMinSize * FRatio;
    end;
  end
  else
  begin
    if Result.Y < FMinSize then
      Result.Y := FMinSize;
    Result.X := Result.Y * FRatio;
    if Result.X < FMinSize then
    begin
      Result.X := FMinSize;
      Result.Y := FMinSize / FRatio;
    end;
  end;
end;

function TSelection6P.GetHandleForPoint(const P: TPointF): TGrabHandle;
var
  Local, R: TRectF;
begin
  Local := LocalRect;
  R := TRectF.Create(Local.Left - GripSize, Local.Top - GripSize,
    Local.Left + GripSize, Local.Top + GripSize);
  if R.Contains(P) then
    Exit(TGrabHandle.LeftTop);
  R := TRectF.Create(Local.Right - GripSize, Local.Top - GripSize,
    Local.Right + GripSize, Local.Top + GripSize);
  if R.Contains(P) then
    Exit(TGrabHandle.RightTop);
  R := TRectF.Create(Local.Right - GripSize, Local.Bottom - GripSize,
    Local.Right + GripSize, Local.Bottom + GripSize);
  if R.Contains(P) then
    Exit(TGrabHandle.RightBottom);
  R := TRectF.Create(Local.Left - GripSize, Local.Bottom - GripSize,
    Local.Left + GripSize, Local.Bottom + GripSize);
  if R.Contains(P) then
    Exit(TGrabHandle.LeftBottom);
  //======================
  R := TRectF.Create(Local.Left - GripSize,
      (Local.Bottom + Local.Top)/ 2 - GripSize, Local.Left + GripSize, (Local.Bottom + Local.Top)/ 2 + GripSize);
  if R.Contains(P) then
    Exit(TGrabHandle.LeftCenter);

  R := TRectF.Create(Local.Right - GripSize,
      (Local.Bottom + Local.Top)/ 2 - GripSize, Local.Right + GripSize, (Local.Bottom + Local.Top)/ 2 + GripSize);
  if R.Contains(P) then
    Exit(TGrabHandle.RightCenter);
  //===============
  Result := TGrabHandle.None;
end;

procedure TSelection6P.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  // this line may be necessary because TSpectrumSelection is not a styled control;
  // must further investigate for a better fix
  if not Enabled then
    Exit;

  inherited;

  FDownPos := TPointF.Create(X, Y);
  if Button = TMouseButton.mbLeft then
  begin
    FRatio := Width / Height;
    FActiveHandle := GetHandleForPoint(FDownPos);

    if Not (FActiveHandle in FHandles) then
      FActiveHandle:= TGrabHandle.None;
  end;
end;

procedure TSelection6P.MouseMove(Shift: TShiftState; X, Y: Single);
const
  CONST_CURSORS: Array[Low(TGrabHandle)..High(TGrabHandle)] of TCursor = (
    crDefault, crSizeNWSE, crSizeNESW, crSizeNESW, crSizeNWSE, crSizeWE, crSizeWE
  );
var
  P, OldPos: TPointF;
  MoveVector: TVector;
  MovePos: TPointF;
  GrabHandle: TGrabHandle;
begin
  // this line may be necessary because TSpectrumSelection is not a styled control;
  // must further investigate for a better fix
  if not Enabled then
    Exit;

  inherited;

  MovePos := TPointF.Create(X, Y);
  if not Pressed then
  begin
    // handle painting for hotspot mouse hovering
    GrabHandle := GetHandleForPoint(MovePos);
    if GrabHandle <> FHotHandle then
      Repaint;
    FHotHandle := GrabHandle;
  end
  else if ssLeft in Shift then
  begin
    if (FActiveHandle = TGrabHandle.None) then
    begin
      if FCanMove then
      begin
        MoveVector := LocalToAbsoluteVector(TVector.Create(X - FDownPos.X,
          Y - FDownPos.Y));
        if ParentControl <> nil then
          MoveVector := ParentControl.AbsoluteToLocalVector(MoveVector);
        Position.Point := Position.Point + TPointF(MoveVector);
        if ParentBounds then
        begin
          if Position.X < 0 then
            Position.X := 0;
          if Position.Y < 0 then
            Position.Y := 0;
          if ParentControl <> nil then
          begin
            if Position.X + Width > ParentControl.Width then
              Position.X := ParentControl.Width - Width;
            if Position.Y + Height > ParentControl.Height then
              Position.Y := ParentControl.Height - Height;
          end
          else if Canvas <> nil then
          begin
            if Position.X + Width > Canvas.Width then
              Position.X := Canvas.Width - Width;
            if Position.Y + Height > Canvas.Height then
              Position.Y := Canvas.Height - Height;
          end;
        end;
        DoTrack();
      end;
      Exit;
    end;

    OldPos := Position.Point;
    P := LocalToAbsolute(MovePos);
    if ParentControl <> nil then
      P := ParentControl.AbsoluteToLocal(P);
    if ParentBounds then
    begin
      if P.Y < 0 then
        P.Y := 0;
      if P.X < 0 then
        P.X := 0;
      if ParentControl <> nil then
      begin
        if P.X > ParentControl.Width then
          P.X := ParentControl.Width;
        if P.Y > ParentControl.Height then
          P.Y := ParentControl.Height;
      end
      else if Canvas <> nil then
      begin
        if P.X > Canvas.Width then
          P.X := Canvas.Width;
        if P.Y > Canvas.Height then
          P.Y := Canvas.Height;
      end;
    end;
    MoveHandle(X, Y);
  end;
  if FHotHandle in FHandles then
    Cursor:= CONST_CURSORS[FHotHandle]
  else
    Cursor:= CONST_CURSORS[TGrabHandle.None];
end;

function TSelection6P.PointInObjectLocal(X, Y: Single): Boolean;
begin
  Result := inherited or (GetHandleForPoint(TPointF.Create(X, Y)) <>
    TGrabHandle.None);
end;

procedure TSelection6P.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  // this line may be necessary because TSpectrumSelection is not a styled control;
  // must further investigate for a better fix
  if not Enabled then
    Exit;

  inherited;

  if Assigned(FOnChange) then
    FOnChange(Self);
  FActiveHandle := TGrabHandle.None;
end;

procedure TSelection6P.DrawFrame(const Canvas: TCanvas;
  const Rect: TRectF);
begin
  Canvas.DrawDashRect(Rect, 0, 0, AllCorners, AbsoluteOpacity, FColor);
end;

procedure TSelection6P.DrawHandle(const Canvas: TCanvas;
  const Handle: TGrabHandle; const Rect: TRectF);
var
  Fill: TBrush;
  Stroke: TStrokeBrush;
begin
  Fill := TBrush.Create(TBrushKind.Solid, claWhite);
  Stroke := TStrokeBrush.Create(TBrushKind.Solid, FColor);
  try
    if Enabled then
      if FHotHandle = Handle then
        Canvas.Fill.Color := claRed
      else
        Canvas.Fill.Color := claWhite
    else
      Canvas.Fill.Color := claGrey;

    Canvas.FillEllipse(Rect, AbsoluteOpacity, Fill);
    Canvas.DrawEllipse(Rect, AbsoluteOpacity, Stroke);
  finally
    Fill.Free;
    Stroke.Free;
  end;
end;

procedure TSelection6P.Paint;
var
  R: TRectF;
begin
  CalcuPaintRect(R);
  PaintSelectoin(R);
end;

procedure TSelection6P.PaintSelectoin(Rect: TRectF);
begin
  DrawFrame(Canvas, Rect);

  if (FHandles <> [])  and ShowHandles then
  begin
    DrawHandles(Rect, FHandles);
  end;
end;


function TSelection6P.DoGetUpdateRect: TRectF;
begin
  Result := inherited;
  Result.Inflate((FGripSize + 1) * Scale.X, (FGripSize + 1) * Scale.Y);
end;

procedure TSelection6P.ResetInSpace(const ARotationPoint: TPointF;
  ASize: TPointF);
var
  LLocalPos: TPointF;
  LAbsPos: TPointF;
begin
  LAbsPos := LocalToAbsolute(ARotationPoint);
  if ParentControl <> nil then
  begin
    LLocalPos := ParentControl.AbsoluteToLocal(LAbsPos);
    LLocalPos.X := LLocalPos.X - ASize.X * RotationCenter.X * Scale.X;
    LLocalPos.Y := LLocalPos.Y - ASize.Y * RotationCenter.Y * Scale.Y;
    if ParentBounds then
    begin
      if LLocalPos.X < 0 then
      begin
        ASize.X := ASize.X + LLocalPos.X;
        LLocalPos.X := 0;
      end;
      if LLocalPos.Y < 0 then
      begin
        ASize.Y := ASize.Y + LLocalPos.Y;
        LLocalPos.Y := 0;
      end;
      if LLocalPos.X + ASize.X > ParentControl.Width then
        ASize.X := ParentControl.Width - LLocalPos.X;
      if LLocalPos.Y + ASize.Y > ParentControl.Height then
        ASize.Y := ParentControl.Height - LLocalPos.Y;
    end;
  end
  else
  begin
    LLocalPos.X := LAbsPos.X - ASize.X * RotationCenter.X * Scale.X;
    LLocalPos.Y := LAbsPos.Y - ASize.Y * RotationCenter.Y * Scale.Y;
  end;
  SetBounds(LLocalPos.X, LLocalPos.Y, ASize.X, ASize.Y);
end;

procedure TSelection6P.GetTransformLeftTop(AX, AY: Single;
  var NewSize: TPointF; var Pivot: TPointF);
var
  LCorrect: TPointF;
begin
  NewSize := Size.Size - TSizeF.Create(AX, AY);
  if NewSize.Y < FMinSize then
  begin
    AY := Height - FMinSize;
    NewSize.Y := FMinSize;
  end;
  if NewSize.X < FMinSize then
  begin
    AX := Width - FMinSize;
    NewSize.X := FMinSize;
  end;
  if FProportional then
  begin
    LCorrect := NewSize;
    NewSize := GetProportionalSize(NewSize);
    LCorrect := LCorrect - NewSize;
    AX := AX + LCorrect.X;
    AY := AY + LCorrect.Y;
  end;
  Pivot := TPointF.Create(Width * RotationCenter.X + AX *
    (1 - RotationCenter.X), Height * RotationCenter.Y + AY *
    (1 - RotationCenter.Y));
end;

procedure TSelection6P.GetTransformLeftBottom(AX, AY: Single;
  var NewSize: TPointF; var Pivot: TPointF);
var
  LCorrect: TPointF;
begin
  NewSize := TPointF.Create(Width - AX, AY);
  if NewSize.Y < FMinSize then
  begin
    AY := FMinSize;
    NewSize.Y := FMinSize;
  end;
  if NewSize.X < FMinSize then
  begin
    AX := Width - FMinSize;
    NewSize.X := FMinSize;
  end;
  if FProportional then
  begin
    LCorrect := NewSize;
    NewSize := GetProportionalSize(NewSize);
    LCorrect := LCorrect - NewSize;
    AX := AX + LCorrect.X;
    AY := AY + LCorrect.Y;
  end;
  Pivot := TPointF.Create(Width * RotationCenter.X + AX *
    (1 - RotationCenter.X), Height * RotationCenter.Y + (AY - Height) *
    RotationCenter.Y);
end;

procedure TSelection6P.GetTransformLeftCenter(AX, AY: Single; var NewSize,
  Pivot: TPointF);
var
  LCorrect: TPointF;
begin
  NewSize := Size.Size - TSizeF.Create(AX, 0);

  if NewSize.X < FMinSize then
  begin
    AX := Width - FMinSize;
    NewSize.X := FMinSize;
  end;

  if FProportional then
  begin
    LCorrect := NewSize;
    NewSize := GetProportionalSize(NewSize);
    LCorrect := LCorrect - NewSize;
    AX := AX + LCorrect.X;
  end;

  Pivot := TPointF.Create(Width * RotationCenter.X + AX * (1 - RotationCenter.X),
    Height * RotationCenter.Y);
end;

procedure TSelection6P.GetTransformRightTop(AX, AY: Single;
  var NewSize: TPointF; var Pivot: TPointF);
var
  LCorrect: TPointF;
begin
  NewSize := TPointF.Create(AX, Height - AY);
  if NewSize.Y < FMinSize then
  begin
    AY := Height - FMinSize;
    NewSize.Y := FMinSize;
  end;
  if AX < FMinSize then
  begin
    AX := FMinSize;
    NewSize.X := FMinSize;
  end;
  if FProportional then
  begin
    LCorrect := NewSize;
    NewSize := GetProportionalSize(NewSize);
    LCorrect := LCorrect - NewSize;
    AX := AX - LCorrect.X;
    AY := AY + LCorrect.Y;
  end;
  Pivot := TPointF.Create(Width * RotationCenter.X + (AX - Width) *
    RotationCenter.X, Height * RotationCenter.Y + AY * (1 - RotationCenter.Y));
end;

procedure TSelection6P.GetTransformRightBottom(AX, AY: Single;
  var NewSize: TPointF; var Pivot: TPointF);
var
  LCorrect: TPointF;
begin
  NewSize := TPointF.Create(AX, AY);
  if NewSize.Y < FMinSize then
  begin
    AY := FMinSize;
    NewSize.Y := FMinSize;
  end;
  if NewSize.X < FMinSize then
  begin
    AX := FMinSize;
    NewSize.X := FMinSize;
  end;
  if FProportional then
  begin
    LCorrect := NewSize;
    NewSize := GetProportionalSize(NewSize);
    LCorrect := LCorrect - NewSize;
    AX := AX - LCorrect.X;
    AY := AY - LCorrect.Y;
  end;
  Pivot := TPointF.Create(Width * RotationCenter.X + (AX - Width) *
    RotationCenter.X, Height * RotationCenter.Y + (AY - Height) *
    RotationCenter.Y);
end;

procedure TSelection6P.GetTransformRightCenter(AX, AY: Single;
  var NewSize, Pivot: TPointF);
var
  LCorrect: TPointF;
begin
  NewSize := TPointF.Create(AX, Height - 0);

  if NewSize.X < FMinSize then
  begin
    AX := FMinSize;
    NewSize.X := FMinSize;
  end;
  if FProportional then
  begin
    LCorrect := NewSize;
    NewSize := GetProportionalSize(NewSize);
    LCorrect := LCorrect - NewSize;
    AX := AX + LCorrect.X;
  end;

  Pivot := TPointF.Create(Width * RotationCenter.X + (AX - Width) *  RotationCenter.X,
    Height * RotationCenter.Y);
end;

procedure TSelection6P.MoveHandle(AX, AY: Single);
var
  NewSize, Pivot: TPointF;
begin
  case FActiveHandle of
    TSelection6P.TGrabHandle.LeftTop:
      GetTransformLeftTop(AX, AY, NewSize, Pivot);
    TSelection6P.TGrabHandle.LeftBottom:
      GetTransformLeftBottom(AX, AY, NewSize, Pivot);
    TSelection6P.TGrabHandle.RightTop:
      GetTransformRightTop(AX, AY, NewSize, Pivot);
    TSelection6P.TGrabHandle.RightBottom:
      GetTransformRightBottom(AX, AY, NewSize, Pivot);
    TSelection6P.TGrabHandle.LeftCenter:
      GetTransformLeftCenter(AX, AY, NewSize, Pivot);
    TSelection6P.TGrabHandle.RightCenter:
      GetTransformRightCenter(AX, AY, NewSize, Pivot);
  end;
  ResetInSpace(Pivot, NewSize);
  if Assigned(FOnTrack) then
    FOnTrack(Self);
end;

procedure TSelection6P.DoMouseLeave;
begin
  inherited;
  FHotHandle := TGrabHandle.None;
  Repaint;
end;

procedure TSelection6P.DrawHandles(R: TRectF; AHandles: TGrabHandles);
begin
  R := LocalRect;
  if TGrabHandle.LeftTop in AHandles then
    DrawHandle(Canvas, TGrabHandle.LeftTop, TRectF.Create(R.Left - GripSize, R.Top - GripSize, R.Left + GripSize, R.Top + GripSize));
  if TGrabHandle.RightTop in AHandles then
    DrawHandle(Canvas, TGrabHandle.RightTop, TRectF.Create(R.Right - GripSize, R.Top - GripSize, R.Right + GripSize, R.Top + GripSize));
  if TGrabHandle.LeftBottom in AHandles then
    DrawHandle(Canvas, TGrabHandle.LeftBottom, TRectF.Create(R.Left - GripSize, R.Bottom - GripSize, R.Left + GripSize, R.Bottom + GripSize));
  if TGrabHandle.RightBottom in AHandles then
    DrawHandle(Canvas, TGrabHandle.RightBottom, TRectF.Create(R.Right - GripSize, R.Bottom - GripSize, R.Right + GripSize, R.Bottom + GripSize));
  if TGrabHandle.LeftCenter in AHandles then
    DrawHandle(Canvas, TGrabHandle.LeftCenter, TRectF.Create(R.Left - GripSize, (R.Bottom + R.Top) / 2 - GripSize, R.Left + GripSize, (R.Bottom + R.Top) / 2 + GripSize));
  if TGrabHandle.RightCenter in AHandles then
    DrawHandle(Canvas, TGrabHandle.RightCenter, TRectF.Create(R.Right - GripSize, (R.Bottom + R.Top) / 2 - GripSize, R.Right + GripSize, (R.Bottom + R.Top) / 2 + GripSize));
end;

procedure TSelection6P.DoTrack();
begin
  if Assigned(FOnTrack) then
    FOnTrack(Self);
end;

procedure TSelection6P.SetHandles(const Value: TGrabHandles);
begin
  FHandles := Value;
  Repaint;
end;



procedure TSelection6P.SetMinSize(const Value: Integer);
begin
  if FMinSize <> Value then
  begin
    FMinSize := Value;
    if FMinSize < 1 then
      FMinSize := 1;
  end;
end;

procedure TSelection6P.SetShowHandles(const Value: Boolean);
begin
  if FShowHandles <> Value then
  begin
    FShowHandles := Value;
    Repaint;
  end;
end;

procedure TSelection6P.SetCanMove(const Value: Boolean);
begin
  FCanMove := Value;
end;

procedure TSelection6P.SetColor(const Value: TAlphaColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Repaint;
  end;
end;

procedure TSelection6P.SetGripSize(const Value: Single);
begin
  if FGripSize <> Value then
  begin
    if Value < FGripSize then
      Repaint;
    FGripSize := Value;
    if FGripSize > 20 then
      FGripSize := 20;
    if FGripSize < 1 then
      FGripSize := 1;
    HandleSizeChanged;
    Repaint;
  end;
end;

procedure TSelectionUI.SetCenterLinePen(const Value: TStrokeBrush);
begin
  FCenterLinePen.Assign(Value);
end;

{ TSpectrumSelection }

constructor TSelectionUI.Create(AOwner: TComponent);
begin
  inherited;
  FCenterLinePen:= TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.Red);
end;

destructor TSelectionUI.Destroy;
begin
  FreeAndNil(FCenterLinePen);
  inherited;
end;

procedure TSelectionUI.DrawCenterLine(const Canvas: TCanvas;
  const Rect: TRectF);
var
  A, B: TPointF;
begin
  A:= TPointF.Create((Rect.Left + Rect.Right) / 2, Rect.Top);
  B:= TPointF.Create((Rect.Left + Rect.Right) / 2, Rect.Bottom);
  Canvas.DrawLine(A, B, 1, FCenterLinePen);
end;




procedure TSelectionUI.PaintSelectoin(Rect: TRectF);
begin
  inherited;
  DrawCenterLine(Canvas, Rect);
end;

end.


