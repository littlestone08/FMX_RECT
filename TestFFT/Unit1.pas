unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    Timer1: TTimer;
    PaintBox1: TPaintBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Bass, Math, DateUtils;

var
  hs: HSTREAM;  {流句柄}
//  FFTData: array[0..512] of Single;
//  FFTPeacks  : array [0..512] of Integer;
//  FFTFallOff : array [0..512] of Integer;

  FFTData: array[0..256] of Single;
  FFTPeacks  : array [0..256] of Integer;
  FFTFallOff : array [0..256] of Integer;

  bit: TBitmap;
  Frames: TArray<TDateTime>;
procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenDialog1.FileName:= 'C:\Users\mei\Desktop\AUDIO\song1.wav';
  Timer1.Enabled := False;
  Timer1.Interval := 5;

  bit := TBitmap.Create;
//  PaintBox1.Align := alTop;

  if HiWord(BASS_GetVersion) <> BASSVERSION then
    MessageBox(0, '"Bass.dll" 文件版本不合适! ', nil, MB_ICONERROR);

  if not BASS_Init(-1, 44100, 0, 0, nil) then ShowMessage('初始化错误');
end;

{打开}
procedure TForm1.Button1Click(Sender: TObject);
var
  Mp3Path: AnsiString;
begin
  BASS_StreamFree(hs);

  OpenDialog1.Filter := 'Mp3 文件(*.mp3)|*.mp3|Wav 文件(*.wav)|*wav';
  if OpenDialog1.Execute then
    Mp3Path := AnsiString(OpenDialog1.FileName);

  hs := BASS_StreamCreateFile(False, PAnsiChar(Mp3Path), 0, 0, 0);
  if hs < BASS_ERROR_ENDED then
    Text := '打开失败'
  else begin
    Text := string(Mp3Path);
    bit.Free;
    bit := TBitmap.Create;
    PaintBox1.Repaint;
  end;
end;

{播放}
procedure TForm1.Button2Click(Sender: TObject);
begin
  Timer1.Enabled := True;
  BASS_ChannelPlay(hs, False);
end;

{暂停}
procedure TForm1.Button3Click(Sender: TObject);
begin
  Timer1.Enabled := False;
  BASS_ChannelPause(hs);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  BASS_Free;
  bit.Free;
end;

{刷新}
procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.StretchDraw(Bounds(0, 0, PaintBox1.Width, PaintBox1.Height), bit);
end;

{绘制 FFT}
procedure TForm1.Timer1Timer(Sender: TObject);
const
  w = 4;
var
  i,di: Integer;
var
  FramesCount: Integer;
begin

  FramesCount:= Length(Frames);
  Insert(Now, Frames, FramesCount);
  While Length(Frames) > 2 do
  begin
    if SecondsBetween(frames[0], Frames[FramesCount]) > 0 then
    begin
      Delete(Frames, 0, 1);
    end
    else
      Break;
  end;
  //==============================================================


  if BASS_ChannelIsActive(hs) <> BASS_ACTIVE_PLAYING then Exit;

  BASS_ChannelGetData(hs, @FFTData, BASS_DATA_FFT512);

  bit.Width := PaintBox1.Width;
  bit.Height := PaintBox1.Height;
  bit.Canvas.Brush.Color := clBlack;
//  bit.Canvas.Brush.Color := clwhite;
  bit.Canvas.FillRect(Rect(0, 0, bit.Width, bit.Height));

  bit.Canvas.Pen.Color := clLime;
  bit.Canvas.Font.Color := clLime;
  bit.Canvas.Brush.Style:= bsClear;;
  Bit.Canvas.TextOut(Bit.Canvas.TextWidth('    '), Bit.Canvas.TextHeight(' '), 'FPS: ' + IntToStr(Length(Frames)));

//  bit.Canvas.MoveTo(0, bit.Height);
  for i := 0 to Length(FFTData) - 1 do
  begin
//    di := Trunc(Abs(FFTData[i]) * 500);
    di := Trunc(Abs(FFTData[i]) * PaintBox1.Height * 2);

    if di > bit.Height then di := bit.Height;
    if di >= FFTPeacks[i] then FFTPeacks[i] := di else FFTPeacks[i] := FFTPeacks[i] - 1;
//    if di >= FFTFallOff[i] then FFTFallOff[i] := di else FFTFallOff[i] := FFTFallOff[i] - 3;
    FFTFallOff[i]:= di;
    if (bit.Height - FFTPeacks[i]) > bit.Height then FFTPeacks[i] := 0;
    if (bit.Height - FFTFallOff[i]) > bit.Height then FFTFallOff[i] := 0;

//    bit.Canvas.MoveTo(i, bit.Height);
//    bit.Canvas.LineTo(i, bit.Height - FFTFallOff[i]);
//    bit.Canvas.Pixels[i, bit.Height - FFTPeacks[i]] := bit.Canvas.Pen.Color;

    bit.Canvas.Pen.Color := bit.Canvas.Pen.Color;
    bit.Canvas.MoveTo(i * (w + 1), bit.Height - FFTPeacks[i]);
    bit.Canvas.LineTo(i * (w + 1) + w, bit.Height - FFTPeacks[i]);

    bit.Canvas.Pen.Color := bit.Canvas.Pen.Color;
    bit.Canvas.Brush.Color := bit.Canvas.Pen.Color;
    bit.Canvas.Rectangle(i * (w + 1), bit.Height - FFTFallOff[i], i * (w + 1) + w, bit.Height);
//    bit.Canvas.LineTo(i * (w + 1) + w div 2, bit.Height - FFTFallOff[i]);


  end;

  BitBlt(PaintBox1.Canvas.Handle, 0, 0, PaintBox1.Width, PaintBox1.Height, bit.Canvas.Handle, 0, 0, SRCCOPY);



end;

end.
