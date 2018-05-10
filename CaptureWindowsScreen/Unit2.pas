unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation
uses
  jpeg;

{$R *.dfm}

procedure CaptureScreen(ABitmap: TBitmap);
var
  vDesktopDC: HDC;   // variable to store the device context handle of desktop window
begin
  // get the device context handle of current desktop window
  vDesktopDC := GetWindowDC(GetDesktopWindow);
  try
      // adjust the dimension and format of the supplied bitmap to match the screen
      ABitmap.PixelFormat := pf24bit;
      ABitmap.Height := Screen.Height;
      ABitmap.Width := Screen.Width;

      // draw the content of desktop into ABitmap
      BitBlt(ABitmap.Canvas.Handle, 0, 0, ABitmap.Width, ABitmap.Height, vDesktopDC, 0, 0, SRCCOPY);
  finally
    // mark that we have done with the desktop device context
    ReleaseDC(GetDesktopWindow, vDesktopDC);
  end;
end;

procedure CaptureScreenToFile(const AFilename: string);
var
  vJpg: TJpegImage;
  vBmp: TBitmap;
begin
  // create temporary bitmap
  vBmp := TBitmap.Create;
  try
    CaptureScreen(vBmp);
    // create Jpg image object
    vJpg := TJpegImage.Create;
    try
      vJpg.Assign(vBmp);
      // compress the image to have quality 70% of original
      vJpg.CompressionQuality := 70;
      // save the captured screen into a file in jpg format
      vJpg.SaveToFile(AFileName);
    finally
      vJpg.Free;  //destroy the jpg image object
    end;
  finally
    vBmp.Free; // destroy temporary bitmap
  end;
end;
procedure TForm2.Button1Click(Sender: TObject);
begin
  CaptureScreenToFile('d:\3.jpeg')
end;

end.
