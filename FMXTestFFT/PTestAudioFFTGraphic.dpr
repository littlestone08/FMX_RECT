program PTestAudioFFTGraphic;

uses
  System.Classes,
  System.SysUtils,
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {Form3},
  ufrmMask in 'ufrmMask.pas' {frmMask},
  ufrmLog in 'ufrmLog.pas' {frmLog};

{$R *.res}
Procedure DoneApplication;
begin
  CheckSynchronize();
end;

begin
  ReportMemoryLeaksOnShutdown:= True;
  AddExitProc(DoneApplication);
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TfrmMask, frmMask);
  Application.CreateForm(TfrmLog, frmLog);
  Application.Run;
end.
