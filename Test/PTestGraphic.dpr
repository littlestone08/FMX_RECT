program PTestGraphic;

uses
  System.Classes,
  System.SysUtils,
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {Form3};

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
  Application.Run;
end.
