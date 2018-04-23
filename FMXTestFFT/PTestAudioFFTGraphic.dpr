program PTestAudioFFTGraphic;

uses
  System.Classes,
  System.SysUtils,
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {Form3},
  bass in '..\TestFFT\bass.pas',
  SpectraLibrary in 'SpectraLibrary.pas';

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
