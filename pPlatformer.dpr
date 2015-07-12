program pPlatformer;

uses
  Vcl.Forms,
  System.SysUtils,
  mMain in 'mMain.pas' {FormMain},
  dglOpenGL in 'dglOpenGL.pas',
  m3DObjects in 'm3DObjects.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.
