program pPlatformer;

uses
  Vcl.Forms,
  System.SysUtils,
  mMain in 'src\mMain.pas' {FormMain} ,
  dglOpenGL in 'src\dglOpenGL.pas',
  m3DObjects in 'src\m3DObjects.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.
