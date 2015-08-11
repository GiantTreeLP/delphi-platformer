program pPlatformer;

uses
  Vcl.Forms,
  System.SysUtils,
  dglOpenGL in 'src\dglOpenGL.pas',
  m3DObjects in 'src\m3DObjects.pas',
  mMain in 'src\mMain.pas' {FormMain} ,
  mUtils in 'src\mUtils.pas',
  mGenericBlock in 'src\Blocks\mGenericBlock.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := False;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Metropolis UI Blue');
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.
