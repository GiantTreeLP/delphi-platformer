unit mMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  dglOpenGL,
  Math,
  System.Generics.Collections,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  m3DObjects,
  DateUtils;

type
  TCustomVector3d = class(TObject)
    X, Y, Z: Double;
    constructor Create(X, Y, Z: Double);
  end;

  TRenderThread = class(TThread)
    DC: HDC;
    RC: HGLRC;
    XRotation, YRotation: Double;
    OldCursorPos: TPoint;
    FrameTime: Cardinal;
    LeftDown: Boolean;

    var
      X, Y, Z: Double;

    private
      procedure Render;
      function Velocity: Double;
    protected
      procedure Execute; override;
  end;

  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    public
      { Public-Deklarationen }
    private
      Thread: TRenderThread;
  end;

var
  FormMain: TFormMain;
  Count: Int64 = 0;
  Objects3d: TList<TCustomObject3d>;

const
  Speed = 2.0;
  NearClipping = 0.1;
  FarClipping = 1000;
  FOV = 75;

implementation

{$R *.dfm}

procedure IncF(var Base: Double; Add: Double);
  begin
    Base := Base + Add;
  end;

constructor TCustomVector3d.Create(X, Y, Z: Double);
  begin
    Self.X := X;
    Self.Y := Y;
    Self.Z := Z;
  end;

procedure TFormMain.FormCreate(Sender: TObject);
  begin
    Thread := TRenderThread.Create;
    ShowCursor(False);
  end;

function TRenderThread.Velocity: Double;
  begin
    Result := Speed * (FrameTime / MSecsPerSec);
  end;

procedure TRenderThread.Render;
  var
    O: TCustomObject3d;
    XDelta, YDelta: Integer;
  begin
    XDelta := Mouse.CursorPos.X - OldCursorPos.X;
    YDelta := Mouse.CursorPos.Y - OldCursorPos.Y;

    SetCursorPos(Screen.Width div 2, Screen.Height div 2);
    GetCursorPos(OldCursorPos);

    IncF(XRotation, (YDelta / Screen.Height) * FOV);
    IncF(YRotation, (XDelta / Screen.Width) * FOV);

    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    gluPerspective(FOV, FormMain.ClientWidth / FormMain.ClientHeight, NearClipping, FarClipping);

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity;

    if GetASyncKeyState(Ord('A')) <> 0 then
      begin
        IncF(X, -Cos(DegToRad(YRotation)) * Velocity);
        IncF(Z, -Sin(DegToRad(YRotation)) * Velocity);
      end;
    if GetASyncKeyState(Ord('D')) <> 0 then
      begin
        IncF(X, Cos(DegToRad(YRotation)) * Velocity);
        IncF(Z, Sin(DegToRad(YRotation)) * Velocity);
      end;

    if GetASyncKeyState(VK_SHIFT) <> 0 then
      begin
        IncF(Y, -Velocity);
      end;
    if GetASyncKeyState(VK_SPACE) <> 0 then
      begin
        IncF(Y, Velocity);
      end;

    if GetASyncKeyState(Ord('W')) <> 0 then
      begin
        IncF(Z, -Cos(DegToRad(YRotation)) * Velocity);
        IncF(X, Sin(DegToRad(YRotation)) * Velocity);
      end;
    if GetASyncKeyState(Ord('S')) <> 0 then
      begin
        IncF(Z, Cos(DegToRad(YRotation)) * Velocity);
        IncF(X, -Sin(DegToRad(YRotation)) * Velocity);
      end;

    if GetASyncKeyState(VK_ESCAPE) <> 0 then
      begin
        Suspended := True;
      end;

    if GetASyncKeyState(VK_LBUTTON) <> 0 then
      begin
        if not LeftDown then
          begin
            Objects3d.Add(TCube3d.Create(Round(X), Round(Y), Round(Z), 1, 1, 1, 0, 0, 0, Random, Random, Random, 1));
            LeftDown := True;
          end;
      end
    else
      begin
        LeftDown := False;
      end;

    glRotated(XRotation, 1, 0, 0);
    glRotated(YRotation, 0, 1, 0);

    glTranslated( -X, -Y, -Z);

    for O in Objects3d do
      O.Render;

    Inc(Count);

    SwapBuffers(DC);
  end;

procedure TRenderThread.Execute;
  var
    StartTime: Cardinal;
    I, J: Integer;
  begin
    Randomize;
    X := 0;
    Y := 1.70;
    Z := 0;
    XRotation := 0;
    YRotation := 180;

    DC := GetDC(FormMain.Handle);
    if not InitOpenGL then
      Application.Terminate;
    RC := CreateRenderingContext(DC, [opDoubleBuffered], 32, 24, 1, 0, 0, 0);
    ActivateRenderingContext(DC, RC);

    glClearColor(0.1, 0.1, 0.2, 1);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
    glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);

    wglSwapIntervalEXT(0);
    glViewport(0, 0, FormMain.ClientWidth, FormMain.ClientHeight);

    Objects3d := TList<TCustomObject3d>.Create;

    for I := 0 to 9 do
      for J := 0 to 9 do
        begin
          Objects3d.Add(TCube3d.Create(I, 0, J, 1, 1, 1, 0, 0, 0, Random, Random, Random, 1));
        end;

    while not Suspended do
      begin
        StartTime := GetTickCount;
        if FormMain.Active then
          begin
            Render;
          end;
        Sleep(16);
        FrameTime := GetTickCount - StartTime + 1;
        FormMain.Caption := Format('FPS: %f, Velocity: %.5f', [1000 / FrameTime, Velocity]);
      end;

    DeactivateRenderingContext;
    DestroyRenderingContext(RC);
    ReleaseDC(Handle, DC);
    Free;
  end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    if Key = VK_ESCAPE then
      Close;
  end;

end.
