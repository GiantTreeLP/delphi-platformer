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
  mUtils,
  DateUtils,
  mGenericBlock;

type

  TRenderThread = class(TThread)
    DC: HDC;
    RC: HGLRC;
    OldCursorPos: TPoint;
    FrameTime: Cardinal;
    RenderNoise: Boolean;
    UpdatingObjects: Boolean;
    DeltaTime: Cardinal;
    private
      procedure Render;
    protected
      procedure Execute; override;
    public
      RenderingBlocks: Boolean;
  end;

  TControlThread = class(TThread)
    LeftDown, ControlDown, AddDown, SubDown, Jumping: Boolean;
    private
      procedure EvaluateKeystates;
      procedure CheckCollision;
      procedure ApplyMotion;
      function Velocity: Double;
    protected
      procedure Execute; override;
  end;

  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    public

      XRotation, YRotation: Double;

    var
      Motion, Position: TCustomVector3d;
      Thread: TRenderThread;
  end;

var
  FormMain: TFormMain;
  Count: Int64 = 0;
  Objects3d: TBlockMap;
  Noise: TOpenSimplexNoise;
  ObjectsRadius: UInt = 15;
  NoiseThreshold: Double = 0;
  P: TCube3d;
  Speed: Double = 3.5;

const
  NearClipping = 0.001;
  FarClipping = 1000;
  FOV = 75;
  YRotationMax = 82.5;
  PlayerSize = 1.8;
  NoiseZoomLevel = 10;
  Gravity = -0.5;
  // DEBUG
  DEBUG = TRUE;
  NOCLIP = TRUE;
  FLY = TRUE;

implementation

{$R *.dfm}

procedure IncF(var Base: Double; Add: Double);
  begin
    Base := Base + Add;
  end;

procedure ClampF(var Base: Double; Low, High: Double);
  begin
    Base := Min(Max(Base, Low), High);
  end;

function HSVToRGB(H: Integer; S, V: Byte): COLORREF;
  var
    ht, d, t1, t2, t3: Integer;
    R, G, B: NativeInt;
  begin
    if S = 0 then
      begin
        R := V;
        G := V;
        B := V;
      end
    else
      begin
        H := H mod 360;
        ht := H * 6;
        d := ht mod 360;

        t1 := Round(V * (255 - S) / 255);
        t2 := Round(V * (255 - S * d / 360) / 255);
        t3 := Round(V * (255 - S * (360 - d) / 360) / 255);

        case ht div 360 of
          0:
            begin
              R := V;
              G := t3;
              B := t1;
            end;
          1:
            begin
              R := t2;
              G := V;
              B := t1;
            end;
          2:
            begin
              R := t1;
              G := V;
              B := t3;
            end;
          3:
            begin
              R := t1;
              G := t2;
              B := V;
            end;
          4:
            begin
              R := t3;
              G := t1;
              B := V;
            end;
          else
            begin
              R := V;
              G := t1;
              B := t2;
            end;
        end;
      end;
    Result := RGB(R, G, B);
  end;

procedure TControlThread.Execute;
  const
    S = 'FPS: %f; FrameTime: %d, Velocity: %.7f; Position: %f, %f, %f; NoiseThreshold: %f';
  begin
    while (not CheckTerminated) do
      begin
        EvaluateKeystates;
        CheckCollision;
        ApplyMotion;
        if FormMain.Active and (FormMain.Thread.DeltaTime > 0) then
          begin
            FormMain.Caption := Format(S, [MSecsPerSec / FormMain.Thread.DeltaTime, FormMain.Thread.DeltaTime, Velocity,
              FormMain.Position.X, FormMain.Position.Y, FormMain.Position.Z, NoiseThreshold]);
          end;
        Sleep(16);
      end;
  end;

procedure CreateObjects;
  var
    I: Integer;
    J: Integer;
    K: Integer;
  begin
    FormMain.Thread.UpdatingObjects := TRUE;
    if Objects3d <> nil then
      Objects3d.FreeInstance;
    Objects3d := TBlockMap.Create;
    for I := 0 to ObjectsRadius do
      begin
        for J := 0 to ObjectsRadius do
          begin
            for K := 0 to ObjectsRadius do
              begin
                if Noise.Eval(I / NoiseZoomLevel, J / NoiseZoomLevel, K / NoiseZoomLevel) >= NoiseThreshold then
                  begin
                    Objects3d.SetBlock(I, J, K, TGenericBlock.Create(I, J, K, 1));
                  end;
              end;
          end;
      end;
    FormMain.Thread.UpdatingObjects := False;
  end;

procedure TControlThread.EvaluateKeystates;
  begin
    begin
      with FormMain do
        begin
          if DEBUG then
            begin
              if GetAsyncKeyState(VK_LEFT) <> 0 then
                IncF(NoiseThreshold, -0.001);
              if GetAsyncKeyState(VK_RIGHT) <> 0 then
                IncF(NoiseThreshold, 0.001);
            end;

          if GetAsyncKeyState(Ord('A')) <> 0 then
            begin
              IncF(Motion.X, -Cos(DegToRad(YRotation)) * Velocity);
              IncF(Motion.Z, -Sin(DegToRad(YRotation)) * Velocity);
            end;
          if GetAsyncKeyState(Ord('D')) <> 0 then
            begin
              IncF(Motion.X, Cos(DegToRad(YRotation)) * Velocity);
              IncF(Motion.Z, Sin(DegToRad(YRotation)) * Velocity);
            end;
          if GetAsyncKeyState(VK_SHIFT) <> 0 then
            begin
              IncF(Motion.Y, -Velocity);
            end;
          if GetAsyncKeyState(VK_SPACE) <> 0 then
            begin
              if not Jumping then
                begin
                  Jumping := TRUE;
                  IncF(Motion.Y, Abs(Gravity) / 3);
                end
            end;
          if GetAsyncKeyState(Ord('W')) <> 0 then
            begin
              IncF(Motion.Z, -Cos(DegToRad(YRotation)) * Velocity);
              IncF(Motion.X, Sin(DegToRad(YRotation)) * Velocity);
            end;
          if GetAsyncKeyState(Ord('S')) <> 0 then
            begin
              IncF(Motion.Z, Cos(DegToRad(YRotation)) * Velocity);
              IncF(Motion.X, -Sin(DegToRad(YRotation)) * Velocity);
            end;
          if GetAsyncKeyState(VK_ESCAPE) <> 0 then
            begin
              Suspended := TRUE;
              FormMain.Thread.Suspended := TRUE;
            end;
          if GetAsyncKeyState(VK_LBUTTON) <> 0 then
            begin
              if not LeftDown then
                begin
                  LeftDown := TRUE;
                end;
            end
          else
            begin
              LeftDown := False;
            end;
          if GetAsyncKeyState(VK_RBUTTON) <> 0 then
            begin
              Position.X := 0;
              Position.Y := 20;
              Position.Z := 0;
            end;
          if GetAsyncKeyState(VK_CONTROL) <> 0 then
            begin
              if not ControlDown then
                FormMain.Thread.RenderNoise := not FormMain.Thread.RenderNoise;
              ControlDown := TRUE;
            end
          else
            begin
              ControlDown := False;
            end;

          if GetAsyncKeyState(VK_ADD) <> 0 then
            begin
              if not AddDown then
                begin
                  Inc(ObjectsRadius);
                  AddDown := TRUE;
                  while FormMain.Thread.RenderingBlocks do
                    begin
                      Sleep(1);
                    end;
                  CreateObjects();
                end;
            end
          else
            begin
              AddDown := False;
            end;

          if GetAsyncKeyState(VK_SUBTRACT) <> 0 then
            begin
              if not SubDown then
                begin
                  SubDown := TRUE;
                  Inc(ObjectsRadius, -1);
                  while FormMain.Thread.RenderingBlocks do
                    begin
                      Sleep(1);
                    end;
                  CreateObjects();
                end;
            end
          else
            begin
              SubDown := False;
            end;
        end;
    end;
  end;

procedure TControlThread.CheckCollision;
  begin
    if NOCLIP or FLY then
      begin
        Jumping := False;
        Exit;
      end;
    with FormMain do
      begin
        IncF(Motion.Y, Gravity / 60);
        if (Objects3d.GetBlockId(Trunc(Position.X), Trunc(Motion.Y + Position.Y - PlayerSize), Trunc(Position.Z)) <> 0)
        then
          begin
            Motion.Y := 0;
            Jumping := False;
          end;
        if (Objects3d.GetBlockId(Trunc(Position.X + Motion.X), Trunc(Position.Y - PlayerSize), Trunc(Position.Z)) <> 0)
        then
          begin
            Motion.X := 0;
          end;
        if (Objects3d.GetBlockId(Trunc(Position.X), Trunc(Position.Y - PlayerSize), Trunc(Position.Z + Motion.Z)) <> 0)
        then
          begin
            Motion.Z := 0;
          end;
      end;
  end;

procedure TControlThread.ApplyMotion;
  begin
    with FormMain do
      begin
        IncF(Position.X, Motion.X);
        IncF(Position.Y, Motion.Y);
        IncF(Position.Z, Motion.Z);

        Motion.X := 0;
        if NOCLIP or FLY then
          Motion.Y := 0;
        Motion.Z := 0;
      end;
  end;

procedure TFormMain.FormCreate(Sender: TObject);
  begin
    Thread := TRenderThread.Create;
    ShowCursor(False);
  end;

function TControlThread.Velocity: Double;
  begin
    Result := Speed * (16 / MSecsPerSec);
  end;

procedure TRenderThread.Render;
  var
    XDelta, YDelta: Integer;
    I, J, K, L, M, N: Integer;
    O: TGenericBlock;
    C: COLORREF;
  begin
    with FormMain do
      begin
        XDelta := Mouse.CursorPos.X - OldCursorPos.X;
        YDelta := Mouse.CursorPos.Y - OldCursorPos.Y;

        SetCursorPos(Screen.Width div 2, Screen.Height div 2);
        GetCursorPos(OldCursorPos);

        IncF(XRotation, (YDelta / Screen.Height) * FOV);
        IncF(YRotation, (XDelta / Screen.Width) * FOV);

        ClampF(XRotation, -YRotationMax, YRotationMax);

        glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

        glMatrixMode(GL_PROJECTION);
        glLoadIdentity;
        gluPerspective(FOV, ClientWidth / ClientHeight, NearClipping, FarClipping);

        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity;

        glRotated(FormMain.XRotation, 1, 0, 0);
        glRotated(FormMain.YRotation, 0, 1, 0);

        glTranslated( -Position.X, -Position.Y, -Position.Z);

        if not UpdatingObjects then
          begin
            RenderingBlocks := TRUE;
            for L := 0 to Length(Objects3d.BackingArray) - 1 do
              for M := 0 to Length(Objects3d.BackingArray[L]) - 1 do
                for N := 0 to Length(Objects3d.BackingArray[L, M]) - 1 do
                  begin
                    O := Objects3d.GetBlock(L, M, N);
                    if (O <> nil) and (O.Cube.Position.DistanceTo(Position.X, Position.Y, Position.Z) < 30) then
                      begin
                        if (Objects3d.GetBlockId(O.X - 1, O.Y, O.Z) = 0) or
                          (Objects3d.GetBlockId(O.X + 1, O.Y, O.Z) = 0) or (Objects3d.GetBlockId(O.X, O.Y - 1, O.Z) = 0)
                          or (Objects3d.GetBlockId(O.X, O.Y + 1, O.Z) = 0) or
                          (Objects3d.GetBlockId(O.X, O.Y, O.Z - 1) = 0) or (Objects3d.GetBlockId(O.X, O.Y, O.Z + 1) = 0)
                        // Only render blocks that are visible
                        // TODO don't recalculate this every frame
                        then
                          begin
                            O.Render;
                          end;
                      end;
                  end;
            RenderingBlocks := False;
          end;

        if RenderNoise then
          begin
            for I := Round(Position.X - 10) to Round(Position.X + 10) do
              for J := Round(Position.Y - 10) to Round(Position.Y + 10) do
                for K := Round(Position.Z - 10) to Round(Position.Z + 10) do
                  begin
                    if Noise.Eval(I / NoiseZoomLevel, J / NoiseZoomLevel, K / NoiseZoomLevel) >= NoiseThreshold then
                      begin
                        P.SetPosition(I, J, K);
                        C := HSVToRGB(Floor(P.Position.DistanceTo(Position.X, Position.Y, Position.Z) * 13), 255, 255);
                        P.SetColor(GetRValue(C) / 255, GetGValue(C) / 255, GetBValue(C) / 255, 1);
                        P.Render;
                      end;
                  end;
          end;
        glFlush;
        glFinish;
        SwapBuffers(DC);
      end;
  end;

procedure TRenderThread.Execute;
  var
    StartTime: Cardinal;
    Control: TControlThread;
  begin
    Randomize;
    FormMain.Position := TCustomVector3d.Create(7.5, 22.7, 7.5);
    FormMain.Motion := TCustomVector3d.Create(0, 0, 0);
    FormMain.XRotation := 0;
    FormMain.YRotation := 180;

    DC := GetDC(FormMain.Handle);
    if not InitOpenGL then
      Application.Terminate;

    RC := CreateRenderingContext(DC, [opDoubleBuffered], 32, 24, 0, 0, 0, 0);
    ActivateRenderingContext(DC, RC);

    glClearColor(0.5, 0.5, 0.78, 1);
    glClearDepth(1.0);
    glShadeModel(GL_SMOOTH);
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LEQUAL);
    glEnable(GL_MULTISAMPLE);
    glEnable(GL_BLEND);
    glEnable(GL_ALPHA_TEST);
    glAlphaFunc(GL_GREATER, 0.1);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
    glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);

    // wglSwapIntervalEXT(0);

    glViewport(0, 0, FormMain.ClientWidth, FormMain.ClientHeight);

    Objects3d := TBlockMap.Create;

    Noise := TOpenSimplexNoise.Create(0);
    P := TCube3d.Create(0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1);

    CreateObjects;

    Control := TControlThread.Create;

    while (not CheckTerminated) do
      begin
        StartTime := GetTickCount;
        if FormMain.Active then
          begin
            Render;
          end;
        FrameTime := GetTickCount - StartTime + 1;
        { if FrameTime < 15 then
          begin
          Sleep(15 - FrameTime);
          end; }
        DeltaTime := GetTickCount - StartTime + 1;
      end;

    FormMain.Motion.FreeInstance;
    FormMain.Position.FreeInstance;

    Control.Terminate;
    Control.FreeInstance;

    Noise.FreeInstance;

    Objects3d.FreeInstance;

    P.FreeInstance;

    DeactivateRenderingContext;
    DestroyRenderingContext(RC);
    ReleaseDC(Handle, DC);
  end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    if Key = VK_ESCAPE then
      begin
        Thread.Terminate;
        Thread.FreeInstance;
        Application.ProcessMessages;
        Sleep(250);
        Close;
      end;
  end;

procedure TFormMain.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
  begin
    IncF(Speed, WheelDelta / 100);
    Speed := Max(Speed, 0.1);
  end;

end.
