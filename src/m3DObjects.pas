unit m3DObjects;

interface

uses
  dglOpenGL,
  Math;

type
  TCustomVector3d = class(TObject)
    X, Y, Z: Double;
    constructor Create(X, Y, Z: Double);
  end;

  TCustomColor4d = class(TObject)
    R, G, B, A: Double;
    constructor Create(R, G, B, A: Double);
  end;

  TCustomObject3d = class(TObject)
    FPosition, FRotation: TCustomVector3d;
    FWidth, FHeight, FDepth: Double;
    FColor: TCustomColor4d;
    constructor Create; overload;
    constructor Create(X, Y, Z, Width, Height, Depth, Rx, Ry, Rz, R, G, B, A: Double); overload;
    procedure Render; virtual;
    // Position
    procedure SetPosition(X, Y, Z: Double); overload;
    procedure SetPosition(Vector: TCustomVector3d); overload;

    procedure Translate(X, Y, Z: Double); overload;
    procedure Translate(Vector: TCustomVector3d); overload;

    function GetPosition: TCustomVector3d;

    property Position: TCustomVector3d
      read GetPosition
      write SetPosition;

    // Rotation
    procedure SetRotation(X, Y, Z: Double); overload;
    procedure SetRotation(Vector: TCustomVector3d); overload;

    procedure Rotate(X, Y, Z: Double); overload;
    procedure Rotate(Vector: TCustomVector3d); overload;

    function GetRotation: TCustomVector3d;

    property Rotation: TCustomVector3d
      read GetRotation
      write SetRotation;

    // Width
    procedure SetWidth(NewWidth: Double);
    function GetWidth: Double;

    property Width: Double
      read GetWidth
      write SetWidth;

    // Height
    procedure SetHeight(NewHeight: Double);
    function GetHeight: Double;

    property Height: Double
      read GetHeight
      write SetHeight;

    // Depth
    procedure SetDepth(NewDepth: Double);
    function GetDepth: Double;

    property Depth: Double
      read GetDepth
      write SetDepth;

    // Color
    procedure SetColor(R, G, B, A: Double); overload;
    procedure SetColor(Color: TCustomColor4d); overload;
    function GetColor: TCustomColor4d;

    property Color: TCustomColor4d
      read GetColor
      write SetColor;
  end;

  TTriangle3d = class(TCustomObject3d)
    public
      procedure Render; override;
  end;

  TPlane3d = class(TCustomObject3d)
    public
      procedure Render; override;
  end;

  TCube3d = class(TCustomObject3d)
    public
      procedure Render; override;
  end;

implementation

procedure IncF(var Base: Double; Add: Double);
  begin
    Base := Base + Add;
  end;

// TCustomVector3d
constructor TCustomVector3d.Create(X, Y, Z: Double);
  begin
    Self.X := X;
    Self.Y := Y;
    Self.Z := Z;
  end;

// TCustomColor4d
constructor TCustomColor4d.Create(R, G, B, A: Double);
  begin
    Self.R := Min(Max(R, 0), 1);
    Self.G := Min(Max(B, 0), 1);
    Self.B := Min(Max(G, 0), 1);
    Self.A := Min(Max(A, 0), 1);
  end;

// TCustomObject3d
constructor TCustomObject3d.Create;
  begin
    SetPosition(0, 0, 0);
    SetRotation(0, 0, 0);
    SetWidth(0);
    SetHeight(0);
    SetDepth(0);
    SetColor(1, 1, 1, 1);
  end;

constructor TCustomObject3d.Create(X, Y, Z, Width, Height, Depth, Rx, Ry, Rz, R, G, B, A: Double);
  begin
    SetPosition(X, Y, Z);
    SetRotation(Rx, Ry, Rz);
    SetWidth(Width);
    SetHeight(Height);
    SetDepth(Depth);
    SetColor(R, G, B, A);
  end;

procedure TCustomObject3d.Render;
  begin

  end;

procedure TCustomObject3d.SetPosition(Vector: TCustomVector3d);
  begin
    SetPosition(Vector.X, Vector.Y, Vector.Z);
  end;

procedure TCustomObject3d.SetPosition(X, Y, Z: Double);
  begin
    if FPosition <> nil then
      begin
        FPosition.X := X;
        FPosition.Y := Y;
        FPosition.Z := Z;
      end
    else
      FPosition := TCustomVector3d.Create(X, Y, Z);
  end;

procedure TCustomObject3d.Translate(Vector: TCustomVector3d);
  begin
    Translate(Vector.X, Vector.Y, Vector.Z);
  end;

procedure TCustomObject3d.Translate(X, Y, Z: Double);
  begin
    IncF(FPosition.X, X);
    IncF(FPosition.Y, Y);
    IncF(FPosition.Z, Z);
  end;

function TCustomObject3d.GetPosition: TCustomVector3d;
  begin
    Result := FPosition;
  end;

procedure TCustomObject3d.SetRotation(Vector: TCustomVector3d);
  begin
    SetRotation(Vector.X, Vector.Y, Vector.Z);
  end;

procedure TCustomObject3d.SetRotation(X, Y, Z: Double);
  begin
    if FRotation <> nil then
      begin
        FRotation.X := FMod(X, 360);
        FRotation.Y := FMod(Y, 360);
        FRotation.Z := FMod(Z, 360);
      end
    else
      FRotation := TCustomVector3d.Create(FMod(X, 360), FMod(Y, 360), FMod(Z, 360));
  end;

procedure TCustomObject3d.Rotate(Vector: TCustomVector3d);
  begin
    Rotate(Vector.X, Vector.Y, Vector.Z);
  end;

procedure TCustomObject3d.Rotate(X, Y, Z: Double);
  begin
    FRotation.X := FMod(FRotation.X + X, 360);
    FRotation.Y := FMod(FRotation.Y + Y, 360);
    FRotation.Z := FMod(FRotation.Z + Z, 360);
  end;

function TCustomObject3d.GetRotation: TCustomVector3d;
  begin
    Result := FRotation;
  end;

procedure TCustomObject3d.SetWidth(NewWidth: Double);
  begin
    FWidth := NewWidth;
  end;

function TCustomObject3d.GetWidth: Double;
  begin
    Result := FWidth;
  end;

procedure TCustomObject3d.SetHeight(NewHeight: Double);
  begin
    FHeight := NewHeight;
  end;

function TCustomObject3d.GetHeight: Double;
  begin
    Result := FHeight;
  end;

procedure TCustomObject3d.SetDepth(NewDepth: Double);
  begin
    FDepth := NewDepth;
  end;

function TCustomObject3d.GetDepth: Double;
  begin
    Result := FDepth;
  end;

procedure TCustomObject3d.SetColor(R, G, B, A: Double);
  begin
    if FColor <> nil then
      begin
        FColor.R := Min(Max(R, 0), 1);
        FColor.G := Min(Max(G, 0), 1);
        FColor.B := Min(Max(B, 0), 1);
        FColor.A := Min(Max(A, 0), 1);
      end
    else
      FColor := TCustomColor4d.Create(R, G, B, A);
  end;

procedure TCustomObject3d.SetColor(Color: TCustomColor4d);
  begin
    SetColor(Color.R, Color.G, Color.B, Color.A);
  end;

function TCustomObject3d.GetColor: TCustomColor4d;
  begin
    Result := FColor;
  end;

// TPlane3d
procedure TPlane3d.Render;
  begin
    glPushMatrix;
    glRotated(Rotation.X, 1, 0, 0);
    glRotated(Rotation.Y, 0, 1, 0);
    glRotated(Rotation.Z, 0, 0, 1);
    glTranslated(Position.X, Position.Y, Position.Z);
    glBegin(GL_QUADS);
    begin
      glColor4d(Color.R, Color.G, Color.B, Color.A);
      glVertex3d(0, 0, 0);
      glVertex3d(0, Height, 0);
      glVertex3d(Width, Height, 0);
      glVertex3d(Width, 0, 0);
    end;
    glEnd;
    glPopMatrix;
  end;

// TTriangle3d
procedure TTriangle3d.Render;
  begin
    glPushMatrix;
    glRotated(Rotation.X, 1, 0, 0);
    glRotated(Rotation.Y, 0, 1, 0);
    glRotated(Rotation.Z, 0, 0, 1);
    glTranslated(Position.X, Position.Y, Position.Z);
    glBegin(GL_TRIANGLES);
    begin
      glColor4d(Color.R, Color.G, Color.B, Color.A);
      glVertex3d(0, 0, 0);
      glVertex3d(Width, 0, 0);
      glVertex3d(0, Height, 0);
    end;
    glEnd;
    glPopMatrix;
  end;

// TCube3d
procedure TCube3d.Render;
  begin
    glPushMatrix;
    glRotated(Rotation.X, 1, 0, 0);
    glRotated(Rotation.Y, 0, 1, 0);
    glRotated(Rotation.Z, 0, 0, 1);
    glTranslated(Position.X, Position.Y, Position.Z);
    glBegin(GL_QUADS);
    begin
      glColor4d(Color.R, Color.G, Color.B, Color.A);

      // Top
      glVertex3d(0, Height, 0);
      glVertex3d(0, Height, Depth);
      glVertex3d(Width, Height, Depth);
      glVertex3d(Width, Height, 0);

      // Bottom
      glVertex3d(0, 0, 0);
      glVertex3d(Width, 0, 0);
      glVertex3d(Width, 0, Depth);
      glVertex3d(0, 0, Depth);

      // Front
      glVertex3d(0, 0, 0);
      glVertex3d(0, Height, 0);
      glVertex3d(Width, Height, 0);
      glVertex3d(Width, 0, 0);

      // Back
      glVertex3d(0, 0, Depth);
      glVertex3d(Width, 0, Depth);
      glVertex3d(Width, Height, Depth);
      glVertex3d(0, Height, Depth);

      // Left
      glVertex3d(0, 0, 0);
      glVertex3d(0, 0, Depth);
      glVertex3d(0, Height, Depth);
      glVertex3d(0, Height, 0);

      // Right
      glVertex3d(Width, 0, 0);
      glVertex3d(Width, Height, 0);
      glVertex3d(Width, Height, Depth);
      glVertex3d(Width, 0, Depth);
    end;
    glEnd;
    glPopMatrix;
  end;

end.
