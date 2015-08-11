unit mGenericBlock;

interface

uses
  m3DObjects;

type
  TGenericBlock = class(TObject)
    constructor Create(X, Y, Z, id: Integer); overload;
    procedure FreeInstance; override;
    procedure Render;
    public
      id: Integer;
      Cube: TCube3d;
      X, Y, Z: Integer;
  end;

implementation

constructor TGenericBlock.Create(X, Y, Z, id: Integer);
  begin
    Cube := TCube3d.Create(X, Y, Z, 1, 1, 1, 0, 0, 0, Random, Random, Random, 1);
    Self.X := X;
    Self.Y := Y;
    Self.Z := Z;
    Self.id := id;
  end;

procedure TGenericBlock.FreeInstance;
  begin
    Cube.FreeInstance;
    inherited FreeInstance;
  end;

procedure TGenericBlock.Render;
  begin
    Cube.Render;
  end;

end.
