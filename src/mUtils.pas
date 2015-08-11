unit mUtils;

interface

uses
  Math,
  SysUtils,
  mGenericBlock;

type
  TOpenSimplexNoise = class(TObject)
    private
      Perm: array [0 .. 255] of SmallInt;
      PermGradIndex3D: array [0 .. 255] of SmallInt;
    public
      constructor Create; overload;
      constructor Create(Perm: array of SmallInt); overload;
      constructor Create(Seed: Int64); overload;
      function Eval(X, Y, Z: Double): Double;
      function Extrapolate(xsb, ysb, zsb: Integer; dx, dy, dz: Double): Double;
  end;

type
  TBlockMap = class(TObject)
    BackingArray: array of array of array of TGenericBlock;
    constructor Create; overload;
    public
      function GetBlockId(X, Y, Z: Integer): Integer;
      function GetBlock(X, Y, Z: Integer): TGenericBlock;
      procedure SetBlock(X, Y, Z: Integer; Block: TGenericBlock);
      procedure FreeInstance; override;
  end;

const
  DEFAULT_SEED: Int64 = 0;
  NORM_CONSTANT_3D: Double = 103;
  STRETCH_CONSTANT_3D: Double = -1.0 / 6;
  SQUISH_CONSTANT_3D: Double = 1.0 / 3;

  PermGradIndex3DLength = 256;
  PermLength = 256;
  Gradients3D: array [0 .. 71] of ShortInt = ( -11, 4, 4, -4, 11, 4, -4, 4, 11, 11, 4, 4, 4, 11, 4, 4, 4, 11, -11, -4,
    4, -4, -11, 4, -4, -4, 11, 11, -4, 4, 4, -11, 4, 4, -4, 11, -11, 4, -4, -4, 11, -4, -4, 4, -11, 11, 4, -4, 4, 11,
    -4, 4, 4, -11, -11, -4, -4, -4, -11, -4, -4, -4, -11, 11, -4, -4, 4, -11, -4, 4, -4, -11);

implementation

procedure IncF(var Base: Double; Add: Double);
  begin
    Base := Base + Add;
  end;

constructor TBlockMap.Create;
  begin
    SetLength(BackingArray, 1, 1, 1);
  end;

procedure TBlockMap.FreeInstance;
  var
    I, J, K: Integer;
  begin
    for I := Low(BackingArray) to High(BackingArray) do
      for J := Low(BackingArray[I]) to High(BackingArray[I]) do
        for K := Low(BackingArray[I, J]) to High(BackingArray[I, J]) do
          begin
            if Assigned(BackingArray[I, J, K]) then
              begin
                BackingArray[I, J, K].FreeInstance;
              end;
          end;

    inherited FreeInstance;
  end;

procedure TBlockMap.SetBlock(X, Y, Z: Integer; Block: TGenericBlock);
  begin
    SetLength(BackingArray, Max(Length(BackingArray), X + 1), Max(Length(BackingArray[0]), Y + 1),
      Max(Length(BackingArray[0][0]), Z + 1));
    BackingArray[X, Y, Z] := Block;
  end;

function TBlockMap.GetBlock(X, Y, Z: Integer): TGenericBlock;
  begin
    Result := nil;
    if (X < Length(BackingArray)) and (Y < Length(BackingArray[0])) and (Z < Length(BackingArray[0, 0])) and
      (X >= 0) and (Y >= 0) and (Z >= 0) then
      begin
        Result := BackingArray[X, Y, Z];
      end;
  end;

function TBlockMap.GetBlockId(X, Y, Z: Integer): Integer;
  begin
    Result := 0;
    if GetBlock(X, Y, Z) <> nil then
      begin
        Result := GetBlock(X, Y, Z).id;
      end;
  end;

// TOpenSimplexNoise
constructor TOpenSimplexNoise.Create;
  begin
    Create(DEFAULT_SEED);
  end;

constructor TOpenSimplexNoise.Create(Perm: array of SmallInt);
  var
    I: Integer;
  begin
    CopyArray(@Self.Perm, @Perm, TypeInfo(SmallInt), Length(Perm));
    FillChar(PermGradIndex3D, PermGradIndex3DLength, 0);
    for I := 0 to PermGradIndex3DLength - 1 do
      begin
        PermGradIndex3D[I] := Round(FMod(Perm[I], (Length(Gradients3D) / 3) * 3));
      end;
  end;

constructor TOpenSimplexNoise.Create(Seed: Int64);
  var
    Source: array [0 .. 255] of SmallInt;
    I, R: Integer;
  begin
    for I := 0 to Length(Source) - 1 do
      begin
        Source[I] := I;
      end;
    Seed := Seed * 6364136223846793005 + 1442695040888963407;
    Seed := Seed * 6364136223846793005 + 1442695040888963407;
    Seed := Seed * 6364136223846793005 + 1442695040888963407;

    for I := Length(Source) - 1 downto 0 do
      begin
        Seed := Seed * 6364136223846793005 + 1442695040888963407;
        R := Integer((Seed + 31) mod (I + 1));
        if R < 0 then
          begin
            Inc(R, I + 1);
          end;
        Perm[I] := Source[R];
        PermGradIndex3D[I] := Round(FMod(Perm[I], (Length(Gradients3D) / 3) * 3));
        Source[R] := Source[I];
      end;
  end;

function TOpenSimplexNoise.Extrapolate(xsb, ysb, zsb: Integer; dx, dy, dz: Double): Double;
  var
    Index: Integer;
  begin
    Index := PermGradIndex3D[(Perm[(Perm[xsb and $FF] + ysb) and $FF] + zsb) and $FF];
    Result := Gradients3D[index] * dx + Gradients3D[index + 1] * dy + Gradients3D[index + 2] * dz;
  end;

function TOpenSimplexNoise.Eval(X, Y, Z: Double): Double;
  var
    StretchOffset, Xs, Ys, Zs, SquishOffset, Xb, Yb, Zb, Xins, Yins, Zins, Wins, InSum, Dx0, Dy0, Dz0, Dx_ext0, Dy_ext0,
      Dz_ext0, Dx_ext1, Dy_ext1, Dz_ext1, Value, AScore, BScore, Attn0, Dx1, Dy1, Dz1, Attn1, Dx2, Dy2, Dz2, Attn2, Dx3,
      Dy3, Dz3, Attn3, P1, P2, P3, Score, Dx4, Dy4, Dz4, Attn4, Dx5, Dy5, Dz5, Attn5, Dx6, Dy6, Dz6, Attn6, Attn_Ext0,
      Attn_Ext1: Double;
    xsb, ysb, zsb, Xsv_ext0, Ysv_ext0, Zsv_ext0, Xsv_ext1, Ysv_ext1, Zsv_ext1: Integer;
    APoint, BPoint, C, C1, C2: Byte;
    AIsFurtherSide, BISFurtherSide: Boolean;
  begin
    StretchOffset := (X + Y + Z) * STRETCH_CONSTANT_3D;
    Xs := X + StretchOffset;
    Ys := Y + StretchOffset;
    Zs := Z + StretchOffset;

    xsb := Floor(Xs);
    ysb := Floor(Ys);
    zsb := Floor(Zs);

    SquishOffset := (xsb + ysb + zsb) * SQUISH_CONSTANT_3D;
    Xb := xsb + SquishOffset;
    Yb := ysb + SquishOffset;
    Zb := zsb + SquishOffset;

    Xins := Xs - xsb;
    Yins := Ys - ysb;
    Zins := Zs - zsb;

    InSum := Xins + Yins + Zins;

    Dx0 := X - Xb;
    Dy0 := Y - Yb;
    Dz0 := Z - Zb;

    Value := 0;
    if InSum <= 1 then
      begin
        APoint := $01;
        AScore := Xins;
        BPoint := $02;
        BScore := Yins;

        if (AScore >= BScore) and (Zins > BScore) then
          begin
            BScore := Zins;
            BPoint := $04;
          end
        else if (AScore < BScore) and (Zins > AScore) then
          begin
            AScore := Zins;
            APoint := $04;
          end;

        Wins := 1 - InSum;
        if (Wins > AScore) or (Wins > BScore) then
          begin
            C := IfThen(BScore > AScore, BPoint, APoint);

            if (C and $01) = 0 then
              begin
                Xsv_ext0 := xsb - 1;
                Xsv_ext1 := xsb;
                Dx_ext0 := Dx0 + 1;
                Dx_ext1 := Dx0;
              end
            else
              begin
                Xsv_ext1 := xsb + 1;
                Xsv_ext0 := Xsv_ext1;
                Dx_ext1 := Dx0 - 1;
                Dx_ext0 := Dx_ext1;
              end;

            if (C and $02) = 0 then
              begin
                Ysv_ext1 := ysb;
                Ysv_ext0 := Ysv_ext1;
                Dy_ext1 := Dy0;
                Dy_ext0 := Dy_ext1;

                if (C and $01) = 0 then
                  begin
                    Inc(Ysv_ext1, -1);
                    IncF(Dy_ext1, 1);
                  end
                else
                  begin
                    Inc(Ysv_ext0, -1);
                    IncF(Dy_ext0, 1);
                  end;
              end
            else
              begin
                Ysv_ext1 := ysb + 1;
                Ysv_ext0 := Ysv_ext1;
                Dy_ext1 := Dy0 - 1;
                Dy_ext0 := Dy_ext1;
              end;

            if (C and $04) = 0 then
              begin
                Zsv_ext0 := zsb;
                Zsv_ext1 := zsb - 1;
                Dz_ext0 := Dz0;
                Dz_ext1 := Dz0 + 1;
              end
            else
              begin
                Zsv_ext1 := zsb + 1;
                Zsv_ext0 := Zsv_ext1;
                Dz_ext1 := Dz0 - 1;
                Dz_ext0 := Dz_ext1;
              end;
          end
        else
          begin
            C := APoint or BPoint;

            if (C and $01) = 0 then
              begin
                Xsv_ext0 := xsb;
                Xsv_ext1 := xsb - 1;
                Dx_ext0 := Dx0 - 2 * SQUISH_CONSTANT_3D;
                Dx_ext1 := Dx0 + 1 - SQUISH_CONSTANT_3D;
              end
            else
              begin
                Xsv_ext1 := xsb + 1;
                Xsv_ext0 := Xsv_ext1;
                Dx_ext0 := Dx0 - 1 - 2 * SQUISH_CONSTANT_3D;
                Dx_ext1 := Dx0 - 1 - SQUISH_CONSTANT_3D;
              end;

            if (C and $02) = 0 then
              begin
                Ysv_ext0 := ysb;
                Ysv_ext1 := ysb - 1;
                Dy_ext0 := Dy0 - 2 * SQUISH_CONSTANT_3D;
                Dy_ext1 := Dy0 + 1 - SQUISH_CONSTANT_3D;
              end
            else
              begin
                Ysv_ext1 := ysb + 1;
                Ysv_ext0 := Ysv_ext1;
                Dy_ext0 := Dy0 - 1 - 2 * SQUISH_CONSTANT_3D;
                Dy_ext1 := Dy0 - 1 - SQUISH_CONSTANT_3D;
              end;

            if (C and $04) = 0 then
              begin
                Zsv_ext0 := zsb;
                Zsv_ext1 := zsb - 1;
                Dz_ext0 := Dz0 - 2 * SQUISH_CONSTANT_3D;
                Dz_ext1 := Dz0 + 1 - SQUISH_CONSTANT_3D;
              end
            else
              begin
                Zsv_ext1 := zsb + 1;
                Zsv_ext0 := Zsv_ext1;
                Dz_ext0 := Dz0 - 1 - 2 * SQUISH_CONSTANT_3D;
                Dz_ext1 := Dz0 - 1 - SQUISH_CONSTANT_3D;
              end;
          end;

        Attn0 := 2 - Dx0 * Dx0 - Dy0 * Dy0 - Dz0 * Dz0;
        if Attn0 > 0 then
          begin
            Attn0 := Attn0 * Attn0;
            IncF(Value, Attn0 * Attn0 * Extrapolate(xsb + 0, ysb + 0, zsb + 0, Dx0, Dy0, Dz0));
          end;

        Dx1 := Dx0 - 1 - SQUISH_CONSTANT_3D;
        Dy1 := Dy0 - 0 - SQUISH_CONSTANT_3D;
        Dz1 := Dz0 - 0 - SQUISH_CONSTANT_3D;
        Attn1 := 2 - Dx1 * Dx1 - Dy1 * Dy1 - Dz1 * Dz1;

        if Attn1 > 0 then
          begin
            Attn1 := Attn1 * Attn1;
            IncF(Value, Attn1 * Attn1 * Extrapolate(xsb + 1, ysb + 0, zsb + 0, Dx1, Dy1, Dz1));
          end;

        Dx2 := Dx0 - 0 - SQUISH_CONSTANT_3D;
        Dy2 := Dy0 - 1 - SQUISH_CONSTANT_3D;
        Dz2 := Dz1;
        Attn2 := 2 - Dx2 * Dx2 - Dy2 * Dy2 - Dz2 * Dz2;
        if (Attn2 > 0) then
          begin
            Attn2 := Attn2 * Attn2;
            IncF(Value, Attn2 * Attn2 * Extrapolate(xsb + 0, ysb + 1, zsb + 0, Dx2, Dy2, Dz2));
          end;

        Dx3 := Dx2;
        Dy3 := Dy1;
        Dz3 := Dz0 - 1 - SQUISH_CONSTANT_3D;
        Attn3 := 2 - Dx3 * Dx3 - Dy3 * Dy3 - Dz3 * Dz3;
        if (Attn3 > 0) then
          begin
            Attn3 := Attn3 * Attn3;
            IncF(Value, Attn3 * Attn3 * Extrapolate(xsb + 0, ysb + 0, zsb + 1, Dx3, Dy3, Dz3));
          end;
      end
    else if InSum >= 2 then
      begin
        APoint := $06;
        AScore := Xins;
        BPoint := $05;
        BScore := Yins;
        if (AScore <= BScore) and (Zins < BScore) then
          begin
            BScore := Zins;
            BPoint := $03;
          end
        else if (AScore > BScore) and (Zins < AScore) then
          begin
            AScore := Zins;
            APoint := $03;
          end;

        Wins := 3 - InSum;
        if (Wins < AScore) or (Wins < BScore) then
          begin
            C := IfThen(BScore < AScore, BPoint, APoint);

            if (C and $01) <> 0 then
              begin
                Xsv_ext0 := xsb + 2;
                Xsv_ext1 := xsb + 1;
                Dx_ext0 := Dx0 - 2 - 3 * SQUISH_CONSTANT_3D;
                Dx_ext1 := Dx0 - 1 - 3 * SQUISH_CONSTANT_3D;
              end
            else
              begin
                Xsv_ext1 := xsb;
                Xsv_ext0 := Xsv_ext1;
                Dx_ext1 := Dx0 - 3 * SQUISH_CONSTANT_3D;
                Dx_ext0 := Dx_ext1;
              end;

            if (C and $02) <> 0 then
              begin
                Ysv_ext1 := ysb + 1;
                Ysv_ext0 := Ysv_ext1;
                Dy_ext1 := Dy0 - 1 - 3 * SQUISH_CONSTANT_3D;
                Dy_ext0 := Dy_ext1;

                if (C and $01) <> 0 then
                  begin
                    Inc(Ysv_ext1, 1);
                    IncF(Dy_ext1, -1);
                  end
                else
                  begin
                    Inc(Ysv_ext0, 1);
                    IncF(Dy_ext0, -1);
                  end;
              end
            else
              begin
                Ysv_ext1 := ysb;
                Ysv_ext0 := Ysv_ext1;
                Dy_ext1 := Dy0 - 3 * SQUISH_CONSTANT_3D;
                Dy_ext0 := Dy_ext1;
              end;

            if (C and $04) <> 0 then
              begin
                Zsv_ext0 := zsb + 1;
                Zsv_ext1 := zsb + 2;
                Dz_ext0 := Dz0 - 1 - 3 * SQUISH_CONSTANT_3D;
                Dz_ext1 := Dz0 - 2 - 3 * SQUISH_CONSTANT_3D;
              end
            else
              begin
                Zsv_ext1 := zsb;
                Zsv_ext0 := Zsv_ext1;
                Dz_ext1 := Dz0 - 3 * SQUISH_CONSTANT_3D;
                Dz_ext0 := Dz_ext1;
              end;
          end
        else
          begin
            C := APoint and BPoint;

            if (C and $01) <> 0 then
              begin
                Xsv_ext0 := xsb + 1;
                Xsv_ext1 := xsb + 2;
                Dx_ext0 := Dx0 - 1 - SQUISH_CONSTANT_3D;
                Dx_ext1 := Dx0 - 2 - 2 * SQUISH_CONSTANT_3D;
              end
            else
              begin
                Xsv_ext1 := xsb;
                Xsv_ext0 := Xsv_ext1;
                Dx_ext0 := Dx0 - SQUISH_CONSTANT_3D;
                Dx_ext1 := Dx0 - 2 * SQUISH_CONSTANT_3D;
              end;

            if (C and $02) <> 0 then
              begin
                Ysv_ext0 := ysb + 1;
                Ysv_ext1 := ysb + 2;
                Dy_ext0 := Dy0 - 1 - SQUISH_CONSTANT_3D;
                Dy_ext1 := Dy0 - 2 - 2 * SQUISH_CONSTANT_3D;
              end
            else
              begin
                Ysv_ext1 := ysb;
                Ysv_ext0 := Ysv_ext1;
                Dy_ext0 := Dy0 - SQUISH_CONSTANT_3D;
                Dy_ext1 := Dy0 - 2 * SQUISH_CONSTANT_3D;
              end;

            if (C and $04) <> 0 then
              begin
                Zsv_ext0 := zsb + 1;
                Zsv_ext1 := zsb + 2;
                Dz_ext0 := Dz0 - 1 - SQUISH_CONSTANT_3D;
                Dz_ext1 := Dz0 - 2 - 2 * SQUISH_CONSTANT_3D;
              end
            else
              begin
                Zsv_ext1 := zsb;
                Zsv_ext0 := Zsv_ext1;
                Dz_ext0 := Dz0 - SQUISH_CONSTANT_3D;
                Dz_ext1 := Dz0 - 2 * SQUISH_CONSTANT_3D;
              end;
          end;

        Dx3 := Dx0 - 1 - 2 * SQUISH_CONSTANT_3D;
        Dy3 := Dy0 - 1 - 2 * SQUISH_CONSTANT_3D;
        Dz3 := Dz0 - 0 - 2 * SQUISH_CONSTANT_3D;
        Attn3 := 2 - Dx3 * Dx3 - Dy3 * Dy3 - Dz3 * Dz3;
        if (Attn3 > 0) then
          begin
            Attn3 := Attn3 * Attn3;
            IncF(Value, Attn3 * Attn3 * Extrapolate(xsb + 1, ysb + 1, zsb + 0, Dx3, Dy3, Dz3));
          end;

        Dx2 := Dx3;
        Dy2 := Dy0 - 0 - 2 * SQUISH_CONSTANT_3D;
        Dz2 := Dz0 - 1 - 2 * SQUISH_CONSTANT_3D;
        Attn2 := 2 - Dx2 * Dx2 - Dy2 * Dy2 - Dz2 * Dz2;
        if (Attn2 > 0) then
          begin
            Attn2 := Attn2 * Attn2;
            IncF(Value, Attn2 * Attn2 * Extrapolate(xsb + 1, ysb + 0, zsb + 1, Dx2, Dy2, Dz2));
          end;

        Dx1 := Dx0 - 0 - 2 * SQUISH_CONSTANT_3D;
        Dy1 := Dy3;
        Dz1 := Dz2;
        Attn1 := 2 - Dx1 * Dx1 - Dy1 * Dy1 - Dz1 * Dz1;
        if (Attn1 > 0) then
          begin
            Attn1 := Attn1 * Attn1;
            IncF(Value, Attn1 * Attn1 * Extrapolate(xsb + 0, ysb + 1, zsb + 1, Dx1, Dy1, Dz1));
          end;

        Dx0 := Dx0 - 1 - 3 * SQUISH_CONSTANT_3D;
        Dy0 := Dy0 - 1 - 3 * SQUISH_CONSTANT_3D;
        Dz0 := Dz0 - 1 - 3 * SQUISH_CONSTANT_3D;
        Attn0 := 2 - Dx0 * Dx0 - Dy0 * Dy0 - Dz0 * Dz0;
        if (Attn0 > 0) then
          begin
            Attn0 := Attn0 * Attn0;
            IncF(Value, Attn0 * Attn0 * Extrapolate(xsb + 1, ysb + 1, zsb + 1, Dx0, Dy0, Dz0));
          end;
      end
    else
      begin
        P1 := Xins + Yins;
        if (P1 > 1) then
          begin
            AScore := P1 - 1;
            APoint := $03;
            AIsFurtherSide := True;
          end
        else
          begin
            AScore := 1 - P1;
            APoint := $04;
            AIsFurtherSide := False;
          end;

        P2 := Xins + Zins;
        if (P2 > 1) then
          begin
            BScore := P2 - 1;
            BPoint := $05;
            BISFurtherSide := True;
          end
        else
          begin
            BScore := 1 - P2;
            BPoint := $02;
            BISFurtherSide := False;
          end;

        P3 := Yins + Zins;
        if (P3 > 1) then
          begin
            Score := P3 - 1;
            if (AScore <= BScore) and (AScore < Score) then
              begin
                AScore := Score;
                APoint := $06;
                AIsFurtherSide := True;
              end
            else if (AScore > BScore) and (BScore < Score) then
              begin
                BScore := Score;
                BPoint := $06;
                BISFurtherSide := True;
              end;
          end
        else
          begin
            Score := 1 - P3;
            if (AScore <= BScore) and (AScore < Score) then
              begin
                AScore := Score;
                APoint := $01;
                AIsFurtherSide := False;
              end
            else if (AScore > BScore) and (BScore < Score) then
              begin
                BScore := Score;
                BPoint := $01;
                BISFurtherSide := False;
              end;
          end;

        if AIsFurtherSide = BISFurtherSide then
          begin
            if AIsFurtherSide then
              begin
                Dx_ext0 := Dx0 - 1 - 3 * SQUISH_CONSTANT_3D;
                Dy_ext0 := Dy0 - 1 - 3 * SQUISH_CONSTANT_3D;
                Dz_ext0 := Dz0 - 1 - 3 * SQUISH_CONSTANT_3D;
                Xsv_ext0 := xsb + 1;
                Ysv_ext0 := ysb + 1;
                Zsv_ext0 := zsb + 1;

                C := APoint and BPoint;

                if (C and $01) <> 0 then
                  begin
                    Dx_ext1 := Dx0 - 2 - 2 * SQUISH_CONSTANT_3D;
                    Dy_ext1 := Dy0 - 2 * SQUISH_CONSTANT_3D;
                    Dz_ext1 := Dz0 - 2 * SQUISH_CONSTANT_3D;
                    Xsv_ext1 := xsb + 2;
                    Ysv_ext1 := ysb;
                    Zsv_ext1 := zsb;
                  end
                else if (C and $02) <> 0 then
                  begin
                    Dx_ext1 := Dx0 - 2 * SQUISH_CONSTANT_3D;
                    Dy_ext1 := Dy0 - 2 - 2 * SQUISH_CONSTANT_3D;
                    Dz_ext1 := Dz0 - 2 * SQUISH_CONSTANT_3D;
                    Xsv_ext1 := xsb;
                    Ysv_ext1 := ysb + 2;
                    Zsv_ext1 := zsb;
                  end
                else
                  begin
                    Dx_ext1 := Dx0 - 2 * SQUISH_CONSTANT_3D;
                    Dy_ext1 := Dy0 - 2 * SQUISH_CONSTANT_3D;
                    Dz_ext1 := Dz0 - 2 - 2 * SQUISH_CONSTANT_3D;
                    Xsv_ext1 := xsb;
                    Ysv_ext1 := ysb;
                    Zsv_ext1 := zsb + 2;
                  end;
              end
            else
              begin
                Dx_ext0 := Dx0;
                Dy_ext0 := Dy0;
                Dz_ext0 := Dz0;
                Xsv_ext0 := xsb;
                Ysv_ext0 := ysb;
                Zsv_ext0 := zsb;

                C := APoint or BPoint;

                if (C and $01) = 0 then
                  begin
                    Dx_ext1 := Dx0 + 1 - SQUISH_CONSTANT_3D;
                    Dy_ext1 := Dy0 - 1 - SQUISH_CONSTANT_3D;
                    Dz_ext1 := Dz0 - 1 - SQUISH_CONSTANT_3D;
                    Xsv_ext1 := xsb - 1;
                    Ysv_ext1 := ysb + 1;
                    Zsv_ext1 := zsb + 1;
                  end
                else if (C and $02) = 0 then
                  begin
                    Dx_ext1 := Dx0 - 1 - SQUISH_CONSTANT_3D;
                    Dy_ext1 := Dy0 + 1 - SQUISH_CONSTANT_3D;
                    Dz_ext1 := Dz0 - 1 - SQUISH_CONSTANT_3D;
                    Xsv_ext1 := xsb + 1;
                    Ysv_ext1 := ysb - 1;
                    Zsv_ext1 := zsb + 1;
                  end
                else
                  begin
                    Dx_ext1 := Dx0 - 1 - SQUISH_CONSTANT_3D;
                    Dy_ext1 := Dy0 - 1 - SQUISH_CONSTANT_3D;
                    Dz_ext1 := Dz0 + 1 - SQUISH_CONSTANT_3D;
                    Xsv_ext1 := xsb + 1;
                    Ysv_ext1 := ysb + 1;
                    Zsv_ext1 := zsb - 1;
                  end;
              end;
          end
        else
          begin
            if AIsFurtherSide then
              begin
                C1 := APoint;
                C2 := BPoint;
              end
            else
              begin
                C1 := BPoint;
                C2 := APoint;
              end;

            if (C1 and $01) = 0 then
              begin
                Dx_ext0 := Dx0 + 1 - SQUISH_CONSTANT_3D;
                Dy_ext0 := Dy0 - 1 - SQUISH_CONSTANT_3D;
                Dz_ext0 := Dz0 - 1 - SQUISH_CONSTANT_3D;
                Xsv_ext0 := xsb - 1;
                Ysv_ext0 := ysb + 1;
                Zsv_ext0 := zsb + 1;
              end
            else if (C1 and $02) = 0 then
              begin
                Dx_ext0 := Dx0 - 1 - SQUISH_CONSTANT_3D;
                Dy_ext0 := Dy0 + 1 - SQUISH_CONSTANT_3D;
                Dz_ext0 := Dz0 - 1 - SQUISH_CONSTANT_3D;
                Xsv_ext0 := xsb + 1;
                Ysv_ext0 := ysb - 1;
                Zsv_ext0 := zsb + 1;
              end
            else
              begin
                Dx_ext0 := Dx0 - 1 - SQUISH_CONSTANT_3D;
                Dy_ext0 := Dy0 - 1 - SQUISH_CONSTANT_3D;
                Dz_ext0 := Dz0 + 1 - SQUISH_CONSTANT_3D;
                Xsv_ext0 := xsb + 1;
                Ysv_ext0 := ysb + 1;
                Zsv_ext0 := zsb - 1;
              end;

            Dx_ext1 := Dx0 - 2 * SQUISH_CONSTANT_3D;
            Dy_ext1 := Dy0 - 2 * SQUISH_CONSTANT_3D;
            Dz_ext1 := Dz0 - 2 * SQUISH_CONSTANT_3D;
            Xsv_ext1 := xsb;
            Ysv_ext1 := ysb;
            Zsv_ext1 := zsb;

            if (C2 and $01) <> 0 then
              begin
                IncF(Dx_ext1, -2);
                Inc(Xsv_ext1, 2);
              end
            else if (C2 and $02) <> 0 then
              begin
                IncF(Dy_ext1, -2);
                Inc(Ysv_ext1, 2);
              end
            else
              begin
                IncF(Dz_ext1, -2);
                Inc(Zsv_ext1, 2);
              end;
          end;

        Dx1 := Dx0 - 1 - SQUISH_CONSTANT_3D;
        Dy1 := Dy0 - 0 - SQUISH_CONSTANT_3D;
        Dz1 := Dz0 - 0 - SQUISH_CONSTANT_3D;
        Attn1 := 2 - Dx1 * Dx1 - Dy1 * Dy1 - Dz1 * Dz1;

        if Attn1 > 0 then
          begin
            Attn1 := Attn1 * Attn1;
            IncF(Value, Attn1 * Attn1 * Extrapolate(xsb + 1, ysb + 0, zsb + 0, Dx1, Dy1, Dz1));
          end;

        Dx2 := Dx0 - 0 - SQUISH_CONSTANT_3D;
        Dy2 := Dy0 - 1 - SQUISH_CONSTANT_3D;
        Dz2 := Dz1;
        Attn2 := 2 - Dx2 * Dx2 - Dy2 * Dy2 - Dz2 * Dz2;
        if (Attn2 > 0) then
          begin
            Attn2 := Attn2 * Attn2;
            IncF(Value, Attn2 * Attn2 * Extrapolate(xsb + 0, ysb + 1, zsb + 0, Dx2, Dy2, Dz2));
          end;

        Dx3 := Dx2;
        Dy3 := Dy1;
        Dz3 := Dz0 - 1 - SQUISH_CONSTANT_3D;
        Attn3 := 2 - Dx3 * Dx3 - Dy3 * Dy3 - Dz3 * Dz3;
        if (Attn3 > 0) then
          begin
            Attn3 := Attn3 * Attn3;
            IncF(Value, Attn3 * Attn3 * Extrapolate(xsb + 0, ysb + 0, zsb + 1, Dx3, Dy3, Dz3));
          end;

        Dx4 := Dx0 - 1 - 2 * SQUISH_CONSTANT_3D;
        Dy4 := Dy0 - 1 - 2 * SQUISH_CONSTANT_3D;
        Dz4 := Dz0 - 0 - 2 * SQUISH_CONSTANT_3D;
        Attn4 := 2 - Dx4 * Dx4 - Dy4 * Dy4 - Dz4 * Dz4;
        if (Attn4 > 0) then
          begin
            Attn4 := Attn4 * Attn4;
            IncF(Value, Attn4 * Attn4 * Extrapolate(xsb + 1, ysb + 1, zsb + 0, Dx4, Dy4, Dz4));
          end;

        Dx5 := Dx4;
        Dy5 := Dy0 - 0 - 2 * SQUISH_CONSTANT_3D;
        Dz5 := Dz0 - 1 - 2 * SQUISH_CONSTANT_3D;
        Attn5 := 2 - Dx5 * Dx5 - Dy5 * Dy5 - Dz5 * Dz5;
        if (Attn5 > 0) then
          begin
            Attn5 := Attn5 * Attn5;
            IncF(Value, Attn5 * Attn5 * Extrapolate(xsb + 1, ysb + 0, zsb + 1, Dx5, Dy5, Dz5));
          end;

        Dx6 := Dx0 - 0 - 2 * SQUISH_CONSTANT_3D;
        Dy6 := Dy4;
        Dz6 := Dz5;
        Attn6 := 2 - Dx6 * Dx6 - Dy6 * Dy6 - Dz6 * Dz6;
        if (Attn6 > 0) then
          begin
            Attn6 := Attn6 * Attn6;
            IncF(Value, Attn6 * Attn6 * Extrapolate(xsb + 0, ysb + 1, zsb + 1, Dx6, Dy6, Dz6));
          end;
      end;

    Attn_Ext0 := 2 - Dx_ext0 * Dx_ext0 - Dy_ext0 * Dy_ext0 - Dz_ext0 * Dz_ext0;
    if (Attn_Ext0 > 0) then
      begin
        Attn_Ext0 := Attn_Ext0 * Attn_Ext0;
        IncF(Value, Attn_Ext0 * Attn_Ext0 * Extrapolate(Xsv_ext0, Ysv_ext0, Zsv_ext0, Dx_ext0, Dy_ext0, Dz_ext0));
      end;

    Attn_Ext1 := 2 - Dx_ext1 * Dx_ext1 - Dy_ext1 * Dy_ext1 - Dz_ext1 * Dz_ext1;
    if (Attn_Ext1 > 0) then
      begin
        Attn_Ext1 := Attn_Ext1 * Attn_Ext1;
        IncF(Value, Attn_Ext1 * Attn_Ext1 * Extrapolate(Xsv_ext1, Ysv_ext1, Zsv_ext1, Dx_ext1, Dy_ext1, Dz_ext1));
      end;

    Result := Value / NORM_CONSTANT_3D;
  end;

end.
