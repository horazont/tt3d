unit TerrainGeometry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GLGeometry, Geometry, dglOpenGL;

const
  TERRAIN_SECTION_WIDTH = 32;
  TERRAIN_SECTION_HEIGHT = 32;

type
  ETerrainError = class (Exception);

  { TTerrain }

  TTerrain = class (TObject)
  public
    constructor Create(const AWidth, AHeight: Integer);
    destructor Destroy; override;
  private
    FBuffer: TGLGeometryBuffer;
    FFormat: TGLGeometryFormatP4C4;
    FSections: array of TGLGeometryTerrainSectionForTris;
    FWidth, FHeight: Integer;
    FStreamIndexBuffer: TGLStreamIndexBuffer;
    FStaticIndexBuffer: TGLIndexBuffer;
  public
    procedure BindForRendering;
    procedure Render;
    procedure UnbindForRendering;
  end;

implementation

function Noise(const X, Y: Integer): Double;
var
  N: Integer;
begin
  N := x + y * 57;
  N := (N shl 13) xor N;
  Exit(1.0 - ( ( n * ( n * n * 15731 + 789221) + 1376312589) and $7fffffff) / 1073741824.0);
end;

(*    corners = ( Noise(x-1, y-1)+Noise(x+1, y-1)+Noise(x-1, y+1)+Noise(x+1, y+1) ) / 16
    sides   = ( Noise(x-1, y)  +Noise(x+1, y)  +Noise(x, y-1)  +Noise(x, y+1) ) /  8
    center  =  Noise(x, y) / 4
    return corners + sides + center
*)

function SmoothedNoise(const X, Y: Integer): Double;
var
  Corners, Sides, Center: Double;
begin
  Corners := (Noise(X-1, Y-1) + Noise(X+1, Y-1) + Noise(X-1, Y+1) + Noise(X+1, Y+1)) / 16.0;
  Sides := (Noise(X-1, Y) + Noise(X+1, Y) + Noise(X, Y-1) + Noise(X, Y+1)) / 8.0;
  Center := Noise(X, Y) / 4.0;
  Exit(Corners + Sides + Center);
end;

function Interpolate_Lin(const V1, V2: Double; const F: Double): Double; inline;
begin
  Exit(V1 * F + V2 * (1.0-F));
end;

function Interpolate_Cos(const V1, V2: Double; const F: Double): Double; inline;
begin
  Exit(Interpolate_Lin(V1, V2, Cos(F * Pi / 2.0)));
end;

function InterpolatedNoise(const X, Y: Double): Double;
var
  IX, IY: Integer;
  FX, FY: Double;
  V: array [0..3] of Double;
  IV: array [0..1] of Double;
begin
  IX := Trunc(X);
  FX := frac(X);
  IY := Trunc(Y);
  FY := frac(Y);

  V[0] := SmoothedNoise(IX, IY);
  V[1] := SmoothedNoise(IX + 1, IY);
  V[2] := SmoothedNoise(IX, IY + 1);
  V[3] := SmoothedNoise(IX + 1, IY + 1);

  IV[0] := Interpolate_Cos(V[0], V[1], FX);
  IV[1] := Interpolate_Cos(V[2], V[3], FX);

  Exit(Interpolate_Cos(IV[0], IV[1], FY));
end;

function Perlin2D(Pos: TVector2; const Persistence: Double; const Octaves: Word): Double;
var
  n: Integer;
  I: Integer;
  f, a: Double;
begin
  Assert(Octaves > 0);
  n := Octaves - 1;
  Result := 0.0;
  f := 1.0;
  a := 1.0;
  for I := 0 to n do
  begin
    Result += InterpolatedNoise(Pos.X * f, Pos.Y * f) * a;
    f *= 2.0;
    a *= Persistence;
  end;
end;

{ TTerrain }

constructor TTerrain.Create(const AWidth, AHeight: Integer);
var
  I, C, X, Y, XRoot, YRoot: Integer;
  Z: Double;
begin
  if (AWidth mod TERRAIN_SECTION_WIDTH <> 0) or (AHeight mod TERRAIN_SECTION_HEIGHT <> 0) then
    raise ETerrainError.CreateFmt('Invalid terrain size %dx%d. Must be multiples of 64.', [AWidth, AHeight]);
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
  C := (AWidth div TERRAIN_SECTION_WIDTH) * (AHeight div TERRAIN_SECTION_HEIGHT);
  SetLength(FSections, C);

  FBuffer := TGLGeometryBuffer.Create(TGLGeometryFormatP4C4T2N3F.GetNeededVertexSize);
  FFormat := TGLGeometryFormatP4C4T2N3F.Create(FBuffer);
  FStreamIndexBuffer := TGLStreamIndexBuffer.Create;
  FStaticIndexBuffer := TGLIndexBuffer.Create;

  XRoot := 0;
  YRoot := 0;
  for I := 0 to High(FSections) do
  begin
    FSections[I] := TGLGeometryTerrainSectionForTris.Create(FBuffer, FFormat,
      TERRAIN_SECTION_WIDTH + 1, TERRAIN_SECTION_HEIGHT + 1, FStaticIndexBuffer,
      FStreamIndexBuffer);

    with TGLGeometryFormatP4C4T2N3F(FSections[I].Format) do
    begin
      UseMap(FSections[I].Map);
      for Y := 0 to TERRAIN_SECTION_HEIGHT do
        for X := 0 to TERRAIN_SECTION_WIDTH do
        begin
          //Z := (Sin((XRoot + X) / 32.0) * Cos((YRoot + Y) / 32.0) + 1.0) * 2.0 + (Random - 0.5) / 8.0;
          Z := Perlin2D(Vector2(XRoot + X, YRoot + Y) / 5.0, 0.8, 5) + 1.0;
          Position[X + Y * (TERRAIN_SECTION_WIDTH+1)] := Vector4(XRoot + X, YRoot + Y, Z, 1.0);
          Z /= 2.0;
          Color[X + Y * (TERRAIN_SECTION_WIDTH+1)] := Vector4(Z, Z, Z, 1.0);
        end;
    end;

    Inc(XRoot, TERRAIN_SECTION_WIDTH);
    if XRoot >= AWidth then
    begin
      Inc(YRoot, TERRAIN_SECTION_HEIGHT);
      XRoot := 0;
    end;
  end;
  FFormat.UseMap(nil);
end;

destructor TTerrain.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FSections) do
    FSections[I].Free;
  FStaticIndexBuffer.Free;
  FStreamIndexBuffer.Free;
  FFormat.Free;
  FBuffer.Free;
  inherited Destroy;
end;

procedure TTerrain.BindForRendering;
begin
  FStaticIndexBuffer.BindForRendering;
  FBuffer.BindForRendering;
  FFormat.BindGLPointer;
end;

procedure TTerrain.Render;
begin
  FStaticIndexBuffer.Draw(GL_TRIANGLES);
end;

procedure TTerrain.UnbindForRendering;
begin
  FFormat.UnbindGLPointer;
  FBuffer.UnbindForRendering;
  FStaticIndexBuffer.UnbindForRendering;
end;

end.

