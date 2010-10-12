unit TerrainGeometry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GLGeometry, Geometry, dglOpenGL;

const
  TERRAIN_SECTION_WIDTH = 128;
  TERRAIN_SECTION_HEIGHT = 128;

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
          Z := (Sin((XRoot + X) / 32.0) * Cos((YRoot + Y) / 32.0) + 1.0) * 2.0 + (Random - 0.5) / 8.0;
          Position[X + Y * (TERRAIN_SECTION_WIDTH+1)] := Vector4(XRoot + X, YRoot + Y, Z, 1.0);
          Z /= 3.0;
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

