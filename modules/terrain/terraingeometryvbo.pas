unit TerrainGeometryVBO;
(**********************************************************************
File name: terraingeometryvbo.pas
This file is part of: tt3d

LICENSE

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
License for the specific language governing rights and limitations under
the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public license (the  "GPL License"), in which case  the
provisions of GPL License are applicable instead of those above.

FEEDBACK & QUESTIONS

For feedback and questions about tt3d please e-mail one of the authors:
    Jonas Wielicki <j.wielicki@sotecware.net>
**********************************************************************)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GLGeometry, Geometry, dglOpenGL, TerrainSource,
  GLShaderMaterial, glBitmap, TerrainSourcePerlinNoise, GLBase, ioLog;

const
  NOISE_TEXTURE_SIZE = 256;
  TERRAIN_BLOCK_SIZE = 64;

type

  { TTerrainFormat }

  TTerrainFormat = class (TGLGeometryFormat)
  protected
    class function GetActualVertexSize: Integer; override;
  public
    procedure BindGLPointer; override;
    procedure UnbindGLPointer; override;
  public
    property Position[Index: TVertexIndex]: TVector3f index 0 read GetVec3 write SetVec3;
    property Normal[Index: TVertexIndex]: TVector3f index 3 * SizeOf(Single) read GetVec3 write SetVec3;
    property Tangent[Index: TVertexIndex]: TVector3f index 6 * SizeOf(Single) read GetVec3 write SetVec3;
  end;

  { TTerrainMaterial }

  TTerrainMaterial = class (TGLShaderMaterial)
  public
    constructor Create(const AGeometryBuffer: TGLGeometryBuffer;
       const AFormat: TGLGeometryFormatClass); override;
  private
    FColorMap, FNormalMap: TglBitmap2D;
  public
    property ColorMap: TglBitmap2D read FColorMap write FColorMap;
    property NormalMap: TglBitmap2D read FNormalMap write FNormalMap;
  public
    procedure BindForRendering(const UseStream: Boolean); override;
    procedure UnbindForRendering; override;
  end;

  TTerrain = class (TObject)
  public
    constructor Create(const AWidth, AHeight: Integer; const ASource: TTerrainSource;
      const AMaterial: TTerrainMaterial);
    destructor Destroy; override;
  private
    FTerrainSections: array of array of TGLGeometryObject;
    FMaterial: TTerrainMaterial;
    FWidth, FHeight: Integer;
    FXSections, FYSections: Integer;
    FSource: TTerrainSource;
    FWaterLine, FSnowLine: Single;
    FHeightfield: PSingle;
    function GetHeightfield(X, Y: Integer): Single;
  public
    procedure Burn;
    procedure Draw(const CamPos, CamPosTransformed: TVector3; const CamFront: TVector3);
    procedure Generate;
  public
    property Height: Integer read FHeight;
    property Heightfield[X, Y: Integer]: Single read GetHeightfield;
    property NormalMap: TGLuint read FNormalMap;
    property SnowLine: Single read FSnowLine write FSnowLine;
    property WaterLine: Single read FWaterLine write FWaterLine;
    property Width: Integer read FWidth;
  end;

implementation


{ TTerrainFormat }

function TTerrainFormat.GetVec3(AIndex: Integer; Index: TVertexIndex
  ): TVector3f;
begin
  // 3x Position, 3x Normal, 3x Tangent [a.k.a. texcoord ;) ]
  Result := SizeOf(Single) * 9;
end;

procedure TTerrainFormat.BindGLPointer;
begin
  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glEnableClientState(GL_NORMAL_ARRAY);
  glVertexPointer(3, GL_FLOAT, FNeededVertexSize, nil);
  glNormalPointer(GL_FLOAT, FNeededVertexSize, Pointer(ptrint(3*SizeOf(Single))));
  glTexCoordPointer(3, GL_FLOAT, FNeededVertexSize, Pointer(ptrint(6*SizeOf(Single))));
end;

procedure TTerrainFormat.UnbindGLPointer;
begin
  glDisableClientState(GL_NORMAL_ARRAY);
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  glDisableClientState(GL_VERTEX_ARRAY);
end;

{ TTerrainMaterial }

constructor TTerrainMaterial.Create(const AGeometryBuffer: TGLGeometryBuffer;
  const AFormat: TGLGeometryFormatClass);
begin
  inherited Create(AGeometryBuffer, AFormat);
  FColorMap := nil;
  FNormalMap := nil;
end;

procedure TTerrainMaterial.BindForRendering(const UseStream: Boolean);
begin
  inherited BindForRendering(UseStream);
  if FColorMap <> nil then
  begin
    glActiveTexture(GL_TEXTURE3);
    FColorMap.Bind();
  end;
  if FNormalMap <> nil then
  begin
    glActiveTexture(GL_TEXTURE4);
    FNormalMap.Bind();
  end;
end;

procedure TTerrainMaterial.UnbindForRendering;
begin
  if FColorMap <> nil then
  begin
    glActiveTexture(GL_TEXTURE3);
    FColorMap.Unbind();
  end;
  if FNormalMap <> nil then
  begin
    glActiveTexture(GL_TEXTURE4);
    FNormalMap.Unbind();
  end;
  glActiveTexture(GL_TEXTURE0);
  inherited UnbindForRendering;
end;

{ TTerrain }

constructor TTerrain.Create(const AWidth, AHeight: Integer;
  const ASource: TTerrainSource; const AMaterial: TTerrainMaterial);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  if (FWidth mod TERRAIN_BLOCK_SIZE <> 0) or (FHeight mod TERRAIN_BLOCK_SIZE <> 0) or (FHeight <> FWidth) then
    raise ETerrainError.CreateFmt('Invalid terrain size: %d×%d, scales are required to be equal and multiples of %d.', [FWidth, FHeight, TERRAIN_BLOCK_SIZE]);

  FMaterial := AMaterial;
  if not (FMaterial.Format is TTerrainFormat) then
    raise ETerrainError.Create('Format must be at least TTerrainFormat.');
  FTerrainSection := nil;
  FHeightfield := nil;
  FSource := ASource;
  FXSections := FWidth div TERRAIN_BLOCK_SIZE;
  FYSections := FHeight div TERRAIN_BLOCK_SIZE;
end;

destructor TTerrain.Destroy;
begin
  Burn;
  inherited Destroy;
end;

function TTerrain.GetHeightfield(X, Y: Integer): Single;
begin
  Result := FHeightfield[X + Y * FWidth];
end;

procedure TTerrain.Burn;
var
  X, Y: Integer;
begin
  if FHeightfield <> nil then
  begin
    for X := 0 to FXSections - 1 do
      for Y := 0 to FYSections - 1 do
      begin
        FTerrainSections[X][Y].Free;
      end;
    FreeMem(FHeightfield);
  end;
end;

procedure TTerrain.Draw(const CamPos, CamPosTransformed: TVector3;
  const CamFront: TVector3);
begin

end;

procedure TTerrain.Generate;
var
  NoiseBuffer: PSingle;
  VertexBuffer, NormalBuffer, TangentBuffer: PVector3f;
  TerrainPoints: Integer;

  X, Y, I, J: Integer;

  FaceNormals: array [0..7] of TVector3;
  FaceEnabled: array [0..3] of Boolean;
  Vertices: array [0..8] of TVector3;

  procedure SetupVertices;
  begin
    if FaceEnabled[0] then
    begin
      J := I - FWidth;
      Vertices[0] := Vector3f(X-1, Y - 1, FHeightfield[J-1]);
      Vertices[1] := Vector3f(X, Y - 1, FHeightfield[J]);
      J += FWidth;
      Vertices[3] := Vector3f(X-1, Y, FHeightfield[J-1]);
      Vertices[4] := Vector3f(X, Y, FHeightfield[J]);
    end;

    if FaceEnabled[1] then
    begin
      J := I - FWidth;
      Vertices[1] := Vector3f(X, Y - 1, FHeightfield[J]);
      Vertices[2] := Vector3f(X+1, Y - 1, FHeightfield[J+1]);
      J += FWidth;
      Vertices[4] := Vector3f(X, Y, FHeightfield[J]);
      Vertices[5] := Vector3f(X+1, Y, FHeightfield[J+1]);
    end;

    if FaceEnabled[2] then
    begin
      J := I;
      Vertices[3] := Vector3f(X-1, Y, FHeightfield[J-1]);
      Vertices[4] := Vector3f(X, Y, FHeightfield[J]);
      J += FWidth;
      Vertices[6] := Vector3f(X-1, Y + 1, FHeightfield[J-1]);
      Vertices[7] := Vector3f(X, Y + 1, FHeightfield[J]);
    end;

    if FaceEnabled[3] then
    begin
      J := I;
      Vertices[4] := Vector3f(X, Y, FHeightfield[J]);
      Vertices[5] := Vector3f(X+1, Y, FHeightfield[J+1]);
      J += FWidth;
      Vertices[7] := Vector3f(X, Y + 1, FHeightfield[J]);
      Vertices[8] := Vector3f(X+1, Y + 1, FHeightfield[J+1]);
    end;
  end;

  procedure SetupIncrementalVertices;
  begin
    if FaceEnabled[0] then
    begin
      Vertices[0] := Vertices[1];
      Vertices[1] := Vertices[2];
      Vertices[3] := Vertices[4];
      Vertices[4] := Vertices[5];
    end;

    if FaceEnabled[2] then
    begin
      Vertices[3] := Vertices[4];
      Vertices[4] := Vertices[5];
      Vertices[6] := Vertices[7];
      Vertices[7] := Vertices[8];
    end;

    if FaceEnabled[1] then
    begin
      J := I - FWidth;
      Vertices[1] := Vector3f(X, Y - 1, FHeightfield[J]);
      Vertices[2] := Vector3f(X+1, Y - 1, FHeightfield[J+1]);
      J += FWidth;
      Vertices[4] := Vector3f(X, Y, FHeightfield[J]);
      Vertices[5] := Vector3f(X+1, Y, FHeightfield[J+1]);
    end;

    if FaceEnabled[3] then
    begin
      J := I;
      Vertices[4] := Vector3f(X, Y, FHeightfield[J]);
      Vertices[5] := Vector3f(X+1, Y, FHeightfield[J+1]);
      J += FWidth;
      Vertices[7] := Vector3f(X, Y + 1, FHeightfield[J]);
      Vertices[8] := Vector3f(X+1, Y + 1, FHeightfield[J+1]);
    end;
  end;

  procedure SetupFaceNormals;
  begin
    if FaceEnabled[0] then
    begin
      FaceNormals[0] := Normalize((Vertices[1] - Vertices[0]) ** (Vertices[3] - Vertices[0]));
      FaceNormals[1] := Normalize((Vertices[3] - Vertices[4]) ** (Vertices[1] - Vertices[4]));
    end;

    if FaceEnabled[1] then
    begin
      FaceNormals[4] := Normalize((Vertices[2] - Vertices[1]) ** (Vertices[4] - Vertices[1]));
      FaceNormals[5] := Normalize((Vertices[4] - Vertices[5]) ** (Vertices[2] - Vertices[5]));
    end;

    if FaceEnabled[2] then
    begin
      FaceNormals[2] := Normalize((Vertices[4] - Vertices[3]) ** (Vertices[6] - Vertices[3]));
      FaceNormals[3] := Normalize((Vertices[6] - Vertices[7]) ** (Vertices[4] - Vertices[7]));
    end;

    if FaceEnabled[3] then
    begin
      FaceNormals[6] := Normalize((Vertices[5] - Vertices[4]) ** (Vertices[7] - Vertices[4]));
      FaceNormals[7] := Normalize((Vertices[7] - Vertices[8]) ** (Vertices[5] - Vertices[8]));
    end;
  end;

var
  Noise: TTerrainSourcePerlinNoise;
  V: TVector3;
  VertexIndex: Integer;
  XBlock, YBlock, XFace, YFace: Integer;
  CurrSection: TGLGeometryTerrainSectionForTris;
begin
  Burn;
  TerrainPoints := FWidth * FHeight;
  {glGenTextures(4, @FHeightMap);
  RaiseLastGLError;

  NoiseBuffer := GetMem(NOISE_TEXTURE_SIZE * NOISE_TEXTURE_SIZE * SizeOf(Single));
  try
    Noise := TTerrainSourcePerlinNoise.Create(NOISE_TEXTURE_SIZE, NOISE_TEXTURE_SIZE, Random(3128) + 1328, Random(3877) + 2187, 0.85, 12, NOISE_TEXTURE_SIZE / 256.0, NOISE_TEXTURE_SIZE / 256.0, 0.5, 0.5);
    try
      Noise.GetData(0, 0, NOISE_TEXTURE_SIZE, NOISE_TEXTURE_SIZE, NoiseBuffer);
    finally
      Noise.Free;
    end;

    glBindTexture(GL_TEXTURE_2D, FNoise);
    RaiseLastGLError;
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    RaiseLastGLError;
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    RaiseLastGLError;
    glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE);
    RaiseLastGLError;
    glTexImage2D(GL_TEXTURE_2D, 0, GL_LUMINANCE8, NOISE_TEXTURE_SIZE, NOISE_TEXTURE_SIZE, 0, GL_LUMINANCE, GL_FLOAT, NoiseBuffer);
    RaiseLastGLError;
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    RaiseLastGLError;
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    RaiseLastGLError;
  finally
    FreeMem(NoiseBuffer);
  end;}

  FHeightfield := GetMem(TerrainPoints * SizeOf(Single));
  FSource.GetData(0, 0, FWidth, FHeight, FHeightfield);

  for XBlock := 0 to FXSections - 1 do
  begin
    for YBlock := 0 to FYSections - 1 do
    begin
      CurrSection := TGLGeometryTerrainSectionForTris.Create(TGLGeometryBuffer.Create(FMaterial.Format.GetNeededVertexSize, GL_DYNAMIC_DRAW), FMaterial.Format, TERRAIN_BLOCK_SIZE, TERRAIN_BLOCK_SIZE));
      FTerrainSections[X][Y] := CurrSection;

      with TTerrainFormat(FMaterial.Format) do
      begin
        UseMap(CurrSection.Map);
        for XQuad := 0 to TERRAIN_BLOCK_SIZE - 1 do
        begin
          for YQuad := 0 to TERRAIN_BLOCK_SIZE - 1 do
          begin
            Position[XQuad + YQuad * TERRAIN_BLOCK_SIZE] :=
              Vector3f(
                TERRAIN_BLOCK_SIZE * XBlock + XQuad,
                TERRAIN_BLOCK_SIZE * YBlock + YQuad,
                0.0);
            Normal[XQuad + YQuad * TERRAIN_BLOCK_SIZE] := Vector3f(0.0, 0.0, 0.0);
            Tangent[XQuad + YQuad * TERRAIN_BLOCK_SIZE] := Vector3f(0.0, 0.0, 0.0);
          end;
        end;
      end;
    end;
  end;


    {I := 0;
    for Y := 0 to FHeight - 1 do
    begin
      for X := 0 to FWidth - 1 do
      begin
        FaceEnabled[0] := (Y > 0) and (X > 0);
        FaceEnabled[1] := (Y > 0) and (X < FWidth-1);
        FaceEnabled[2] := (Y < FHeight - 1) and (X > 0);
        FaceEnabled[3] := (Y < FHeight - 1) and (X < FWidth-1);

        //if FaceEnabled[2] or FaceEnabled[0] then
        //  SetupIncrementalVertices
        //else
          SetupVertices;
        SetupFaceNormals;
        CalcNormalTangent;

        V := Vector3(0., 0., 0.);
        for VertexIndex := 0 to 3 do
          if FaceEnabled[VertexIndex] then
            V += FaceNormals[VertexIndex*2] + FaceNormals[VertexIndex*2+1]
        var
          F: Integer;
          V: TVector3;
        begin
          V := Vector3(0.0, 0.0, 0.0);
          for F := 0 to 3 do
            if FaceEnabled[F] then
              V += FaceNormals[F*2] + FaceNormals[F*2+1];

          //NormalBuffer[I] := Vector4f(V, FHeightfield[I]);
          HeightBuffer[I] := FHeightfield[I];
          NormalBuffer[I] := Vector3ub(Normalize(V));
          if not (FaceEnabled[1] or FaceEnabled[3]) then
            TangentBuffer[I] := Vector3ub(Normalize(Vertices[4] - Vertices[3]))
          else
            TangentBuffer[I] := Vector3ub(Normalize(Vertices[5] - Vertices[4]));
        end;

        Inc(I);
      end;
    end;   }
end;

end.

