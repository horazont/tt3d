unit TerrainGeometryShaded;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GLGeometry, Geometry, dglOpenGL, TerrainSource,
  GLShaderMaterial, glBitmap, TerrainSourcePerlinNoise, GLBase;

const
  NOISE_TEXTURE_SIZE = 256;
  SHADED_TERRAIN_BLOCK_SIZE = 32;

type

  { TTerrainFormat }

  TTerrainFormat = class (TGLGeometryFormat)
  protected
    class function GetActualVertexSize: Integer; override;
  public
    procedure BindGLPointer; override;
    procedure UnbindGLPointer; override;
  public
    property Position[Index: TVertexIndex]: TVector2f index 0 read GetVec2 write SetVec2;
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
    FNormalMap, FTangentMap, FNoise: TGLUint;
    FTerrainSection: TGLGeometryObject;
    FMaterial: TTerrainMaterial;
    FWidth, FHeight: Integer;
    FHeightfield: PSingle;
    FSource: TTerrainSource;
    FWaterLine, FSnowLine: Single;
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

class function TTerrainFormat.GetActualVertexSize: Integer;
begin
  // 2x Position
  Result := SizeOf(Single) * 2;
end;

procedure TTerrainFormat.BindGLPointer;
begin
  glEnableClientState(GL_VERTEX_ARRAY);
  {glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glEnableClientState(GL_NORMAL_ARRAY);}
  glVertexPointer(2, GL_FLOAT, FNeededVertexSize, nil);
  {glNormalPointer(GL_FLOAT, FNeededVertexSize, Pointer(ptrint(2*SizeOf(Single))));
  glTexCoordPointer(3, GL_FLOAT, FNeededVertexSize, Pointer(ptrint(5*SizeOf(Single))));}
end;

procedure TTerrainFormat.UnbindGLPointer;
begin
  {glDisableClientState(GL_NORMAL_ARRAY);
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);}
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
  if (FWidth mod SHADED_TERRAIN_BLOCK_SIZE <> 0) or (FHeight mod SHADED_TERRAIN_BLOCK_SIZE <> 0) or (FHeight <> FWidth) then
    raise ETerrainError.CreateFmt('Invalid terrain size: %d×%d, scales are required to be equal and multiples of %d.', [FWidth, FHeight, SHADED_TERRAIN_BLOCK_SIZE]);

  FMaterial := AMaterial;
  if not (FMaterial.Format is TTerrainFormat) then
    raise ETerrainError.Create('Format must be at least TTerrainFormat.');
  FTerrainSection := nil;
  FHeightfield := nil;
  FSource := ASource;
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
begin
  if FTerrainSection <> nil then
  begin
    glDeleteTextures(3, @FNormalMap);
    FTerrainSection.Free;
    Freemem(FHeightfield);
  end;
end;

procedure TTerrain.Draw(const CamPos, CamPosTransformed: TVector3;
  const CamFront: TVector3);
var
  X, Y, W, H: Integer;
  LocOffset, LocWidth: TGLuint;
  FrontDistance, ViewDistance: TVectorFloat;
  Cam3f: TVector3f;
  Loc: TVector2;
begin
  W := FWidth div SHADED_TERRAIN_BLOCK_SIZE;
  H := FHeight div SHADED_TERRAIN_BLOCK_SIZE;
  FrontDistance := 2 * sqrt(2) * SHADED_TERRAIN_BLOCK_SIZE;
  ViewDistance := ln(2) * 200.0 + sqrt(2) * SHADED_TERRAIN_BLOCK_SIZE;
  Cam3f := CamPos;

  FMaterial.BindForRendering(False);
  LocOffset := glGetUniformLocation(FMaterial.Shader.ProgramObject, 'offset');
  LocWidth := glGetUniformLocation(FMaterial.Shader.ProgramObject, 'width');

  glActiveTexture(GL_TEXTURE0);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, FNoise);
  glActiveTexture(GL_TEXTURE1);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, FNormalMap);
  glActiveTexture(GL_TEXTURE2);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, FTangentMap);

  glUniform1i(glGetUniformLocation(FMaterial.Shader.ProgramObject, 'noise'), 0);
  glUniform1i(glGetUniformLocation(FMaterial.Shader.ProgramObject, 'normalMap'), 1);
  glUniform1i(glGetUniformLocation(FMaterial.Shader.ProgramObject, 'tangentMap'), 2);
  glUniform1i(glGetUniformLocation(FMaterial.Shader.ProgramObject, 'colorMap'), 3);
  glUniform1i(glGetUniformLocation(FMaterial.Shader.ProgramObject, 'normalDetailMap'), 4);
  glUniform1f(glGetUniformLocation(FMaterial.Shader.ProgramObject, 'waterLine'), FWaterLine);
  glUniform1f(glGetUniformLocation(FMaterial.Shader.ProgramObject, 'snowLine'), FSnowLine);
  glUniform3fv(glGetUniformLocation(FMaterial.Shader.ProgramObject, 'camPos'), 1, @Cam3f[0]);
  glUniform2f(LocWidth, FWidth, FHeight);
  for Y := 0 to H - 1 do
  begin
    for X := 0 to W - 1 do
    begin
      Loc := Vector2(X * (SHADED_TERRAIN_BLOCK_SIZE), Y * (SHADED_TERRAIN_BLOCK_SIZE));
      if CamFront * Vector3(CamPosTransformed - Vector3(Loc, 0.0)) < -FrontDistance then
        Continue;
      if VLength(CamPos.Vec2 - Loc) > ViewDistance then
        Continue;
      glUniform2f(LocOffset, Loc.X, Loc.Y);
      FMaterial.Render(GL_TRIANGLES);

    end;

  end;
  glActiveTexture(GL_TEXTURE2);
  glBindTexture(GL_TEXTURE_2D, 0);
  glEnable(GL_TEXTURE_2D);
  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D, 0);
  glEnable(GL_TEXTURE_2D);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, 0);
  glEnable(GL_TEXTURE_2D);
  FMaterial.UnbindForRendering;
end;

procedure TTerrain.Generate;

var
  NoiseBuffer: PSingle;
  NormalBuffer, TangentBuffer: PVector4f;
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

  procedure CalcNormalTangent;
  var
    F: Integer;
    V: TVector3;
  begin
    V := Vector3(0.0, 0.0, 0.0);
    for F := 0 to 3 do
      if FaceEnabled[F] then
        V += FaceNormals[F*2] + FaceNormals[F*2+1];

    NormalBuffer[I] := Vector4f(V, FHeightfield[I]);
    if not (FaceEnabled[1] or FaceEnabled[3]) then
      TangentBuffer[I] := Vector4f(Normalize(Vertices[4] - Vertices[3]), 0.0)
    else
      TangentBuffer[I] := Vector4f(Normalize(Vertices[5] - Vertices[4]), 0.0);
  end;

var
  Noise: TTerrainSourcePerlinNoise;
begin
  Burn;
  TerrainPoints := FWidth * FHeight;
  glGenTextures(3, @FNormalMap);
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
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_LUMINANCE8, NOISE_TEXTURE_SIZE, NOISE_TEXTURE_SIZE, 0, GL_LUMINANCE, GL_FLOAT, NoiseBuffer);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  finally
    FreeMem(NoiseBuffer);
  end;

  FHeightfield := GetMem(TerrainPoints * SizeOf(Single));
  FSource.GetData(0, 0, FWidth, FHeight, FHeightfield);

  NormalBuffer := GetMem(TerrainPoints * SizeOf(Single) * 4);
  TangentBuffer := GetMem(TerrainPoints * SizeOf(Single) * 4);
  try

    I := 0;
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

        Inc(I);
      end;
    end;

    glBindTexture(GL_TEXTURE_2D, FNormalMap);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA_FLOAT32_ATI, FWidth, FHeight, 0, GL_RGBA, GL_FLOAT, NormalBuffer);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    glBindTexture(GL_TEXTURE_2D, FTangentMap);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA_FLOAT32_ATI, FWidth, FHeight, 0, GL_RGBA, GL_FLOAT, TangentBuffer);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  finally
    FreeMem(NormalBuffer);
    FreeMem(TangentBuffer);
  end;

  FTerrainSection := TGLGeometryTerrainSectionForTris.Create(FMaterial.GeometryBuffer, FMaterial.Format, SHADED_TERRAIN_BLOCK_SIZE+1, SHADED_TERRAIN_BLOCK_SIZE+1, FMaterial.StaticIndexBuffer);
  with FTerrainSection.Format as TTerrainFormat do
  begin
    UseMap(FTerrainSection.Map);
    I := 0;
    for Y := 0 to SHADED_TERRAIN_BLOCK_SIZE do
      for X := 0 to SHADED_TERRAIN_BLOCK_SIZE do
      begin
        Position[I]   := Vector2(X,   Y);
        {Position[I+1] := Vector2(X+1, Y);
        Position[I+2] := Vector2(X+1, Y+1);
        Position[I+3] := Vector2(X,   Y+1);}
        Inc(I, 1);
      end;
  end;
end;

end.

