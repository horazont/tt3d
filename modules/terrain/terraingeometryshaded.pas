unit TerrainGeometryShaded;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GLGeometry, Geometry, dglOpenGL, TerrainSource,
  GLShaderMaterial;

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

  TTerrain = class (TObject)
  public
    constructor Create(const AWidth, AHeight: Integer; const ASource: TTerrainSource;
      const AMaterial: TGLShaderMaterial);
    destructor Destroy; override;
  private
    FHeightfield, FNormalMap, FTangentMap: TGLUint;
    FTerrainSection: TGLGeometryObject;
    FMaterial: TGLShaderMaterial;
    FWidth, FHeight: Integer;
    FSource: TTerrainSource;
  public
    procedure Burn;
    procedure Draw;
    procedure Generate;
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

{ TTerrain }

constructor TTerrain.Create(const AWidth, AHeight: Integer;
  const ASource: TTerrainSource; const AMaterial: TGLShaderMaterial);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  if (FWidth mod 64 <> 0) or (FHeight mod 64 <> 0) or (FHeight <> FWidth) then
    raise ETerrainError.CreateFmt('Invalid terrain size: %d×%d, scales are required to be equal and multiples of %d.', [FWidth, FHeight, TERRAIN_GEOMETRY_BLOCK_SIZE]);

  FMaterial := AMaterial;
  if not (FMaterial.Format is TTerrainFormat) then
    raise ETerrainError.Create('Format must be at least TTerrainFormat.');
  FTerrainSection := nil;
  FSource := ASource;
end;

destructor TTerrain.Destroy;
begin
  Burn;
  inherited Destroy;
end;

procedure TTerrain.Burn;
begin
  if FTerrainSection <> nil then
  begin
    glDeleteTextures(3, @FHeightfield);
    FTerrainSection.Free;
  end;
end;

procedure TTerrain.Draw;
var
  X, Y, W, H: Integer;
  LocOffset, LocWidth: TGLuint;
begin
  W := FWidth div TERRAIN_GEOMETRY_BLOCK_SIZE;
  H := FHeight div TERRAIN_GEOMETRY_BLOCK_SIZE;
  FMaterial.BindForRendering(False);
  LocOffset := glGetUniformLocation(FMaterial.Shader.ProgramObject, 'offset');
  LocWidth := glGetUniformLocation(FMaterial.Shader.ProgramObject, 'width');

  glActiveTexture(GL_TEXTURE0);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, FHeightfield);
  glActiveTexture(GL_TEXTURE1);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, FNormalMap);
  glActiveTexture(GL_TEXTURE2);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, FTangentMap);

  glUniform1i(glGetUniformLocation(FMaterial.Shader.ProgramObject, 'heightfield'), 0);
  glUniform1i(glGetUniformLocation(FMaterial.Shader.ProgramObject, 'normalMap'), 1);
  glUniform1i(glGetUniformLocation(FMaterial.Shader.ProgramObject, 'tangentMap'), 2);
  glUniform2f(LocWidth, FWidth, FHeight);
  for Y := 0 to H - 1 do
  begin
    for X := 0 to W - 1 do
    begin
      glUniform2f(LocOffset, X * TERRAIN_GEOMETRY_BLOCK_SIZE, Y * TERRAIN_GEOMETRY_BLOCK_SIZE);
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

{  ->
  0  1  2
 ∨ 01 45
  3<-4  5
   23 67
  6  7  8
}
var
  TerrainBuffer: PSingle;
  NormalBuffer, TangentBuffer: PVector3f;
  TerrainPoints: Integer;

  X, Y, I, J: Integer;

  FaceNormals: array [0..7] of TVector3;
  Vertices: array [0..8] of TVector3;
begin
  Burn;
  TerrainPoints := FWidth * FHeight;
  glGenTextures(3, @FHeightfield);
  RaiseLastGLError;
  TerrainBuffer := GetMem(TerrainPoints * SizeOf(Single));
  try
    FSource.GetData(0, 0, FWidth, FHeight, TerrainBuffer);
    glBindTexture(GL_TEXTURE_2D, FHeightfield);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_LUMINANCE_FLOAT32_ATI, FWidth, FHeight, 0, GL_LUMINANCE, GL_FLOAT, TerrainBuffer);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);


    NormalBuffer := GetMem(TerrainPoints * SizeOf(Single) * 3);
    TangentBuffer := GetMem(TerrainPoints * SizeOf(Single) * 3);
    try
      X := 0;
      Y := 0;
      I := 0;
      J := I;
      Vertices[4] := Vector3f(X, Y, TerrainBuffer[J]);
      Vertices[5] := Vector3f(X+1, Y, TerrainBuffer[J+1]);
      J += FWidth;
      Vertices[7] := Vector3f(X, Y+1, TerrainBuffer[J]);
      Vertices[8] := Vector3f(X+1, Y+1, TerrainBuffer[J+1]);

      FaceNormals[6] := Normalize((Vertices[5] - Vertices[4]) ** (Vertices[7] - Vertices[4]));
      FaceNormals[7] := Normalize((Vertices[7] - Vertices[8]) ** (Vertices[5] - Vertices[8]));

      NormalBuffer[0] := (FaceNormals[6] + FaceNormals[7]) * 0.5;
      TangentBuffer[0] := Normalize(Vertices[5] - Vertices[4]);

      I := FWidth;
      for Y := 1 to FHeight - 2 do
      begin

        X := 0;

        J := I - FWidth;
        Vertices[1] := Vector3f(X, Y - 1, TerrainBuffer[J]);
        Vertices[2] := Vector3f(X+1, Y - 1, TerrainBuffer[J+1]);
        J += FWidth;
        Vertices[4] := Vector3f(X, Y, TerrainBuffer[J]);
        Vertices[5] := Vector3f(X+1, Y, TerrainBuffer[J+1]);
        J += FWidth;
        Vertices[7] := Vector3f(X, Y + 1, TerrainBuffer[J]);
        Vertices[8] := Vector3f(X+1, Y + 1, TerrainBuffer[J+1]);

        FaceNormals[4] := Normalize((Vertices[2] - Vertices[1]) ** (Vertices[4] - Vertices[1]));
        FaceNormals[5] := Normalize((Vertices[4] - Vertices[5]) ** (Vertices[2] - Vertices[5]));

        FaceNormals[6] := Normalize((Vertices[5] - Vertices[4]) ** (Vertices[7] - Vertices[4]));
        FaceNormals[7] := Normalize((Vertices[7] - Vertices[8]) ** (Vertices[5] - Vertices[8]));

        NormalBuffer[I] := (FaceNormals[4] + FaceNormals[5] + FaceNormals[6] + FaceNormals[7]) * 0.25;
        TangentBuffer[I] := Normalize(Vertices[5] - Vertices[4]);

        for X := 1 to FWidth - 2 do
        begin
          J := I - FWidth;
          Vertices[0] := Vector3f(X-1, Y - 1, TerrainBuffer[J-1]);
          Vertices[1] := Vector3f(X, Y - 1, TerrainBuffer[J]);
          Vertices[2] := Vector3f(X+1, Y - 1, TerrainBuffer[J+1]);
          J += FWidth;
          Vertices[3] := Vector3f(X-1, Y, TerrainBuffer[J-1]);
          Vertices[4] := Vector3f(X, Y, TerrainBuffer[J]);
          Vertices[5] := Vector3f(X+1, Y, TerrainBuffer[J+1]);
          J += FWidth;
          Vertices[6] := Vector3f(X-1, Y + 1, TerrainBuffer[J-1]);
          Vertices[7] := Vector3f(X, Y + 1, TerrainBuffer[J]);
          Vertices[8] := Vector3f(X+1, Y + 1, TerrainBuffer[J+1]);

          FaceNormals[0] := Normalize((Vertices[1] - Vertices[0]) ** (Vertices[3] - Vertices[0]));
          FaceNormals[1] := Normalize((Vertices[3] - Vertices[4]) ** (Vertices[1] - Vertices[4]));

          FaceNormals[2] := Normalize((Vertices[4] - Vertices[3]) ** (Vertices[6] - Vertices[3]));
          FaceNormals[3] := Normalize((Vertices[6] - Vertices[7]) ** (Vertices[4] - Vertices[7]));

          FaceNormals[4] := Normalize((Vertices[2] - Vertices[1]) ** (Vertices[4] - Vertices[1]));
          FaceNormals[5] := Normalize((Vertices[4] - Vertices[5]) ** (Vertices[2] - Vertices[5]));

          FaceNormals[6] := Normalize((Vertices[5] - Vertices[4]) ** (Vertices[7] - Vertices[4]));
          FaceNormals[7] := Normalize((Vertices[7] - Vertices[8]) ** (Vertices[5] - Vertices[8]));

          NormalBuffer[I] := (FaceNormals[0] + FaceNormals[1] + FaceNormals[2] + FaceNormals[3] + FaceNormals[4] + FaceNormals[5] + FaceNormals[6] + FaceNormals[7]) * 0.125;
          TangentBuffer[I] := Normalize(Vertices[5] - Vertices[4]);
          Inc(I);
        end;

        X := FWidth - 1;

        J := I - FWidth;
        Vertices[1] := Vector3f(X-1, Y - 1, TerrainBuffer[J-1]);
        Vertices[2] := Vector3f(X, Y - 1, TerrainBuffer[J]);
        J += FWidth;
        Vertices[4] := Vector3f(X-1, Y, TerrainBuffer[J-1]);
        Vertices[5] := Vector3f(X, Y, TerrainBuffer[J]);
        J += FWidth;
        Vertices[7] := Vector3f(X-1, Y + 1, TerrainBuffer[J-1]);
        Vertices[8] := Vector3f(X, Y + 1, TerrainBuffer[J]);

        FaceNormals[4] := Normalize((Vertices[4] - Vertices[1]) ** (Vertices[2] - Vertices[1]));
        FaceNormals[5] := Normalize((Vertices[2] - Vertices[5]) ** (Vertices[4] - Vertices[5]));

        FaceNormals[6] := Normalize((Vertices[7] - Vertices[4]) ** (Vertices[5] - Vertices[4]));
        FaceNormals[7] := Normalize((Vertices[5] - Vertices[8]) ** (Vertices[7] - Vertices[8]));

        NormalBuffer[I] := (FaceNormals[4] + FaceNormals[5] + FaceNormals[6] + FaceNormals[7]) * 0.25;
        TangentBuffer[I] := Normalize(Vertices[5] - Vertices[4]);

      end;

      glBindTexture(GL_TEXTURE_2D, FNormalMap);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA_FLOAT32_ATI, FWidth, FHeight, 0, GL_RGB, GL_FLOAT, NormalBuffer);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

      glBindTexture(GL_TEXTURE_2D, FTangentMap);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA_FLOAT32_ATI, FWidth, FHeight, 0, GL_RGB, GL_FLOAT, TangentBuffer);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    finally
      FreeMem(NormalBuffer);
      FreeMem(TangentBuffer);
    end;
  finally
    FreeMem(TerrainBuffer);
  end;

  FTerrainSection := TGLGeometryQuadsForTris.Create(FMaterial.GeometryBuffer, FMaterial.Format, 64*64, FMaterial.StaticIndexBuffer);
  with FTerrainSection.Format as TTerrainFormat do
  begin
    UseMap(FTerrainSection.Map);
    I := 0;
    for Y := 0 to 63 do
      for X := 0 to 63 do
      begin
        Position[I]   := Vector2(X,   Y);
        Position[I+1] := Vector2(X+1, Y);
        Position[I+2] := Vector2(X+1, Y+1);
        Position[I+3] := Vector2(X,   Y+1);
        Inc(I, 4);
      end;
  end;
end;

end.

