unit TerrainWater;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GLShader, GLShaderMaterial, dglOpenGL, GLGeometry,
  Geometry, TerrainGeometryShaded;

type
  TTerrainWaterMaterial = class (TGLShaderMaterial)

  end;

  { TTerrainWater }

  TTerrainWater = class (TObject)
  public
    constructor Create(const AWidth, AHeight: Integer;
      const AMaterial: TTerrainWaterMaterial);
    destructor Destroy; override;
  private
    FGeometry: TGLGeometryObject;
    FMaterial: TTerrainWaterMaterial;
    FWidth, FHeight: Integer;
  protected
    procedure Generate;
  public
    procedure Draw;
  end;

implementation

{ TTerrainWater }

constructor TTerrainWater.Create(const AWidth, AHeight: Integer;
  const AMaterial: TTerrainWaterMaterial);
begin
  FMaterial := AMaterial;
  FWidth := AWidth;
  FHeight := AHeight;
  FGeometry := TGLGeometryTerrainSectionForTris.Create(FMaterial.GeometryBuffer, FMaterial.Format, 2,  2, FMaterial.StaticIndexBuffer);
  Generate;
end;

destructor TTerrainWater.Destroy;
begin
  FGeometry.Free;
  inherited Destroy;
end;

procedure TTerrainWater.Generate;
begin
  with FGeometry.Format as TTerrainFormat do
  begin
    UseMap(FGeometry.Map);
    Position[0] := Vector2(0, 0);
    Position[1] := Vector2(FWidth, 0);
    Position[2] := Vector2(0, FHeight);
    Position[3] := Vector2(FWidth, FHeight);
  end;
end;

procedure TTerrainWater.Draw;
begin
  FMaterial.BindForRendering(False);

  glUniform1f(glGetUniformLocation(FMaterial.Shader.ProgramObject, 'waterLine'), -4.8);
  glUniform1f(glGetUniformLocation(FMaterial.Shader.ProgramObject, 'snowLine'), 3.0);

  FMaterial.Render(GL_TRIANGLES);
  FMaterial.UnbindForRendering;
end;

end.

