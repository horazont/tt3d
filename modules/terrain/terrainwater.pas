unit TerrainWater;
(**********************************************************************
File name: terrainwater.pas
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
  Classes, SysUtils, GLShader, GLShaderMaterial, dglOpenGL, GLGeometry,
  Geometry, TerrainGeometryShaded;

type

  { TTerrainWaterMaterial }

  TTerrainWaterMaterial = class (TGLShaderMaterial)
  private
    FHeightfield: TGLuint;
    FNormalMap: TGLuint;
    FReflectTex: TGLuint;
  public
    property Heightfield: TGLuint read FHeightfield write FHeightfield;
    property NormalMap: TGLuint read FNormalMap write FNormalMap;
    property ReflectTex: TGLuint read FReflectTex write FReflectTex;
  public
    procedure BindForRendering(const UseStream: Boolean); override;
    procedure UnbindForRendering; override;
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
    FWaterLine: Single;
  protected
    procedure Generate;
  public
    procedure Draw(const Matrix: TMatrix4f; const CamPos: TVector3f);
  public
    property WaterLine: Single read FWaterLine write FWaterLine;
  end;

implementation

{ TTerrainWater }

constructor TTerrainWater.Create(const AWidth, AHeight: Integer;
  const AMaterial: TTerrainWaterMaterial);
begin
  FMaterial := AMaterial;
  FWidth := AWidth;
  FHeight := AHeight;
  FGeometry := TGLGeometryTerrainSectionForTris.Create(FMaterial.GeometryBuffer, FMaterial.Format, SHADED_TERRAIN_BLOCK_SIZE+1, SHADED_TERRAIN_BLOCK_SIZE+1, FMaterial.StaticIndexBuffer);
  Generate;
end;

destructor TTerrainWater.Destroy;
begin
  FGeometry.Free;
  inherited Destroy;
end;

procedure TTerrainWater.Generate;
var
  X, Y, I: Integer;
begin
  with FGeometry.Format as TTerrainFormat do
  begin
    UseMap(FGeometry.Map);
    I := 0;
    for Y := 0 to SHADED_TERRAIN_BLOCK_SIZE do
      for X := 0 to SHADED_TERRAIN_BLOCK_SIZE do
      begin
        Position[I]   := Vector2(X,   Y);
        Inc(I, 1);
      end;
  end;
end;

procedure TTerrainWater.Draw(const Matrix: TMatrix4f; const CamPos: TVector3f);
var
  X, Y, W, H: Integer;
  LocOffset, LocWidth: TGLuint;
begin
  W := FWidth div SHADED_TERRAIN_BLOCK_SIZE;
  H := FHeight div SHADED_TERRAIN_BLOCK_SIZE;
  LocOffset := glGetUniformLocation(FMaterial.Shader.ProgramObject, 'offset');

  FMaterial.BindForRendering(False);

  glUniform1f(glGetUniformLocation(FMaterial.Shader.ProgramObject, 'waterLine'), FWaterLine);
  glUniformMatrix4fv(glGetUniformLocation(FMaterial.Shader.ProgramObject, 'texMat'), 1, False, @Matrix);
  glUniform1i(glGetUniformLocation(FMaterial.Shader.ProgramObject, 'reflectTex'), 0);
  glUniform1i(glGetUniformLocation(FMaterial.Shader.ProgramObject, 'normalMap'), 1);
  glUniform1i(glGetUniformLocation(FMaterial.Shader.ProgramObject, 'heightfield'), 2);
  glUniform2f(glGetUniformLocation(FMaterial.Shader.ProgramObject, 'size'), FWidth, FHeight);
  glUniform2f(glGetUniformLocation(FMaterial.Shader.ProgramObject, 'coord'), 0.0, 0.0);
  glUniform3fv(glGetUniformLocation(FMaterial.Shader.ProgramObject, 'camPos'), 1, @CamPos[0]);

  for Y := 0 to H - 1 do
  begin
    for X := 0 to W - 1 do
    begin
      glUniform2f(LocOffset, X * (SHADED_TERRAIN_BLOCK_SIZE), Y * (SHADED_TERRAIN_BLOCK_SIZE));
      FMaterial.Render(GL_TRIANGLES);
    end;

  end;

  FMaterial.Render(GL_TRIANGLES);
  FMaterial.UnbindForRendering;
end;

{ TTerrainWaterMaterial }

procedure TTerrainWaterMaterial.BindForRendering(const UseStream: Boolean);
begin
  inherited BindForRendering(UseStream);
  glActiveTexture(GL_TEXTURE0);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, FReflectTex);
  glActiveTexture(GL_TEXTURE1);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, FNormalMap);
  glActiveTexture(GL_TEXTURE2);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, FHeightfield);
end;

procedure TTerrainWaterMaterial.UnbindForRendering;
begin
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, 0);
  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D, 0);
  glActiveTexture(GL_TEXTURE2);
  glBindTexture(GL_TEXTURE_2D, 0);
  inherited UnbindForRendering;
end;

end.

