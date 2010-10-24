program tt3d;

{$mode objfpc}{$H+}

// {$define UseHeapTrc}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  {$ifdef UseHeapTrc}heaptrc,{$endif}
  SysUtils, Classes, Geometry, math, dateutils, ioConfig, coreUIBootstrap,
  TransportGeometryGL, FileUtil, uiGL, GLMaterials, XMLGeometry, coreScene,
  GLHelpers, uiTT3D, GeometryColors, TerrainGeometrySimple, Voronoi, GLCamera,
  TerrainGeometryDynamic, TerrainSourcePerlinNoise, GLOctree, GLFrustum,
GLObject, GLShaderMaterial, GLShader;

{$R *.res}

var
  App: TTT3D;
{$ifdef UseHeapTrc}
  HeapTrcFile: String;
{$endif}
begin
  {$ifdef UseHeapTrc}
  HeapTrcFile := ExtractFilePath(ParamStr(0)) + 'heaptrc.txt';
  if FileExistsUTF8(HeapTrcFile) then
    DeleteFileUTF8(HeapTrcFile);
  SetHeapTraceOutput(HeapTrcFile);
  {$endif}

  App := TTT3D.Create;
  try
    App.RunApp;
  finally
    App.Free;
  end;
end.
