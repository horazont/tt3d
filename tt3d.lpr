program tt3d;

{$mode objfpc}{$H+}

{$define UseHeapTrc}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  {$ifdef UseHeapTrc}heaptrc,{$endif}
  SysUtils, Classes, Geometry, math, dateutils, ioConfig, iolog,
  coreUIBootstrap, TransportGeometryGL, FileUtil, uiGL, GLMaterials,
  XMLGeometry, coreScene, GLHelpers, uiTT3D, GeometryColors,
  TerrainGeometrySimple, Voronoi, GLCamera, TerrainGeometryDynamic,
  TerrainSourcePerlinNoise, GLOctree, GLFrustum, GLObject, GLShaderMaterial,
  GLShader, TerrainGeometryShaded, TerrainSource, TerrainWater, GLFramebuffer,
  GLBase, DynamicException, Pathfinder, TransportGraph, TransportClasses,
  TransportGeometryClasses, PGBuildingNodes;

var
  App: TTT3D;
{$ifdef UseHeapTrc}
  HeapTrcFile: String;
{$endif}

  {NodeA, NodeB, NodeC: TPathNode;
  APathfinder: TPathfinder;
  Solution: TPath;}

  Curve3: TCubicBezier3;
  Curve1: TCubicBezier1;
begin
  {$ifdef UseHeapTrc}
  HeapTrcFile := ExtractFilePath(ParamStr(0)) + 'heaptrc.txt';
  if FileExistsUTF8(HeapTrcFile) then
    DeleteFileUTF8(HeapTrcFile);
  SetHeapTraceOutput(HeapTrcFile);
  {$endif}

  Randomize;

  {Curve3 := CubicBezier3(
    Vector3(0, 0, 0),
    Vector3(0, 0, 0),
    Vector3(2, 0, 0),
    Vector3(3, 0, 0)
  );
  Curve1 := CubicBezier1(
    0, 10, -10, 0
  );

  WriteLn(Format('%.4f', [BLength(Curve1)]));
  WriteLn(Format('%.4f %.4f', [BLength(Curve3), BLengthAuto(Curve3)]));}

  {NodeA := TPathNode.Create;
  NodeA.Location := Vector3(0.0, 0.0, 0.0);
  NodeA.NewSidePair(Vector3(0.0, 1.0, 0.0));

  NodeB := TPathNode.Create;
  NodeB.Location := Vector3(0.0, 11.0, 0.0);
  NodeB.NewSidePair(Vector3(0.0, 1.0, 0.0));

  NodeC := TPathNode.Create;
  NodeC.Location := Vector3(0.0, -1.0, 1.0);
  NodeC.NewSidePair(Normalize(Vector3(0.0, -1.0, -1.0)));

  NodeA.Connect(SideDefinition(0, sdA), SideDefinition(0, sdB), NodeB, TPathLinkStraight).Invalidate;
  with NodeB.Connect(SideDefinition(0, sdA), SideDefinition(0, sdA), NodeC, TPathLinkBezier) do
  begin
    Invalidate;
    WriteLn(Length);
  end;
  //NodeA.Connect(SideDefinition(0, sdA), SideDefinition(0, sdA), NodeC, TPathLinkBezier).Invalidate;
  NodeB.Connect(SideDefinition(0, sdA), SideDefinition(0, sdB), NodeA, TPathLinkBezier).Invalidate;

  APathfinder := TPathfinder.Create;

  WriteLn(APathfinder.FindPath(NodeA, NodeC, NodeA.SidePairs[0].Side[sdB], Solution));
  WriteLn(Format('%.3f', [Solution.Length]));
  WriteLn(Length(Solution.Links));

  APathfinder.Free;
  NodeC.Free;
  NodeB.Free;
  NodeA.Free;}

  App := TTT3D.Create;
  try
    App.RunApp;
  finally
    App.Free;
  end;
end.
