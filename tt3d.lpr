program tt3d;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, Classes, TransportRail, Geometry, TransportGeometry, MessageLoop,
  PathfinderRail, PathfinderBase, math;

{$R *.res}

function NewNode: PRailNode;
begin
  New(Result);
  New(Result^.GeometryNode);
end;

procedure ConnectNodes(A, B: PRailNode; ASide, BSide: TRailLinkSide; ASlot, BSlot: Word; Length: Single);
var
  Link: PRailLink;
begin
  New(Link);
  if IsNaN(Length) then
    Link^.Length := VLength(A^.GeometryNode^.Location - B^.GeometryNode^.Location)
  else
    Link^.Length := Length;
  Link^.LinkType := RAIL_LINK_TYPE_NORMAL;
  Link^.Nodes[lsA].Node := A;
  Link^.Nodes[lsA].NodeSide := ASide;
  Link^.Nodes[lsA].NodeSlot := ASlot;
  Link^.Nodes[lsB].Node := B;
  Link^.Nodes[lsB].NodeSide := BSide;
  Link^.Nodes[lsB].NodeSlot := BSlot;
  A^.Sides[ASide][ASlot].Link := Link;
  A^.Sides[ASide][ASlot].LinkSide := lsA;
  B^.Sides[BSide][BSlot].Link := Link;
  B^.Sides[BSide][BSlot].LinkSide := lsB;
end;

var
  TestNodeA, TestNodeB, TestNodeC, TestNodeD, TestNodeE: PRailNode;
  Pathfinder: TPathfinderRail;
  Path: TPath;
begin
  TestNodeA := NewNode;
  TestNodeB := NewNode;
  TestNodeC := NewNode;
  TestNodeD := NewNode;
  TestNodeE := NewNode;
  TestNodeA^.GeometryNode^.Location := Vector3f(0.0, 0.0, 0.0);
  TestNodeB^.GeometryNode^.Location := Vector3f(1.0, 0.0, 0.0);
  TestNodeC^.GeometryNode^.Location := Vector3f(0.5, 0.5, 0.0);
  TestNodeD^.GeometryNode^.Location := Vector3f(0.5, 0.25, 0.0);
  TestNodeE^.GeometryNode^.Location := Vector3f(0.5, -0.4, 0.0);
  ConnectNodes(TestNodeA, TestNodeC, lsA, lsB, 1, 0, NaN);
  ConnectNodes(TestNodeC, TestNodeB, lsA, lsB, 0, 1, NaN);
  ConnectNodes(TestNodeA, TestNodeD, lsA, lsB, 2, 0, NaN);
  ConnectNodes(TestNodeD, TestNodeB, lsA, lsB, 0, 2, NaN);
  ConnectNodes(TestNodeA, TestNodeE, lsB, lsA, 2, 0, NaN);
  ConnectNodes(TestNodeE, TestNodeB, lsB, lsA, 0, 2, NaN);

  Pathfinder := TPathfinderRail.Create;
  try
    WriteLn(Pathfinder.FindPath(TestNodeA, TestNodeB, lsA, Path));
    WriteLn(Format('len: %.2f; steps: %d', [Path.Length, Length(Path.Links)]));
    WriteLn(Pathfinder.FindPath(TestNodeA, TestNodeB, lsB, Path));
    WriteLn(Format('len: %.2f; steps: %d', [Path.Length, Length(Path.Links)]));
  finally
    Pathfinder.Free;
  end;
end.

