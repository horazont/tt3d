program tt3d;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  heaptrc,
  SysUtils, Classes, Geometry, math, dateutils, ioConfig, Main;

{$R *.res}

(*function NewNode: PRailNode;
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
  ConnectNodes(TestNodeD, TestNodeB, lsA, lsB, 0, 2, 0.0);
  ConnectNodes(TestNodeA, TestNodeE, lsB, lsA, 2, 0, NaN);
  ConnectNodes(TestNodeE, TestNodeB, lsB, lsA, 0, 2, NaN);
  ConnectNodes(TestNodeD, TestNodeB, lsB, lsA, 1, 0, 0.0);

  Pathfinder := TPathfinderRail.Create;
  try
    WriteLn(Pathfinder.FindPath(TestNodeA, TestNodeB, lsA, Path));
    WriteLn(Format('len: %.2f; steps: %d', [Path.Length, Length(Path.Links)]));
    WriteLn(Pathfinder.FindPath(TestNodeA, TestNodeB, lsB, Path));
    WriteLn(Format('len: %.2f; steps: %d', [Path.Length, Length(Path.Links)]));
  finally
    Pathfinder.Free;
  end;
end.*)

(*var
  p: array [0..3] of TVector3;

function c(t: TVectorFloat): TVector3;
begin
  Result := (-p[0] + 3*p[1] - 3*p[2] + p[3]) * power(t, 3) +
            (3*p[0] - 6*p[1] + 3*p[2]) * power(t, 2) +
            (-3*p[0] + 3*p[1])*t +
            p[0];
end;

function len(n: Integer): TVectorFloat;
var
  I: Integer;
  Step: TVectorFloat;
  Prev, Curr: TVector3;
  L: TVectorFloat;
  LSum: Extended;
  Min, Max: TVectorFloat;
begin
  Result := 0.0;
  Step := 1/n;
  Prev := c(0);
  Min := Infinity;
  Max := NegInfinity;
  for I := 1 to n do
  begin
    Curr := c(I * Step);
    L := VLength(Curr - Prev);
    if L < Min then
      Min := L
    else if L > Max then
      Max := L;
    Result += L;
    Prev := Curr;
  end;
  WriteLn(Format('avg: %.4f; min: %.4f; max: %.4f', [Result / n, Min, Max]));
end;

function seconds(time: TDateTime): Double;
begin
  Result := HourOf(time) * 3600 + MinuteOf(time) * 60 + SecondOf(time) + MilliSecondOf(time) / 1000.0;
end;

procedure Angletest(abase: TVectorFloat);

  procedure WriteSingle(a: TVectorFloat);
  begin
    WriteLn(Format('%.4f => %.4f', [a*180/Pi, VecToAngle(AngleToVec(a))*180/Pi]));
  end;

begin
  WriteSingle(abase);
  WriteSingle(abase + pi / 6);
  WriteSingle(abase + pi / 4);
  WriteSingle(abase + pi / 3);
end;

var
  TimeStart, TimeEnd: TDateTime;
  Accuracy: TVectorFloat;
  Current, Previous: TVectorFloat;
  Curve: TCubicBezier3;
begin
  Angletest(0);
  Angletest(Pi / 2);
  Angletest(Pi);
  Angletest(Pi + Pi / 2);
  Angletest(2*Pi);
  Curve[0] := Vector3(0.0, 0.0, 0.0);
  Curve[1] := Vector3(1.0, 0.0, 0.0);
  Curve[2] := Vector3(1.0, 1000.0, 0.0);
  Curve[3] := Vector3(2.0, 1000.0, 0.0);
  p[0] := Curve[0];
  p[1] := Curve[1];
  p[2] := Curve[2];
  p[3] := Curve[3];
  WriteLn(FormatVector(Curve ** 0.0));
  WriteLn(FormatVector(Curve ** 1.0));
  Accuracy := 100;
  Previous := 0;
  while Accuracy > 1/10000 do
  begin
    TimeStart := Now;
    Current := BLengthEx(Curve, Accuracy);
    TimeEnd := Now;
    WriteLn(Format('%.7f, difference = %.7f', [Current, Abs(Current - Previous)]));
    WriteLn(Format('needed: %.4fs with planned accuracy = %011.7f', [seconds(TimeEnd -TimeStart), Accuracy]));
    Accuracy /= 10;
    Previous := Current;
  end;
  TimeStart := Now;
  Current := BLengthAuto(Curve);
  TimeEnd := Now;
  WriteLn(Format('auto: %.7f', [Current]));
  WriteLn(Format('needed: %.4fs', [seconds(TimeEnd -TimeStart), Accuracy]));
end.
*)

(*var
  A, B, C: TPathNode;
begin
  A := TPathNode.Create;
  A.Location := Vector3(2.0, 0.0, 0.0);
  A.SidePairs[PAIR_DEFAULT].Tangent := Vector3(-1.0, 0.0, 0.0);
  B := TPathNode.Create;
  B.Location := Vector3(1.0, 0.0, 0.0);
  B.SidePairs[PAIR_DEFAULT].Tangent := Normalize(Vector3(1.0, 0.0, 0.0));

  WriteLn(A.Connect(SideDefinition(PAIR_DEFAULT, sdA), SideDefinition(PAIR_DEFAULT, sdB), B, TPathLinkArc).Length);
end.*)

var
  App: TTT3D;
begin
  App := TTT3D.Create;
  try
    App.RunApp;
  finally
    App.Free;
  end;
end.
