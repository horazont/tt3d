unit PathfinderRail;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PathfinderBase, TransportRail, stwOrderedQueue, Geometry,
  math;

type
  PPathfinderNode = ^TPathfinderNode;
  TPathfinderNode = record
    F: Double;
    FromSide: TRailLinkSide;
    FromSlot: Integer;
    Node: PRailNode;
    Parent: PPathfinderNode;
  end;

  TPathfinderRailQueue_ = specialize TstwOrderedQueue<PPathfinderNode>;

  { TPathfinderRailQueue }

  TPathfinderRailQueue = class (TPathfinderRailQueue_)
  protected
    function GetValue(const AItem: PPathfinderNode): Double; override;
  end;


  TPathfinderRail_ = specialize TPathfinderBase<PPathfinderNode, TPathfinderRailQueue>;

  { TPathfinderRail }

  TPathfinderRail = class(TPathfinderRail_)
  protected
    procedure ExpandNode(ANode: PPathfinderNode);
    function HashNode(ANode: PPathfinderNode): String; override;
    function InClosedList(ANode: PRailNode): Boolean;
  public
    procedure AddToOpenList(ANode: PRailNode; F: Double; ASide: TRailLinkSide; AFromSlot: Integer; AParent: PPathfinderNode);
    function FindPath(const AOrigin, ATarget: PRailNode; ASide: TRailLinkSide; out Solution: TPath): Boolean;
  end;

implementation

{ TPathfinderRailQueue }

function TPathfinderRailQueue.GetValue(const AItem: PPathfinderNode): Double;
begin
  Result := AItem^.F;
end;

{ TPathfinderRail }

procedure TPathfinderRail.ExpandNode(ANode: PPathfinderNode);
var
  I: Integer;
  F: Double;
  Link: PRailLink;
  TargetNode: PRailNode;
  LinkNode: PRailLinkNode;
  ExistingNode: PPathfinderNode;
  ThroughSide: TRailLinkSide;
  TargetHashData: String;
  FromLinkNode: PRailLinkNode;
begin
  ThroughSide := ANode^.FromSide;
  WriteLn('expanding: ', FormatVector(ANode^.Node^.GeometryNode^.Location), ' through side ', Ord(ThroughSide));
  for I := 0 to 2 do
  begin
    Link := ANode^.Node^.Sides[ThroughSide][I].Link;
    if Link <> nil then
    begin
      FromLinkNode := @Link^.Nodes[ANode^.Node^.Sides[ThroughSide][I].LinkSide];
      LinkNode := @Link^.Nodes[OppositeSide(ANode^.Node^.Sides[ThroughSide][I].LinkSide)];
      TargetNode := LinkNode^.Node;
      WriteLn('opening: ', FormatVector(TargetNode^.GeometryNode^.Location), ' with length ', Format('%.2f', [Link^.Length]));
      TargetHashData := HashData(TargetNode);

      if FClosedTable[TargetHashData] <> nil then
        Continue;

      F := Link^.Length + ANode^.F + VLength(ANode^.Node^.GeometryNode^.Location + TargetNode^.GeometryNode^.Location);
      ExistingNode := FOpenTable[TargetHashData];
      if ExistingNode <> nil then
      begin
        if ExistingNode^.F <= F then
          Continue;
        ExistingNode^.Parent := ANode;
        ExistingNode^.F := F;
        ExistingNode^.FromSide := FromLinkNode^.NodeSide;
        ExistingNode^.FromSlot := FromLinkNode^.NodeSlot;
        FOpenQueue.Invalidate(ExistingNode);
      end
      else
        AddToOpenList(TargetNode,
          F,
          FromLinkNode^.NodeSide,
          FromLinkNode^.NodeSlot,
          ANode);
    end;
  end;
end;

function TPathfinderRail.HashNode(ANode: PPathfinderNode): String;
begin
  Result := TransportRail.HashData(ANode^.Node);
end;

function TPathfinderRail.InClosedList(ANode: PRailNode): Boolean;
begin
  Result := FClosedTable.Find(HashData(ANode)) <> nil;
end;

procedure TPathfinderRail.AddToOpenList(ANode: PRailNode; F: Double;
  ASide: TRailLinkSide; AFromSlot: Integer; AParent: PPathfinderNode);
var
  NewData: PPathfinderNode;
begin
  New(NewData);
  NewData^.F := F;
  NewData^.Node := ANode;
  NewData^.FromSide := ASide;
  NewData^.Parent := AParent;
  NewData^.FromSlot := AFromSlot;
  FOpenTable.Add(HashData(ANode), NewData);
  FOpenQueue.Add(NewData);
  FOpenList.Add(NewData);
end;

function TPathfinderRail.FindPath(const AOrigin, ATarget: PRailNode;
  ASide: TRailLinkSide; out Solution: TPath): Boolean;
var
  FirstNode, Node: PPathfinderNode;
  RailNode: PRailNode;
  TmpLink: PRailNodeLink;
  FirstNodeHashData: String;
  TempList: TFPList;
  TmpDescription: PRailLinkDescription;
  I: Integer;
begin
  Result := False;
  AddToOpenList(AOrigin, 0.0, ASide, -1, nil);
  while not FOpenQueue.Empty do
  begin
    FirstNode := FOpenQueue.PopFirst;
    if FirstNode^.Node = ATarget then
    begin
      Result := True;
      Break;
    end;
    ExpandNode(FirstNode);
    FirstNodeHashData := HashData(FirstNode^.Node);
    FClosedTable.Add(FirstNodeHashData, FirstNode);
    FClosedList.Add(FirstNode);
    FOpenTable.Delete(FirstNodeHashData);
    FOpenList.Remove(FirstNode);
  end;
  if Result then
  begin
    TempList := TFPList.Create;
    try
      TempList.Capacity := 50;
      Solution.Length := 0.0;
      Node := FirstNode;
      while Node^.Parent <> nil do
      begin
        New(TmpDescription);
        TmpDescription^.Side := Node^.FromSide;
        TmpDescription^.Slot := Node^.FromSlot;
        TempList.Add(TmpDescription);
        Node := Node^.Parent;
      end;
      RailNode := AOrigin;
      SetLength(Solution.Links, TempList.Count);
      for I := 0 to High(Solution.Links) do
      begin
        TmpDescription := PRailLinkDescription(TempList[High(Solution.Links) - I]);
        Solution.Links[I] := TmpDescription^;
        TmpLink := @RailNode^.Sides[TmpDescription^.Side][TmpDescription^.Slot];
        Solution.Length += TmpLink^.Link^.Length;
        RailNode := TmpLink^.Link^.Nodes[OppositeSide(TmpLink^.LinkSide)].Node;
        Dispose(TmpDescription);
      end;
    finally
      TempList.Free;
    end;
  end
  else
    Solution.Length := NaN;
  Clear;
end;

end.

