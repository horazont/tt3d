unit Pathfinder;
(**********************************************************************
File name: pathfinder.pas
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
  Classes, SysUtils, TransportGraph, contnrs, GTOrderedQueue, Geometry, math,
  fgl;

type
  PPathfinderNode = ^TPathfinderNode;
  TPathfinderNode = record
    F: TVectorFloat;
    Node: TPathNode;
    Parent: PPathfinderNode;
  case First: Boolean of
    True: (FromSide: TPathNodeSide);
    False: (FromLink: TPathNodeLink);
  end;

  TPath = record
    Length: TVectorFloat;
    Links: array of TPathNodeLink;
  end;

  TPathfinderQueue_ = specialize TGTOrderedQueue<PPathfinderNode>;

  { TPathfinderQueue }

  TPathfinderQueue = class (TPathfinderQueue_)
  protected
    function GetValue(const AItem: PPathfinderNode): Double; override;
  end;

  TPathfinderList = specialize TFPGList<PPathfinderNode>;

  TPathfinder = class (TObject)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FClosedTable, FOpenTable: TFPDataHashTable;
    FClosedList, FOpenList: TPathfinderList;
    FOpenQueue: TPathfinderQueue;
  protected
    procedure Clear;
    procedure ExpandNode(ANode: PPathfinderNode);
    function InClosedList(ANode: TPathNode): Boolean;
    function InOpenList(ANode: TPathNode): Boolean;
    procedure AddToOpenList(ANode: TPathNode; F: Double; ALink: TPathNodeLink;
      AParent: PPathfinderNode);
    procedure AddToOpenList(ANode: TPathNode; F: Double; ASide: TPathNodeSide);
  public
    function FindPath(const AOrigin, ATarget: TPathNode; ASide: TPathNodeSide;
      out Solution: TPath): Boolean;
  end;

function HashPathNode(const ANode: TPathNode): String;

implementation

function HashPathNode(const ANode: TPathNode): String;
begin
  SetLength(Result, SizeOf(TVectorFloat) * 3);
  Move(ANode.Location.AsArray[0], Result[1], SizeOf(TVectorFloat) * 3);
end;

{ TPathfinderQueue }

function TPathfinderQueue.GetValue(const AItem: PPathfinderNode): Double;
begin
  Result := AItem^.F;
end;

{ TPathfinder }

constructor TPathfinder.Create;
begin
  FClosedTable := TFPDataHashTable.Create;
  FOpenTable := TFPDataHashTable.Create;
  FClosedList := TPathfinderList.Create;
  FOpenList := TPathfinderList.Create;
  FOpenQueue := TPathfinderQueue.Create;
end;

destructor TPathfinder.Destroy;
begin
  Clear;
  FClosedTable.Free;
  FOpenTable.Free;
  FClosedList.Free;
  FOpenList.Free;
  FOpenQueue.Free;
  inherited Destroy;
end;

procedure TPathfinder.Clear;
var
  Node: PPathfinderNode;
begin
  for Node in FClosedList do
    FreeMem(Node);

  for Node in FOpenList do
    FreeMem(Node);

  FClosedTable.Clear;
  FOpenTable.Clear;
  FOpenQueue.Clear;
end;

procedure TPathfinder.ExpandNode(ANode: PPathfinderNode);
var
  I: Integer;
  F: Double;
  ThroughSide: TPathNodeSide;
  LinkNode: TPathNodeLink;
  TargetNodeLink: TPathNodeLink;
  Target: TPathNode;
  TargetHash: String;
  ExistingNode: PPathfinderNode;
begin
  if ANode^.First then
    ThroughSide := ANode^.FromSide.Owner.Side[SideOpposite(ANode^.FromSide.Direction)]
  else
    ThroughSide := ANode^.FromLink.Owner.Owner.Side[SideOpposite(ANode^.FromLink.Owner.Direction)];
  for I := 0 to ThroughSide.Count - 1 do
  begin
    LinkNode := ThroughSide.Links[I];

    TargetNodeLink := LinkNode.Link.Sides[SideOpposite(LinkNode.Side)].NodeLink;
    Target := TargetNodeLink.Owner.Owner.Owner;

    TargetHash := HashPathNode(Target);

    if FClosedTable[TargetHash] <> nil then
      Continue;

    F := LinkNode.Link.Length + ANode^.F + VLength(ANode^.Node.Location + Target.Location);
    ExistingNode := FOpenTable[TargetHash];
    if ExistingNode <> nil then
    begin
      if ExistingNode^.F <= F then
        Exit;
      ExistingNode^.Parent := ANode;
      ExistingNode^.F := F;
      ExistingNode^.FromLink := TargetNodeLink;
      ExistingNode^.First := False;
      ExistingNode^.Node := Target;
      FOpenQueue.Invalidate(ExistingNode);
    end
    else
    begin
      AddToOpenList(Target, F, TargetNodeLink, ANode);
    end;
  end;
end;

function TPathfinder.InClosedList(ANode: TPathNode): Boolean;
begin
  Result := FClosedTable.Find(HashPathNode(ANode)) <> nil;
end;

function TPathfinder.InOpenList(ANode: TPathNode): Boolean;
begin
  Result := FOpenTable.Find(HashPathNode(ANode)) <> nil;
end;

procedure TPathfinder.AddToOpenList(ANode: TPathNode; F: Double;
  ALink: TPathNodeLink; AParent: PPathfinderNode);
var
  Entry: PPathfinderNode;
begin
  New(Entry);
  Entry^.Node := ANode;
  Entry^.FromLink := ALink;
  Entry^.F := F;
  Entry^.First := False;
  Entry^.Parent := AParent;
  Entry^.First := False;

  FOpenTable.Add(HashPathNode(ANode), Entry);
  FOpenQueue.Add(Entry);
  FOpenList.Add(Entry);
end;

procedure TPathfinder.AddToOpenList(ANode: TPathNode; F: Double;
  ASide: TPathNodeSide);
var
  Entry: PPathfinderNode;
begin
  New(Entry);
  Entry^.Node := ANode;
  Entry^.FromSide := ASide;
  Entry^.F := F;
  Entry^.Parent := nil;
  Entry^.First := True;

  FOpenTable.Add(HashPathNode(ANode), Entry);
  FOpenQueue.Add(Entry);
  FOpenList.Add(Entry);
end;

function TPathfinder.FindPath(const AOrigin, ATarget: TPathNode;
  ASide: TPathNodeSide; out Solution: TPath): Boolean;
var
  FirstNode: PPathfinderNode;
  FirstNodeHash: String;

  TmpList: TPathNodeLinks;
  Node: PPathfinderNode;
  I: Integer;
  LinkPtr: PPathNodeLink;
begin
  Result := False;
  AddToOpenList(AOrigin, 0.0, ASide);
  while not FOpenQueue.Empty do
  begin
    FirstNode := FOpenQueue.PopFirst;
    if FirstNode^.Node = ATarget then
    begin
      Result := True;
      Break;
    end;
    ExpandNode(FirstNode);
    FirstNodeHash := HashPathNode(FirstNode^.Node);
    FClosedTable.Add(FirstNodeHash, FirstNode);
    FClosedList.Add(FirstNode);
    FOpenTable.Delete(FirstNodeHash);
    FOpenList.Remove(FirstNode);
  end;

  if Result then
  begin
    TmpList := TPathNodeLinks.Create;
    try
      TmpList.Capacity := 100;
      Node := FirstNode;
      while not Node^.First do
      begin
        TmpList.Add(Node^.FromLink);
        Node := Node^.Parent;
      end;

      Solution.Length := 0.0;
      SetLength(Solution.Links, TmpList.Count);
      LinkPtr := @TmpList.List[0][High(Solution.Links)];
      for I := 0 to High(Solution.Links) do
      begin
        Solution.Links[I] := LinkPtr^;
        Solution.Length += LinkPtr^.Link.Length;
        Dec(LinkPtr);
      end;
    finally
      TmpList.Free;
    end;
  end;
end;

end.

