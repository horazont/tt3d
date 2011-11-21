unit Voronoi;
(**********************************************************************
File name: voronoi.pas
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
  Classes, SysUtils, Geometry, math, GTOrderedQueue;

type
  TPolygon2 = array of TVector2;
  TPolygon3 = array of TVector3;

  TVoronoiPolygon2 = record
    Vertices: TPolygon2;
    Origin: TVector2;
  end;

  //TDotCloud2 = specialize TGTSimpleList<TVector2>;
  //TDotCloud3 = specialize TFPGList<TVector3>;
  TDotCloud2 = TList;

  TVoronoiLine2 = record
    Center: TVector2;
    Direction: TVector2;
    T1, T2: TVectorFloat;
  end;

  TVoronoiQueueNode2 = record
    V: TVector2;
    Offset: TVectorFloat;
  end;
  PVoronoiQueueNode2 = ^TVoronoiQueueNode2;

  // We cannot use the generic lists here; They miss overloaded operators :/
  TVoronoiPolygons2 = TList; // specialize TGTSimpleList<TVoronoiPolygon2>;

  TVoronoiQueue2_ = specialize TGTOrderedQueue<PVoronoiQueueNode2>;

  { TVoronoiQueue2 }

  TVoronoiQueue2 = class (TVoronoiQueue2_)
  protected
    function Compare(const ItemA, ItemB: PVoronoiQueueNode2): Integer; override;
    procedure FreeItem(var Item: PVoronoiQueueNode2); override;
  end;

function FortuneSort2(A, B: Pointer): Integer;
procedure Voronoi2(const Input: TDotCloud2; Output: TVoronoiPolygons2);

implementation

function FortuneSort2(A, B: Pointer): Integer;
begin
  if PVector2(A)^.Y < PVector2(B)^.Y then
    Exit(-1);
  if PVector2(A)^.Y > PVector2(B)^.Y then
    Exit(1);
  if PVector2(A)^.X < PVector2(B)^.X then
    Exit(-1);
  if PVector2(A)^.X > PVector2(B)^.X then
    Exit(1);
end;

procedure FortuneMetrics(const Input: TDotCloud2; out Bounds: TBounds2);
var
  I: Integer;
  V: PVector2;
begin
  Bounds.Min := PVector2(Input[0])^;
  Bounds.Max := PVector2(Input[0])^;
  for I := 1 to Input.Count - 1 do
  begin
    V := Input[I];
    if Bounds.Min.X > V^.X then
      Bounds.Min.X := V^.X;
    if Bounds.Min.Y > V^.Y then
      Bounds.Min.Y := V^.Y;
    if Bounds.Max.X < V^.X then
      Bounds.Max.X := V^.X;
    if Bounds.Max.Y > V^.Y then
      Bounds.Max.Y := V^.Y;
  end;
end;

procedure Voronoi2(const Input: TDotCloud2; Output: TVoronoiPolygons2);
var
  SiteIndex: Integer;

  function GetNextSite: PVector2; inline;
  begin
    if SiteIndex = Input.Count then
      Exit(nil);
    Result := Input[SiteIndex];
    Inc(SiteIndex);
  end;

var
  Queue: TVoronoiQueue2;
  QueueNode: PVoronoiQueueNode2;

  NextSite: PVector2;
  BottomSite: TVector2;
  NewPoint: TVector2;
begin
  Input.Sort(@FortuneSort2);
  SiteIndex := 0;

  {Queue := TVoronoiQueue2.Create(True);
  try
    BottomSite := GetNextSite()^;
    NextSite := GetNextSite();

    while True do
    begin
      if not Queue.Empty then
        NewPoint := Queue.Items[0]^.V;

      if (NextSite <> nil) and (Queue.Empty or (NextSite^.Y < NewPoint.Y) or ((NextSite^.Y = NewPoint.Y) and (NextSite^.X < NewPoint.X))) then
      begin
        // SITE EVENT


      end
      else if
      begin

      end
      else
      begin

      end;
    end;
  finally
    Queue.Free;
  end;}
end;

{ TVoronoiQueue2 }

function TVoronoiQueue2.Compare(const ItemA, ItemB: PVoronoiQueueNode2): Integer;
begin
  Result := CompareValue(ItemA^.V.Y + ItemA^.Offset, ItemB^.V.Y + ItemB^.Offset) * 2
    + CompareValue(ItemA^.V.X, ItemB^.V.X);
end;

procedure TVoronoiQueue2.FreeItem(var Item: PVoronoiQueueNode2);
begin
  Dispose(Item);
end;


end.

