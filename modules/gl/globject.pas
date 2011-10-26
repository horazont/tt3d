unit GLObject;
(**********************************************************************
File name: globject.pas
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
  Classes, SysUtils, GLGeometry, fgl, Geometry, math;

type
  TGLGeometryObjects = specialize TFPGList<TGLGeometryObject>;

  { TGLObject }

  TGLObject = class (TObject)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FBoundingBox: TGLBoundingBox;
    FObjects: TGLGeometryObjects;
    function GetCount: Integer;
    function GetObject(Index: Integer): TGLGeometryObject;
  public
    property BoundingBox: TGLBoundingBox read FBoundingBox;
    property Count: Integer read GetCount;
    property GLObject[Index: Integer]: TGLGeometryObject read GetObject;
  public
    procedure Clear;
    procedure RecalculateBoundingBox;
  end;

implementation

{ TGLObject }

constructor TGLObject.Create;
begin
  FObjects := TGLGeometryObjects.Create;
  FBoundingBox.Origin := Vector3(0.0, 0.0, 0.0);
  FBoundingBox.Dimension := Vector3(0.0, 0.0, 0.0);
end;

destructor TGLObject.Destroy;
begin
  Clear;
  FObjects.Free;
  inherited Destroy;
end;

function TGLObject.GetCount: Integer;
begin
  Result := FObjects.Count;
end;

function TGLObject.GetObject(Index: Integer): TGLGeometryObject;
begin
  Result := FObjects[Index];
end;

procedure TGLObject.Clear;
begin
  FObjects.Clear;
end;

procedure TGLObject.RecalculateBoundingBox;
var
  Min, Max, V: TVector3;
  I, J: Integer;
  Obj: TGLGeometryObject;

begin
  for I := 0 to 2 do
    Min.AsArray[I] := Infinity;
  for I := 0 to 2 do
    Max.AsArray[I] := NegInfinity;
  for I := 0 to FObjects.Count - 1 do
  begin
    Obj := FObjects[I];
    with Obj.Format as TGLGeometryFormatP4 do
    begin
      UseMap(Obj.Map);
      for J := 0 to Obj.Count - 1 do
      begin
        V := Vector4(Position[J]).Vec3;
        if V.X > Max.X then
          Max.X := V.X;
        if V.X < Min.X then
          Min.X := V.X;
        if V.Y > Max.Y then
          Max.Y := V.Y;
        if V.Y < Min.Y then
          Min.Y := V.Y;
        if V.Z > Max.Z then
          Max.Z := V.Z;
        if V.Z < Min.Z then
          Min.Z := V.Z;
      end;
    end;
  end;
  FBoundingBox.Origin := Min;
  FBoundingBox.Dimension := Max - Min;
end;

end.

