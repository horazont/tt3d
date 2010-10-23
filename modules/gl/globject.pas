unit GLObject;

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

