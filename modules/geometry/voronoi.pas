unit Voronoi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Geometry, fgl;

type
  TPolygon2 = array of TVector2;
  TPolygon3 = array of TVector3;

  TPolygons2 = array of TPolygon2;

  TDotCloud2 = specialize TFPGList<TVector2>;
  TDotCloud3 = specialize TFPGList<TVector3>;

  TVoronoiLine2 = record
    Center: TVector2;
    Direction: TVector2;
    T1, T2: TVectorFloat;
  end;

procedure GrahamScan2(const DotCloud: TDotCloud2; const ConvexHull: TDotCloud2);

procedure Voronoi2(const DotCloud: TDotCloud2; const Polygons: TPolygons2);

implementation

function GrahamScanSort2(const A, B: TVector2): Integer;
begin

end;

procedure GrahamScan2(const DotCloud: TDotCloud2; const ConvexHull: TDotCloud2);
begin

end;

procedure Voronoi2(const DotCloud: TDotCloud2; const Polygons: TPolygons2);
begin
  if Length(DotCloud) < 2 then
    Exit;
end;

end.

