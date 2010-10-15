unit Voronoi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Geometry;

type
  TPolygon2 = array of TVector2;
  TPolygon3 = array of TVector3;

  TPolygons2 = array of TPolygon2;

  TDotCloud2 = array of TVector2;
  TDotCloud3 = array of TVector3;

  TVoronoiLine2 = record
    Center: TVector2;
    Direction: TVector2;
    T1, T2: TVectorFloat;
  end;

procedure Voronoi2(const DotCloud: TDotCloud2; const Polygons: TPolygons2);

implementation

procedure Voronoi2(const DotCloud: TDotCloud2; const Polygons: TPolygons2);
begin
  if Length(DotCloud) < 2 then
    Exit;
end;

end.

