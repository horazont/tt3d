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

procedure Voronoi2(const DotCloud: TDotCloud2; const Polygons: TPolygons2);

implementation

procedure Voronoi2_Helper(const DotA, DotB: TVector2)

procedure Voronoi2(const DotCloud: TDotCloud2; const Polygons: TPolygons2);
begin
  if Length(DotCloud) < 2 then
    Exit;
end;

end.

