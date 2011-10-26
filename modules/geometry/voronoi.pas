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
  Classes, SysUtils, Geometry, fgl;

type
  TPolygon2 = array of TVector2;
  TPolygon3 = array of TVector3;

  TPolygons2 = array of TPolygon2;

  //TDotCloud2 = specialize TFPGList<TVector2>;
  //TDotCloud3 = specialize TFPGList<TVector3>;

  TVoronoiLine2 = record
    Center: TVector2;
    Direction: TVector2;
    T1, T2: TVectorFloat;
  end;

//procedure GrahamScan2(const DotCloud: TDotCloud2; const ConvexHull: TDotCloud2);

//procedure Voronoi2(const DotCloud: TDotCloud2; const Polygons: TPolygons2);

implementation

{function GrahamScanSort2(const A, B: TVector2): Integer;
begin

end;

procedure GrahamScan2(const DotCloud: TDotCloud2; const ConvexHull: TDotCloud2);
begin

end;

procedure Voronoi2(const DotCloud: TDotCloud2; const Polygons: TPolygons2);
begin
  //if Length(DotCloud) < 2 then
  //  Exit;
end;}

end.

