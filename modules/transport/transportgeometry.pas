unit TransportGeometry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Geometry;

type
  PGeometryNode = ^TGeometryNode;

  TGeometryLink = record
    ToNode: PGeometryNode;
    ToLink: Word;
    BezierVector: TVector3;
    Yaw: TVectorFloat;
  end;

  TGeometryNode = object
    Location: TVector3;
    Yaw: TVectorFloat;
    Links: array of TGeometryLink;
  end;

implementation

end.

