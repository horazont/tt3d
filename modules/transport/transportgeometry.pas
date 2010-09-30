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
    BezierVector: TVector4f;
  end;

  TGeometryNode = object
    Location: TVector3f;
    Yaw: Single;
    Links: array of TGeometryLink;
  end;

implementation

end.

