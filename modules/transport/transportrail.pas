unit TransportRail;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Geometry, TransportGeometry, fgl, contnrs;

const
  RAIL_NODE_FLAG_SIGNAL = $0001;
  RAIL_NODE_FLAG_SPLITMERGE = $0002;
  RAIL_NODE_FLAG_DEAD_END = $0004;
  RAIL_NODE_FLAG_DEPOT = $0008;
  RAIL_NODE_FLAG_STATION_START = $0010;
  RAIL_NODE_FLAG_STATION_END = $0020;

  RAIL_LINK_TYPE_NORMAL = $0000;
  RAIL_LINK_TYPE_ELECTRIFIED = $0001;
  RAIL_LINK_TYPE_MONORAIL = $0002;
  RAIL_LINK_TYPE_MAGLEV = $0004;
  RAIL_LINK_TYPE_RESERVED0 = $0008;
  RAIL_LINK_TYPE_RESERVED1 = $0010;
  RAIL_LINK_TYPE_RESERVED2 = $0020;
  RAIL_LINK_TYPE_RESERVED3 = $0040;

type
  TRailLinkType = Word;
  TRailNodeFlags = Word;
  TRailLinkSide = (lsA, lsB);
  TRailSignalKind = (skSemaphore, skLight);
  TRailSignalType = (stPath, stPathOneWay);

  PRailNode = ^TRailNode;
  PRailLink = ^TRailLink;

  TRailNodeSignal = object
    Kind: TRailSignalKind;
    SinalType: TRailSignalType;
  end;
  PRailNodeSignal = ^TRailNodeSignal;

  TRailNodeLink = record
    LinkSide: TRailLinkSide;
    Link: PRailLink;
  end;
  PRailNodeLink = ^TRailNodeLink;

  TRailNodeSide = array [0..2] of TRailNodeLink;

  // Objects without constructors have no overhead, makes 'em cheap.
  TRailNode = object
    GeometryNode: PGeometryNode;
    NodeFlags: TRailNodeFlags;
    Sides: array [TRailLinkSide] of TRailNodeSide;
    Signal: PRailNodeSignal;
    StationDepot: Pointer;
  end;

  TRailLinkNode = record
    NodeSide: TRailLinkSide;
    NodeSlot: Word;
    Node: PRailNode;
  end;
  PRailLinkNode = ^TRailLinkNode;

  TRailLink = object
    Nodes: array [TRailLinkSide] of TRailLinkNode;
    Length: Single;
    AbsoluteHeightChange: Single;
    LinkType: TRailLinkType;
  end;

  TRailLinkDescription = record
    Side: TRailLinkSide;
    Slot: Word;
  end;
  PRailLinkDescription = ^TRailLinkDescription;

  TPath = record
    Length: Single;
    Links: array of TRailLinkDescription;
  end;

function OppositeSide(const ASide: TRailLinkSide): TRailLinkSide;

function HashData(const OfANode: PRailNode): String;

implementation

function OppositeSide(const ASide: TRailLinkSide): TRailLinkSide;
begin
  Result := TRailLinkSide(1- Ord(ASide));
end;

function HashData(const OfANode: PRailNode): String;
begin
  SetLength(Result, SizeOf(TVector3f));
  Move(OfANode^.GeometryNode^.Location, Result[1], SizeOf(TVector3f));
end;

end.

