unit TransportRail;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Geometry, TransportGeometry, fgl, contnrs, GTBase;

const
  PAIR_DEFAULT = 0;
  PAIR_CROSSING0 = 1;
  PAIR_CROSSING1 = 2;
  // and so forth.

type
  TPathLinkSide = class;
  TPathLink = class;
  TPathNodeLink = class;
  TPathNodeSide = class;
  TPathNodeSidePair = class;
  TPathNode = class;

  TPathPairNumber = Integer;
  TPathSideSlotNumber = Integer;

  TPathNodeSideDirection = (sdA, sdB);

  TPathSideDefinition = record
    Number: TPathPairNumber;
    Direction: TPathNodeSideDirection;
  end;

  TPathLinkSide = class (TObject)
  private
    FBezierVector: TVector3;
    FNode: TPathNode;
    FNodeLink: TPathNodeLink;
    FOwner: TPathLink;
  public
    procedure DoChange;
  public
    property BezierVector: PVector4 read GetBezierVector;
  end;

  TPathLink = class (TGTBaseObject)
  private
    FBezierPath: TCubicBezier3;
    FBezierYaw: TCubicBezier1;
    FLength: Double;
    FMaxSpeed: Double;
    FMinSpeed: Double;
    FSides: array [TPathNodeSideDirection] of TPathLinkSide;
    FYaw: TVectorFloat;
  public
    procedure DoChange; override;
  published
    property Length: Double read FLength write SetLength;
    property MinSpeed: Double read FMinSpeed write SetMinSpeed;
    property MaxSpeed: Double read FMaxSpeed write SetMaxSpeed;
    property Yaw: Double read FYaw write SetYaw;
  end;

  TPathLinks = specialize TFPGList<TPathLink>;

  TPathNodeLink = class (TGTBaseObject)
  public
    destructor Destroy; override;
  private
    FLink: TPathLink;
    FOwner: TPathNodeSide;
  protected
    procedure LinkLink(const ALink: TPathLink);
    procedure LinkChanged(Sender: TObject);
    procedure LinkDeleting(Sender: TObject);
    procedure SetLink(const ALink: TPathLink);
    procedure SetOwner(const AOwner: TPathNodeSide);
    procedure UnlinkLink(const ALink: TPathLink);
  public
    property Link: TPathLink read FLink;
    property Owner: TPathNodeSide read FOwner;
    property Side: TPathNodeSideDirection read FSide;
  end;

  TPathNodeSide = class (TGTBaseObject)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FDirection: TPathNodeSideDirection;
    FLinks: TPathLinks;
    FOwner: TPathNodeSidePair;
  protected
    procedure LinkNodeLink(const ALink: TPathNodeLink);
    function New(const ALink: TPathLink): TPathNodeLink;
    procedure NodeLinkDeleting(Sender: TObject);
    procedure Remove(const ALink: TPathNodeLink);
    procedure SetOwner(const AOwner: TPathNodeSidePair);
    procedure UnlinkNodeLink(const ALink: TPathNodeLink);
  public
    function ToDefinition: TPathSideDefinition;
  public
    property Count: Integer read GetCount;
    property Links[Index: Integer]: TPathLink read GetLink; default;
  end;

  TPathNodeSidePair = class (TObject)
  private
    FNumber: TPathPairNumber;
    FOwner: TPathNode;
    FSides: array [TPathNodeSideDirection] of TPathNodeSide;
  protected
    procedure SetOwner(const AOwner: TPathNode);
  public
    property Side[ASide: TPathNodeSideDirection]: TPathNodeSide read GetSide; default;
  end;

  TPathNodeSidePairs = specialize TFPGList<TPathNodeSidePair>;

  TPathNode = class (TObject)
  private
    FLocation: TVector3;
    FSidePairs: TPathNodeSidePairs;
  public
    function Connect(const AThroughSide, AToSide: TPathSideDefinition; const AAtNode: TPathNode): TPathLink;
  public
    property Count: Integer read GetCount;
    property SidePairs[Index: Integer]: TPathNodeSidePair read GetSidePair; default;
  end;

function SideDefinition(const APair: TPathPairNumber; const ASide: TPathNodeSideDirection): TPathSideDefinition;

(*const
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
  end;       *)

function OppositeSide(const ASide: TRailLinkSide): TRailLinkSide;

function HashData(const OfANode: PRailNode): String;

implementation

function OppositeSide(const ASide: TRailLinkSide): TRailLinkSide;
begin
  Result := TRailLinkSide(1- Ord(ASide));
end;

function HashData(const OfANode: PRailNode): String;
begin
  SetLength(Result, SizeOf(TVector3));
  Move(OfANode^.GeometryNode^.Location, Result[1], SizeOf(TVector3));
end;

end.

