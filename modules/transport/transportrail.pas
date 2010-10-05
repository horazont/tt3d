unit TransportRail;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Geometry, TransportGeometry, fgl, contnrs, GTBase, math;

const
  GRAVITY = 9.81;
  YAW_TOLERANCE_UP = 0.15;
  YAW_TOLERANCE_DOWN = 0.05;

  PAIR_DEFAULT = 0;
  PAIR_CROSSING0 = 1;
  PAIR_CROSSING1 = 2;
  // and so forth.

  ARC_PATH_STEP_LENGTH = 0.5;
  BEZIER_PATH_STEP_LENGTH = 0.5;
  BEZIER_PATH_STEP_EPSILON = 0.01;

  Pi2 = Pi * 2;

  MAX_YAW = pi / 2.0;

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

  TPathGeometryCallback = procedure (Sender: TPathLink; const t, s: TVectorFloat; const p: TVector4; Userdata: Pointer) of object;

  TPathSideDefinition = record
    Number: TPathPairNumber;
    Direction: TPathNodeSideDirection;
  end;

  { TPathLinkSide }

  TPathLinkSide = class (TObject)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FNode: TPathNode;
    FNodeLink: TPathNodeLink;
    FOnChange: TGTEventList;
    FOwner: TPathLink;
    FPathTangent: TVector3;
    FUpdatedOnce: Boolean;
    function GetPathTangent: PVector3;
  public
    procedure DoChange;
  public
    property PathTangent: PVector3 read GetPathTangent;
    property OnChange: TGTEventList read FOnChange;
  end;

  { TPathLinkLinearSegment }

  TPathLinkLinearSegment = class (TObject)
  public
    constructor Create(const ALength: TVectorFloat; AFrom, ATo: TVector4);
  private
    FFrom: TVector4;
    FLength: TVectorFloat;
    FTo: TVector4;
  public
    function Interpolate(var x, Movement: TVectorFloat; out p: TVector4): Boolean;
  end;
  TPathLinkLinearSegments = specialize TFPGList<TPathLinkLinearSegment>;

  TPathLink = class (TGTBaseObject)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FBezierYaw: TCubicBezier1;
    FInvalidated: Boolean;
    FLength: TVectorFloat;
    FMaxSpeed: TVectorFloat;
    FMinSpeed: TVectorFloat;
    FPathSegments: TPathLinkLinearSegments;
    FSides: array [TPathNodeSideDirection] of TPathLinkSide;
    FYaw: TVectorFloat;
    function GetLength: TVectorFloat;
    function GetMaxSpeed: TVectorFloat;
    function GetMinSpeed: TVectorFloat;
    function GetPathSegmets: TPathLinkLinearSegments;
    function GetYaw: TVectorFloat;
  protected
    procedure AddPathSegment(const AFrom, ATo: TVector4; const ALength: Double);
    procedure BuildPath; virtual; abstract;
    function ClampYaw(const AValue: TVectorFloat): TVectorFloat;
    procedure ClearPath;
    procedure Deinvalidate;
    procedure HandleLinkSideChanged(Sender: TObject); virtual;
    procedure SetLength(const AValue: TVectorFloat);
    procedure SetMaxSpeed(const AValue: TVectorFloat);
    procedure SetMinSpeed(const AValue: TVectorFloat);
    procedure SetYaw(const AValue: TVectorFloat); virtual;
    procedure UpdateSpeedLimits(const ARadius: TVectorFloat);
  public
    procedure DoChange; override;
    procedure Invalidate;
  published
    property Length: TVectorFloat read GetLength;
    property MinSpeed: TVectorFloat read GetMinSpeed;
    property MaxSpeed: TVectorFloat read GetMaxSpeed;
    property PathSegmets: TPathLinkLinearSegments read GetPathSegmets;
    property Yaw: TVectorFloat read GetYaw;
  end;
  TPathLinkClass = class of TPathLink;

  { TPathLinkBezier }

  TPathLinkBezier = class (TPathLink)
  private
    FBezierPath: TCubicBezier3;
    FMaxRadius: TVectorFloat;
    FPathPoints: array of TVector4;
  protected
    procedure BuildPath; override;
    procedure SetMaxRadius(const AValue: TVectorFloat);
    procedure SetYaw(const AValue: TVectorFloat); override;
    procedure UpdateCurve; virtual;
  end;

  { TPathLinkStraight }

  TPathLinkStraight = class (TPathLinkBezier)
  protected
    procedure BuildPathSimple(const vFrom, vTo: TVector4);
    procedure BuildPath; override;
    procedure SetYaw(const AValue: TVectorFloat); override;
    procedure UpdateCurve; override;
  end;

  { TPathLinkArc }

  TPathLinkArc = class (TPathLinkStraight)
  private
    FAngleStart: TVectorFloat;
    FAngleEnd: TVectorFloat;
    FCenter: TVector2;
    FRadius: TVectorFloat;
    FCosSinStart, FCosSinEnd: TVector2;
  protected
    procedure BuildPath; override;
  end;

  { TPathNodeLink }

  TPathNodeLink = class (TGTBaseObject)
  public
    destructor Destroy; override;
  private
    FLink: TPathLink;
    FOwner: TPathNodeSide;
    FSide: TPathNodeSideDirection;
  protected
    procedure LinkLink(const ALink: TPathLink);
    procedure LinkChanged(Sender: TObject);
    procedure LinkDeleting(Sender: TObject);
    procedure SetLink(const ALink: TPathLink);
    procedure SetOwner(const AOwner: TPathNodeSide);
    procedure UnlinkLink(const ALink: TPathLink);
  public
    procedure UpdateLink;
  public
    property Link: TPathLink read FLink;
    property Owner: TPathNodeSide read FOwner;
    property Side: TPathNodeSideDirection read FSide;
  end;

  TPathNodeLinks = specialize TFPGList<TPathNodeLink>;

  { TPathNodeSide }

  TPathNodeSide = class (TGTBaseObject)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FDirection: TPathNodeSideDirection;
    FLinks: TPathNodeLinks;
    FOwner: TPathNodeSidePair;
    FTangent: TVector3;
    function GetCount: Integer;
    function GetLink(Index: Integer): TPathNodeLink;
  protected
    procedure Clear;
    procedure LinkNodeLink(const ALink: TPathNodeLink);
    function New(const ALink: TPathLink; const AsSide: TPathNodeSideDirection): TPathNodeLink;
    procedure NodeLinkDeleting(Sender: TObject);
    procedure Remove(const ALink: TPathNodeLink);
    procedure SetOwner(const AOwner: TPathNodeSidePair);
    procedure SetTangent(const ATangent: TVector3);
    procedure UnlinkNodeLink(const ALink: TPathNodeLink);
    procedure UpdateLinks;
  public
    function ToDefinition: TPathSideDefinition;
  public
    property Count: Integer read GetCount;
    property Links[Index: Integer]: TPathNodeLink read GetLink; default;
  end;

  { TPathNodeSidePair }

  TPathNodeSidePair = class (TGTBaseObject)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FNumber: TPathPairNumber;
    FOwner: TPathNode;
    FSides: array [TPathNodeSideDirection] of TPathNodeSide;
    FTangent: TVector3;
    function GetSide(ASide: TPathNodeSideDirection): TPathNodeSide;
    procedure SetTangent(const AValue: TVector3);
  protected
    procedure SetNumber(const ANumber: TPathPairNumber);
    procedure SetOwner(const AOwner: TPathNode);
    procedure UpdateSides;
  public
    property Side[ASide: TPathNodeSideDirection]: TPathNodeSide read GetSide; default;
    property Tangent: TVector3 read FTangent write SetTangent;
  end;

  TPathNodeSidePairs = specialize TFPGList<TPathNodeSidePair>;

  TPathNode = class (TGTBaseObject)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FLocation: TVector3;
    FSidePairs: TPathNodeSidePairs;
    function GetCount: Integer;
    function GetSidePair(Index: Integer): TPathNodeSidePair;
    procedure SetLocation(const AValue: TVector3);
  protected
    procedure FullClear;
  public
    procedure Clear;
    function NewSidePair: Integer;
    function Connect(const AThroughSide, AToSide: TPathSideDefinition;
      const AAtNode: TPathNode; const AWithLink: TPathLinkClass): TPathLink;
  public
    property Count: Integer read GetCount;
    property Location: TVector3 read FLocation write SetLocation;
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

implementation

function SideDefinition(const APair: TPathPairNumber;
  const ASide: TPathNodeSideDirection): TPathSideDefinition;
begin
  Result.Number := APair;
  Result.Direction := ASide;
end;

{ TPathLinkSide }

constructor TPathLinkSide.Create;
begin
  FOnChange := TGTEventList.Create;
  FUpdatedOnce := False;
end;

destructor TPathLinkSide.Destroy;
begin
  FOnChange.Free;
  inherited Destroy;
end;

function TPathLinkSide.GetPathTangent: PVector3;
begin
  Result := @FPathTangent;
end;

procedure TPathLinkSide.DoChange;
begin
  FUpdatedOnce := True;
  FOnChange.Call(Self);
end;

{ TPathLink }

constructor TPathLink.Create;
begin
  inherited Create;
  FInvalidated := True;
  FBezierYaw[0] := 0.0;
  FBezierYaw[3] := 0.0;
  FSides[sdA] := TPathLinkSide.Create;
  FSides[sdA].OnChange.RegisterHandler(@HandleLinkSideChanged);
  FSides[sdB] := TPathLinkSide.Create;
  FSides[sdB].OnChange.RegisterHandler(@HandleLinkSideChanged);
  FPathSegments := TPathLinkLinearSegments.Create;
end;

destructor TPathLink.Destroy;
begin
  ClearPath;
  FPathSegments.Free;
  FSides[sdA].Free;
  FSides[sdB].Free;
  inherited Destroy;
end;

function TPathLink.GetLength: Double;
begin
  Deinvalidate;
  Exit(FLength);
end;

function TPathLink.GetMaxSpeed: Double;
begin
  Deinvalidate;
  Exit(FMaxSpeed);
end;

function TPathLink.GetMinSpeed: Double;
begin
  Deinvalidate;
  Exit(FMinSpeed);
end;

function TPathLink.GetPathSegmets: TPathLinkLinearSegments;
begin
  Deinvalidate;
  Exit(FPathSegments);
end;

function TPathLink.GetYaw: Double;
begin
  Deinvalidate;
  Exit(FYaw);
end;

procedure TPathLink.AddPathSegment(const AFrom, ATo: TVector4;
  const ALength: Double);
begin
  FPathSegments.Add(TPathLinkLinearSegment.Create(ALength, AFrom, ATo));
end;

function TPathLink.ClampYaw(const AValue: TVectorFloat): TVectorFloat;
begin
  if AValue < 0.0 then
    Exit(0.0)
  else if AValue > MAX_YAW then
    Exit(MAX_YAW)
  else
    Exit(AValue);
end;

procedure TPathLink.ClearPath;
var
  I: Integer;
begin
  for I := 0 to FPathSegments.Count - 1 do
    FPathSegments[I].Free;
  FPathSegments.Clear;
end;

procedure TPathLink.Deinvalidate;
begin
  BuildPath;
  FInvalidated := False;
end;

procedure TPathLink.HandleLinkSideChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TPathLink.SetLength(const AValue: TVectorFloat);
begin
  if FLength = AValue then
    Exit;
  FLength := AValue;
  DoChange;
end;

procedure TPathLink.SetMaxSpeed(const AValue: TVectorFloat);
begin
  FMaxSpeed := AValue;
end;

procedure TPathLink.SetMinSpeed(const AValue: TVectorFloat);
begin
  FMinSpeed := AValue;
end;

procedure TPathLink.SetYaw(const AValue: TVectorFloat);
begin
  FYaw := ClampYaw(AValue);
  FBezierYaw[1] := FYaw;
  FBezierYaw[2] := FYaw;
end;

procedure TPathLink.UpdateSpeedLimits(const ARadius: TVectorFloat);
var
  vMin, vMax: TVectorFloat;
begin
  vMin := sqrt(GRAVITY * ARadius * tan(FYaw - FYaw * YAW_TOLERANCE_DOWN));
  vMax := sqrt(GRAVITY * ARadius * tan(FYaw + FYaw * YAW_TOLERANCE_UP));
  if FMinSpeed <> vMin then
    SetMinSpeed(vMin);
  if FMaxSpeed <> vMax then
    SetMaxSpeed(vMax);
end;

procedure TPathLink.DoChange;
begin
  inherited DoChange;
end;

procedure TPathLink.Invalidate;
begin
  FInvalidated := True;
  DoChange;
end;

{ TPathLinkLinearSegment }

constructor TPathLinkLinearSegment.Create(const ALength: TVectorFloat; AFrom,
  ATo: TVector4);
begin
  FLength := ALength;
  FFrom := AFrom;
  FTo := ATo;
end;

function TPathLinkLinearSegment.Interpolate(var x, Movement: TVectorFloat; out
  p: TVector4): Boolean;
var
  step: TVectorFloat;
  t: TVectorFloat;
begin
  step := Movement;
  Movement -= step;
  x += step;
  t := x / FLength;
  if t > 1.0 then
    Exit(False);
  p := (1-t) * FFrom + t * FTo;
  Exit(True);
end;

{ TPathLinkBezier }

procedure TPathLinkBezier.BuildPath;
var
  tStep, tPrev, t, l, lSegment: TVectorFloat;
  vCurr, vPrev: TVector3;
  vTanPrev, vTanCurr: TVector2;
  radiusMax, radius, alpha: TVectorFloat;
begin
  UpdateCurve;
  ClearPath;
  l := 0.0;
  tPrev := 0.0;
  vPrev := FBezierPath ** tPrev;
  if FBezierPath[0].Vec2 = FBezierPath[1].Vec2 then
    vTanPrev := Normalize(FBezierPath[3].Vec2 - FBezierPath[0].Vec2)
  else
    vTanPrev := Normalize(FBezierPath[0].Vec2 - FBezierPath[1].Vec2);
  tStep := BLengthAutoAccuracy(FBezierPath);
  if tStep >= 0.1 then
    tStep := 0.1;
  radiusMax := NegInfinity;
  t := tStep;
  while t < 1.0 do
  begin
    vCurr := FBezierPath ** t;
    vTanCurr := vCurr.Vec2 - vPrev.Vec2;
    lSegment := VLength(vTanCurr);
    // Segment is correctly sized, so add it and continue
    if Abs(lSegment - BEZIER_PATH_STEP_LENGTH) < BEZIER_PATH_STEP_EPSILON then
    begin
      vTanCurr := vTanCurr / lSegment;
      alpha := arccos(vTanPrev * vTanCurr);
      radius := alpha / lSegment;
      if radius > radiusMax then
        radiusMax := radius;
      AddPathSegment(Vector4(vPrev, FBezierYaw ** tPrev), Vector4(vCurr, FBezierYaw ** t), lSegment);
      tPrev := t;
      t += tStep;
      vPrev := vCurr;
      l += lSegment;
      Continue;
    end;
    // Segment is too small, so increase step size by one tenth and retry.
    if lSegment < BEZIER_PATH_STEP_LENGTH then
    begin
      tStep += tStep / 10.0;
      t := tPrev + tStep;
      Continue;
    end;
    // Segment is too large, retry with half step size
    tStep /= 2.0;
    t := tPrev + tStep;
  end;
  // Last step was too large, so add one linear segment with correct length
  if t > 1.0 then
  begin
    t := 1.0;
    vCurr := FBezierPath ** 1.0;
    lSegment := VLength(vCurr - vPrev);
    l += lSegment;
    AddPathSegment(Vector4(vPrev, FBezierYaw ** tPrev), Vector4(vCurr, FBezierYaw ** 1.0), lSegment);
  end;

  FLength := l;
  FMaxRadius := radiusMax;
  UpdateSpeedLimits(radiusMax);
end;

procedure TPathLinkBezier.SetMaxRadius(const AValue: TVectorFloat);
begin
  if FMaxRadius = AValue then
    Exit;
  FMaxRadius := AValue;
  UpdateSpeedLimits(FMaxRadius);
end;

procedure TPathLinkBezier.SetYaw(const AValue: TVectorFloat);
var
  ClampedYaw: TVectorFloat;
begin
  ClampedYaw := ClampYaw(AValue);
  if FYaw = AValue  then
    Exit;
  inherited SetYaw(AValue);
  UpdateSpeedLimits(FMaxRadius);
end;

procedure TPathLinkBezier.UpdateCurve;
var
  HalfLen: TVectorFloat;
begin
  FBezierPath[0] := FSides[sdA].FNode.FLocation;
  FBezierPath[3] := FSides[sdB].FNode.FLocation;
  HalfLen := VLength(FBezierPath[0] - FBezierPath[3]) / 2.0;
  FBezierPath[1] := FBezierPath[0] + Normalize(FSides[sdA].FPathTangent) * HalfLen;
  FBezierPath[2] := FBezierPath[3] + Normalize(FSides[sdB].FPathTangent) * HalfLen;
end;

{ TPathLinkStraight }

procedure TPathLinkStraight.BuildPathSimple(const vFrom, vTo: TVector4);
var
  len: TVectorFloat;
begin
  len := VLength(vFrom.Vec2 - vTo.Vec2);
  AddPathSegment(vFrom, vTo, len);
  SetMinSpeed(NegInfinity);
  SetMaxSpeed(Infinity);
  FLength := len;
end;

procedure TPathLinkStraight.BuildPath;
var
  vFrom, vTo: TVector4;
begin
  vFrom := Vector4(FSides[sdA].FNode.FLocation, 0.0);
  vTo := Vector4(FSides[sdB].FNode.FLocation, 0.0);
  if vFrom.Z = vTo.Z then
    BuildPathSimple(vFrom, vTo)
  else
    inherited BuildPath;
end;

procedure TPathLinkStraight.SetYaw(const AValue: TVectorFloat);
begin
  // Straight cannot have yaw
  inherited SetYaw(0.0);
end;

procedure TPathLinkStraight.UpdateCurve;
begin
  inherited UpdateCurve;
  FBezierPath[1].Vec2 := FBezierPath[0].Vec2;
  FBezierPath[2].Vec2 := FBezierPath[3].Vec2;
end;

{ TPathLinkArc }

procedure TPathLinkArc.BuildPath;
var
  vTA, vTB: TVector2;
  vA, vB: TVector3;
  a, aPrev, aStep: TVectorFloat;
  t, tPrev, tStep: TVectorFloat;
  vCurr, vHY: TVector2;
  BezierHeightYaw: TCubicBezier2;
begin
  vTA := FSides[sdA].FPathTangent.Vec2;
  vTB := FSides[sdB].FPathTangent.Vec2;
  vA := FSides[sdA].FNode.FLocation;
  vB := FSides[sdB].FNode.FLocation;
  if (vTA = -vTB) then
  begin
    inherited;
    Exit;
  end;
  FAngleStart := VecToAngle(vTA) - Pi / 2.0;
  FAngleEnd := VecToAngle(vTB) + Pi / 2.0;
  FCosSinStart := AngleToVec(FAngleStart);
  FCosSinEnd := AngleToVec(FAngleEnd);
  FCenter.X := -(vA.X*FCosSinEnd.Cos - vB.X*FCosSinStart.Cos)/(FCosSinStart.Cos-FCosSinEnd.Cos);
  FCenter.Y := -(vA.Y*FCosSinEnd.Sin - vB.Y*FCosSinStart.Sin)/(FCosSinStart.Sin-FCosSinEnd.Sin);
  FRadius := VLength(vA.Vec2 - FCenter);

  BezierHeightYaw[0] := Vector2(vA.Z, 0.0);
  BezierHeightYaw[1] := Vector2(vA.Z, FYaw);
  BezierHeightYaw[2] := Vector2(vB.Z, FYaw);
  BezierHeightYaw[3] := Vector2(vB.Z, 0.0);

  aStep := ARC_PATH_STEP_LENGTH / FRadius;
  if FAngleEnd < FAngleStart then
    aStep := -aStep;
  aPrev := FAngleStart;
  a := aPrev + aStep;
  tPrev := 0.0;
  tStep := aStep / (FAngleEnd - FAngleStart);
  t := tStep;
  while a < FAngleEnd do
  begin
    vCurr := AngleToVec(a);
    vHY := BezierHeightYaw ** t;

    AddPathSegment(Vector4(vCurr, vHY.X, vHY.Y), Vector4(vCurr, vHY.X, vHY.Y), ARC_PATH_STEP_LENGTH);

    aPrev := a;
    a += aStep;
    tPrev := t;
    t += tStep;
  end;
  if a > FAngleEnd then
  begin
    t := 1.0;
    a := FAngleEnd;

    vCurr := AngleToVec(a);
    vHY := BezierHeightYaw ** t;

    AddPathSegment(Vector4(vCurr, vHY.X, vHY.Y), Vector4(vCurr, vHY.X, vHY.Y), Abs(a - aPrev) * FRadius);
  end;

  FLength := FRadius * (FAngleEnd - FAngleStart);
  UpdateSpeedLimits(FRadius);
end;

{ TPathNodeLink }

destructor TPathNodeLink.Destroy;
begin
  SetLink(nil);
  inherited Destroy;
end;

procedure TPathNodeLink.LinkLink(const ALink: TPathLink);
begin
  ALink.OnChange.RegisterHandler(@LinkChanged);
  ALink.OnDestruction.RegisterHandler(@LinkDeleting);
end;

procedure TPathNodeLink.LinkChanged(Sender: TObject);
begin
  DoChange;
end;

procedure TPathNodeLink.LinkDeleting(Sender: TObject);
begin
  Free;
end;

procedure TPathNodeLink.SetLink(const ALink: TPathLink);
begin
  if FLink <> nil then
    UnlinkLink(FLink);
  FLink := ALink;
  if FLink <> nil then
    LinkLink(FLink);
end;

procedure TPathNodeLink.SetOwner(const AOwner: TPathNodeSide);
begin
  FOwner := AOwner;
end;

procedure TPathNodeLink.UnlinkLink(const ALink: TPathLink);
begin
  ALink.OnChange.UnRegisterHandler(@LinkChanged);
  ALink.OnDestruction.UnRegisterHandler(@LinkDeleting);
end;

procedure TPathNodeLink.UpdateLink;
begin
  FLink.FSides[FSide].FNode := FOwner.FOwner.FOwner;
  FLink.FSides[FSide].FNodeLink := Self;
  WriteLn(FormatVector(FLink.FSides[FSide].FPathTangent), ' = ', FormatVector(FOwner.FTangent));
  if FLink.FSides[FSide].FPathTangent <> FOwner.FTangent then
  begin
    FLink.FSides[FSide].FPathTangent := FOwner.FTangent;
    FLink.FSides[FSide].DoChange;
  end;
end;

{ TPathNodeSide }

constructor TPathNodeSide.Create;
begin
  inherited Create;
  FLinks := TPathNodeLinks.Create;
end;

destructor TPathNodeSide.Destroy;
begin
  Clear;
  FLinks.Free;
  inherited Destroy;
end;

function TPathNodeSide.GetCount: Integer;
begin
  Result := FLinks.Count;
end;

function TPathNodeSide.GetLink(Index: Integer): TPathNodeLink;
begin
  Result := FLinks[Index];
end;

procedure TPathNodeSide.Clear;
var
  I: Integer;
begin
  for I := 0 to FLinks.Count - 1 do
  begin
    UnlinkNodeLink(FLinks[I]);
    FLinks[I].Free;
  end;
  FLinks.Clear;
end;

procedure TPathNodeSide.LinkNodeLink(const ALink: TPathNodeLink);
begin
  ALink.OnDestruction.RegisterHandler(@NodeLinkDeleting);
end;

function TPathNodeSide.New(const ALink: TPathLink;
  const AsSide: TPathNodeSideDirection): TPathNodeLink;
begin
  Result := TPathNodeLink.Create;
  Result.SetOwner(Self);
  Result.SetLink(ALink);
  Result.FSide := AsSide;
  LinkNodeLink(Result);
  FLinks.Add(Result);
end;

procedure TPathNodeSide.NodeLinkDeleting(Sender: TObject);
begin
  Remove(Sender as TPathNodeLink);
end;

procedure TPathNodeSide.Remove(const ALink: TPathNodeLink);
begin
  UnlinkNodeLink(ALink);
  FLinks.Remove(ALink);
end;

procedure TPathNodeSide.SetOwner(const AOwner: TPathNodeSidePair);
begin
  FOwner := AOwner;
end;

procedure TPathNodeSide.SetTangent(const ATangent: TVector3);
var
  I: Integer;
begin
  FTangent := ATangent;
  for I := 0 to FLinks.Count - 1 do
    FLinks[I].UpdateLink;
end;

procedure TPathNodeSide.UnlinkNodeLink(const ALink: TPathNodeLink);
begin
  ALink.OnDestruction.UnRegisterHandler(@NodeLinkDeleting);
end;

procedure TPathNodeSide.UpdateLinks;
var
  I: Integer;
begin
  for I := 0 to FLinks.Count - 1 do
    FLinks[I].FLink.DoChange;
end;

function TPathNodeSide.ToDefinition: TPathSideDefinition;
begin
  Result.Number := FOwner.FNumber;
  Result.Direction := FDirection;
end;

{ TPathNodeSidePair }

constructor TPathNodeSidePair.Create;
begin
  inherited Create;
  FSides[sdA] := TPathNodeSide.Create;
  FSides[sdA].FDirection := sdA;
  FSides[sdA].SetOwner(Self);
  FSides[sdB] := TPathNodeSide.Create;
  FSides[sdB].FDirection := sdB;
  FSides[sdB].SetOwner(Self);
end;

destructor TPathNodeSidePair.Destroy;
begin
  FSides[sdA].Free;
  FSides[sdB].Free;
  inherited Destroy;
end;

function TPathNodeSidePair.GetSide(ASide: TPathNodeSideDirection
  ): TPathNodeSide;
begin
  Result := FSides[ASide];
end;

procedure TPathNodeSidePair.SetTangent(const AValue: TVector3);
begin
  if FTangent = AValue then
    Exit;
  FTangent := AValue;
  FSides[sdA].SetTangent(AValue);
  FSides[sdB].SetTangent(-AValue);
end;

procedure TPathNodeSidePair.SetNumber(const ANumber: TPathPairNumber);
begin
  FNumber := ANumber;
end;

procedure TPathNodeSidePair.SetOwner(const AOwner: TPathNode);
begin
  FOwner := AOwner;
end;

procedure TPathNodeSidePair.UpdateSides;
begin
  FSides[sdA].UpdateLinks;
  FSides[sdB].UpdateLinks;
end;

{ TPathNode }

constructor TPathNode.Create;
begin
  inherited Create;
  FSidePairs := TPathNodeSidePairs.Create;
  NewSidePair;
end;

destructor TPathNode.Destroy;
begin
  FullClear;
  FSidePairs.Free;
  inherited Destroy;
end;

function TPathNode.GetCount: Integer;
begin
  Result := FSidePairs.Count;
end;

function TPathNode.GetSidePair(Index: Integer): TPathNodeSidePair;
begin
  Result := FSidePairs[Index];
end;

procedure TPathNode.SetLocation(const AValue: TVector3);
var
  I: Integer;
begin
  if FLocation = AValue then
    Exit;
  FLocation := AValue;
  for I := 0 to FSidePairs.Count - 1 do
    FSidePairs[I].UpdateSides;
end;

procedure TPathNode.FullClear;
var
  I: Integer;
begin
  for I := 0 to FSidePairs.Count - 1 do
    FSidePairs[I].Free;
  FSidePairs.Clear;
end;

procedure TPathNode.Clear;
begin
  FullClear;
  NewSidePair;
end;

function TPathNode.NewSidePair: Integer;
var
  Pair: TPathNodeSidePair;
begin
  Pair := TPathNodeSidePair.Create;
  Pair.SetOwner(Self);
  Result := FSidePairs.Add(Pair);
end;

function TPathNode.Connect(const AThroughSide, AToSide: TPathSideDefinition;
  const AAtNode: TPathNode; const AWithLink: TPathLinkClass): TPathLink;
var
  SideHere, SideThere: TPathNodeSide;
begin
  SideHere := FSidePairs[AThroughSide.Number].FSides[AThroughSide.Direction];
  SideThere := AAtNode.FSidePairs[AToSide.Number].FSides[AToSide.Direction];
  Result := AWithLink.Create;
  SideHere.New(Result, sdA).UpdateLink;
  SideThere.New(Result, sdB).UpdateLink;
end;

end.

