unit TransportGeometryGL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GLGeometry, Geometry, TransportGraph, GTBase, stwIntMap,
  fgl;

type
  ETransportGeometryError = class (Exception);

  TTransportGeometryMaterials = class;
  TTransportGeometryManager = class;

  { TTransportGeometrySlice }

  TTransportGeometrySlice = class (TObject)
  public
    constructor Create(const AManager: TTransportGeometryManager); virtual;
  protected
    FOrigin: TVector3;
    FTangent: TVector3;
    FSideStart, FSideEnd: TVector3;
    FUpStart, FUpEnd: TVector3;
    FEnd: TVector3;
    FLength: TVectorFloat;

    FManager: TTransportGeometryManager;
    FMaterials: TTransportGeometryMaterials;
  protected
    procedure ExtractGeometryInfo(const ASegment: TPathLinkLinearSegment);
  public
    procedure GenerateFromSegment(const ASegment: TPathLinkLinearSegment); virtual;
  end;

  { TTransportGeometrySliceLines }

  TTransportGeometrySliceLines = class (TTransportGeometrySlice)
  private
    FGeometry: TGLGeometryObject;
  public
    procedure GenerateFromSegment(const ASegment: TPathLinkLinearSegment);
       override;
  end;

  TTransportGeometrySlices = specialize TFPGList<TTransportGeometrySlice>;

  { TTransportGeometryNode }

  TTransportGeometryNode = class (TGTBaseObject)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FGraphNode: TPathLink;
    FInvalidated: Boolean;
    FManager: TTransportGeometryManager;
    FSlices: TTransportGeometrySlices;
  protected
    procedure Clear;
    procedure GraphNodeChanged(Sender: TObject);
    procedure GraphNodeDeleting(Sender: TObject);
    procedure LinkGraphNode(const ANode: TPathLink);
    procedure SetGraphNode(const AGraphNode: TPathLink);
    procedure SetManager(const AManager: TTransportGeometryManager);
    procedure UnlinkGraphNode(const ANode: TPathLink);
  public
    procedure Invalidate;
    procedure Update;
  public
    property GraphNode: TPathLink read FGraphNode;
  end;

  { TTransportGeometryMaterials }

  TTransportGeometryMaterials = class (TGTBaseObject)
  public
    destructor Destroy; override;
  private
    FMaterials: array [0..3] of TGLMaterial;
  private
    function GetMaterial(AIndex: integer): TGLMaterial;
    procedure SetMaterial(AIndex: integer; const AValue: TGLMaterial);
  protected
    procedure LinkMaterial(const AMaterial: TGLMaterial);
    procedure MaterialChanged(Sender: TObject);
    procedure MaterialDeleting(Sender: TObject);
    procedure UnlinkMaterial(const AMaterial: TGLMaterial);
  public
    property DebugLine: TGLMaterial index 3 read GetMaterial write SetMaterial;
    property Rail: TGLMaterial index 0 read GetMaterial write SetMaterial;
    property RailBasement: TGLMaterial index 1 read GetMaterial write SetMaterial;
    property RailStones: TGLMaterial index 2 read GetMaterial write SetMaterial;
  end;

  TTransportGeometryMap = specialize TstwGIntHashMap<TTransportGeometryNode>;
  TTransportGeometryNodes = specialize TFPGList<TTransportGeometryNode>;

  { TTransportGeometryManager }

  TTransportGeometryManager = class (TObject)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FMap: TTransportGeometryMap;
    FNodes: TTransportGeometryNodes;
    FMaterials: TTransportGeometryMaterials;
  public
    property Materials: TTransportGeometryMaterials read FMaterials;
  public
    procedure AddTransportNode(ANode: TPathNode; Recursive: Boolean);
    procedure Clear;
    procedure Update;
  end;

implementation

{ TTransportGeometrySlice }

constructor TTransportGeometrySlice.Create(
  const AManager: TTransportGeometryManager);
begin
  FManager := AManager;
  FMaterials := AManager.Materials;
end;

procedure TTransportGeometrySlice.ExtractGeometryInfo(
  const ASegment: TPathLinkLinearSegment);
var
  FromInfo, ToInfo: TVector4;
begin
  FromInfo := ASegment.FromInfo;
  ToInfo := ASegment.ToInfo;
  FOrigin := FromInfo.Vec3;
  FEnd := ToInfo.Vec3;
  FTangent := FEnd - FOrigin;
  FLength := NormalizeInPlace(FTangent);

  if FromInfo.W = 0.0 then
    FUpStart := V_EUP
  else
    FUpStart := RotationMatrix(FTangent, FromInfo.W) * V_EUP;

  if ToInfo.W = 0.0 then
    FUpEnd := V_EUP
  else if FromInfo.W = ToInfo.W then
    FUpEnd := FUpStart
  else
    FUpEnd := RotationMatrix(FTangent, ToInfo.W) * V_EUP;

  FSideStart := FTangent ** FUpStart;
  FSideEnd := FTangent ** FUpEnd;
end;

procedure TTransportGeometrySlice.GenerateFromSegment(
  const ASegment: TPathLinkLinearSegment);
begin
  ExtractGeometryInfo(ASegment);
end;

{ TTransportGeometrySliceLines }

procedure TTransportGeometrySliceLines.GenerateFromSegment(
  const ASegment: TPathLinkLinearSegment);
begin
  FGeometry.Free;
  inherited;
  FGeometry := TGLGeometryObject.Create(FMaterials.DebugLine, 2);
  with FGeometry.Format as TGLGeometryFormatP4C4 do
  begin
    UseMap(FGeometry.Map);
    Position[0] := Vector4(FOrigin, 1.0);
    Color[0] := Vector4(1.0, 0.0, 0.0, 1.0);
    Position[1] := Vector4(FEnd, 1.0);
    Color[1] := Vector4(0.0, 0.0, 1.0, 1.0);
    UseMap(nil);
  end;
end;

{ TTransportGeometryNode }

constructor TTransportGeometryNode.Create;
begin
  inherited Create;
  FGraphNode := nil;
  FInvalidated := True;
  FSlices := TTransportGeometrySlices.Create;
end;

destructor TTransportGeometryNode.Destroy;
begin
  Clear;
  FSlices.Free;
  SetGraphNode(nil);
  inherited Destroy;
end;

procedure TTransportGeometryNode.Clear;
var
  I: Integer;
begin
  for I := 0 to FSlices.Count - 1 do
    FSlices[I].Free;
  FSlices.Clear;
end;

procedure TTransportGeometryNode.GraphNodeChanged(Sender: TObject);
begin
  DoChange;
end;

procedure TTransportGeometryNode.GraphNodeDeleting(Sender: TObject);
begin
  Free;
end;

procedure TTransportGeometryNode.LinkGraphNode(const ANode: TPathLink);
begin
  ANode.OnChange.RegisterHandler(@GraphNodeChanged);
  ANode.OnDestruction.RegisterHandler(@GraphNodeDeleting);
end;

procedure TTransportGeometryNode.SetGraphNode(const AGraphNode: TPathLink);
begin
  if FGraphNode = AGraphNode then
    Exit;
  if FGraphNode <> nil then
    UnlinkGraphNode(FGraphNode);
  FGraphNode := AGraphNode;
  if FGraphNode <> nil then
    LinkGraphNode(FGraphNode);
  Invalidate;
end;

procedure TTransportGeometryNode.SetManager(
  const AManager: TTransportGeometryManager);
begin
  if FManager = AManager then
    Exit;
  FManager := AManager;
  Invalidate;
end;

procedure TTransportGeometryNode.UnlinkGraphNode(const ANode: TPathLink);
begin
  ANode.OnChange.UnRegisterHandler(@GraphNodeChanged);
  ANode.OnDestruction.UnRegisterHandler(@GraphNodeDeleting);
end;

procedure TTransportGeometryNode.Invalidate;
begin
  FInvalidated := True;
  Clear;
end;

procedure TTransportGeometryNode.Update;
var
  I: Integer;
  Segments: TPathLinkLinearSegments;
  Slice: TTransportGeometrySlice;
begin
  FInvalidated := False;
  Segments := FGraphNode.PathSegmets;
  for I := 0 to Segments.Count - 1 do
  begin
    Slice := TTransportGeometrySliceLines.Create(FManager);
    FSlices.Add(Slice);
    Slice.GenerateFromSegment(Segments[I]);
  end;
end;

{ TTransportGeometryMaterials }

destructor TTransportGeometryMaterials.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FMaterials) do
  begin
    SetMaterial(I, nil);
  end;
  inherited Destroy;
end;

function TTransportGeometryMaterials.GetMaterial(AIndex: integer): TGLMaterial;
begin
  Result := FMaterials[AIndex];
end;

procedure TTransportGeometryMaterials.SetMaterial(AIndex: integer;
  const AValue: TGLMaterial);
begin
  if FMaterials[AIndex] = AValue then
    Exit;
  if FMaterials[AIndex] <> nil then
    UnlinkMaterial(FMaterials[AIndex]);
  FMaterials[AIndex] := AValue;
  if FMaterials[AIndex] <> nil then
    LinkMaterial(FMaterials[AIndex]);
  DoChange;
end;

procedure TTransportGeometryMaterials.LinkMaterial(const AMaterial: TGLMaterial
  );
begin
  AMaterial.OnChange.RegisterHandler(@MaterialChanged);
  AMaterial.OnDestruction.RegisterHandler(@MaterialDeleting);
end;

procedure TTransportGeometryMaterials.MaterialChanged(Sender: TObject);
begin
  DoChange;
end;

procedure TTransportGeometryMaterials.MaterialDeleting(Sender: TObject);
var
  Mat: TGLMaterial;
  I: Integer;
begin
  Mat := Sender as TGLMaterial;
  UnlinkMaterial(Mat);
  for I := 0 to High(FMaterials) do
    if FMaterials[I] = Mat then
    begin
      FMaterials[I] := nil;
      Exit;
    end;
  DoChange;
end;

procedure TTransportGeometryMaterials.UnlinkMaterial(
  const AMaterial: TGLMaterial);
begin
  AMaterial.OnDestruction.UnRegisterHandler(@MaterialDeleting);
  AMaterial.OnChange.UnRegisterHandler(@MaterialChanged);
end;

{ TTransportGeometryManager }

constructor TTransportGeometryManager.Create;
begin
  FMaterials := TTransportGeometryMaterials.Create;
  FMap := TTransportGeometryMap.Create(3079);
  FNodes := TTransportGeometryNodes.Create;
end;

destructor TTransportGeometryManager.Destroy;
begin
  FNodes.Free;
  FMap.Free;
  FMaterials.Free;
  inherited Destroy;
end;

procedure TTransportGeometryManager.AddTransportNode(ANode: TPathNode;
  Recursive: Boolean);
var
  I, K: Integer;
  J: TPathNodeSideDirection;
  Pair: TPathNodeSidePair;
  Side: TPathNodeSide;
  NodeLink: TPathNodeLink;
  Link: TPathLink;
  GeoNode: TTransportGeometryNode;
begin
  for I := 0 to ANode.Count - 1 do
  begin
    Pair := ANode[I];
    for J := Low(TPathNodeSideDirection) to High(TPathNodeSideDirection) do
    begin
      Side := Pair[J];
      for K := 0 to Side.Count - 1 do
      begin
        NodeLink := Side[K];
        Link := NodeLink.Link;
        if not FMap.HasKey(ptruint(Link)) then
        begin
          GeoNode := TTransportGeometryNode.Create;
          FMap.Items[ptruint(Link)] := GeoNode;
          FNodes.Add(GeoNode);
          GeoNode.SetGraphNode(Link);
          GeoNode.SetManager(Self);
          GeoNode.Invalidate;
          if Recursive then
            AddTransportNode(Link.Sides[SideOpposite(NodeLink.Side)].Node, True);
        end;
      end;
    end;
  end;
end;

procedure TTransportGeometryManager.Clear;
var
  I: Integer;
begin
  for I := 0 to FNodes.Count - 1 do
    FNodes[I].Free;
  FMap.Clear;
  FNodes.Clear;
end;

procedure TTransportGeometryManager.Update;
var
  I: Integer;
begin
  for I := 0 to FNodes.Count - 1 do
    FNodes[I].Update;
end;

end.

