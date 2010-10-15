unit TerrainGeometryDynamic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Geometry, GLGeometry, Math, dglOpenGL;

const
  TERRAIN_GEOMETRY_CHUNK_SIZE = 64;
  TERRAIN_GEOMETRY_BLOCK_SIZE = 64;

type
  ETerrainError = class (Exception);

  TQuadTreeNode = packed record
    Subdivide: Boolean;
    D2Value: Byte;
  end;

  { TTerrainSource }

  TTerrainSource = class (TObject)
  public
    function Height: Integer; virtual; abstract;
    procedure GetData(const X, Y, W, H: Integer; const TargetBuffer: PSingle); virtual; abstract;
    function Width: Integer; virtual; abstract;
  end;

  TTerrain = class (TObject)
  public
    constructor Create(const AWidth, AHeight: Integer; const ASource: TTerrainSource;
      const AMaterial: TGLMaterial);
    destructor Destroy; override;
  private
    FD2Invalidated: Boolean;
    FGeometry: TGLGeometryObjectDynamic;
    FHeight: Integer;
    FHeightmap: array of Single;
    FMaterial: TGLMaterial;
    FQuadTree: array of TQuadTreeNode;
    FWidth: Integer;
  protected
    procedure CalcD2Value(X, Y, Size: Integer);
    function GetHeight(AX, AY: Integer): Single; inline;
    procedure RecalcD2Values;
    procedure SetupLOD(const CameraPosition: TVector3; ResolutionFactor, ErrorReductionFactor: Double);
  public
    procedure DrawDirect;
    procedure UpdateForFrustum(const CameraPosition: TVector3; HighResolutionRadius, GeneralResolutionFactor: Double);
  public
    property Geometry: TGLGeometryObjectDynamic read FGeometry;
  end;

implementation

{ TTerrain }

constructor TTerrain.Create(const AWidth, AHeight: Integer;
  const ASource: TTerrainSource; const AMaterial: TGLMaterial);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  if ((FWidth-1) mod 64 <> 0) or ((FHeight-1) mod 64 <> 0) or (FHeight <> FWidth) then
    raise ETerrainError.CreateFmt('Invalid terrain size: %d×%d, scales are required to be equal and multiples of %d.', [TERRAIN_GEOMETRY_BLOCK_SIZE]);

  FMaterial := AMaterial;
  if not (FMaterial.Format is TGLGeometryFormatP4C4) then
    raise ETerrainError.Create('Format must have at least P4C4T2N3F.');

  SetLength(FHeightmap, FWidth * FHeight);
  SetLength(FQuadTree, FWidth * FHeight);

  FillByte(FQuadTree[0], SizeOf(Boolean), 0);
  if (ASource.Width < AWidth) or (ASource.Height < AHeight) then
    raise ETerrainError.CreateFmt('Supplied source has an invalid size %d×%d. At least %d×%d is required.', [ASource.Width, ASource.Height, FWidth,FHeight]);
  ASource.GetData(0, 0, FWidth, FHeight, @FHeightmap[0]);

  CalcD2Value((FWidth-1) div 2, (FHeight-1) div 2, (FWidth-1) div 2);
  FD2Invalidated := False;
end;

destructor TTerrain.Destroy;
begin
  inherited Destroy;
end;

procedure TTerrain.CalcD2Value(X, Y, Size: Integer);
var
  I, HalfSize: Integer;
  EdgeHeights: array [0..3] of Single;
  ExpectedHeight, RealHeight: Single;

  DH, DHMax: Single;

const
  Edges: array [0..3] of array [0..1] of ShortInt = ((1, 1), (1, -1), (-1, -1), (-1, 1));
  DHLines: array [0..5] of array [0..1] of Byte = ((0, 1), (1, 2), (2, 3), (3, 0), (0, 2), (1, 3));
  DHPositions: array [0..5] of array [0..1] of ShortInt = ((1, 0), (0, -1), (-1, 0), (0, 1), (0, 0), (0, 0));
begin
  for I := 0 to 3 do
    EdgeHeights[I] := GetHeight(X + Size * Edges[I, 0], Y + Size * Edges[I, 1]);

  DHMax := 0.0;
  for I := 0 to 5 do
  begin
    ExpectedHeight := (EdgeHeights[DHLines[I, 0]] + EdgeHeights[DHLines[I, 1]]) / 2;
    RealHeight := GetHeight(X + DHPositions[I, 0] * Size, Y + DHPositions[I, 1] * Size);

    DH := Abs(ExpectedHeight - RealHeight);
    if DH > DHMax then
      DHMax := DH;
  end;

  FQuadTree[X + Y * FWidth].D2Value := Trunc(DHMax/(2*Size));

  if Size > 1 then
  begin
    HalfSize := Size div 2;
    CalcD2Value(X + HalfSize, Y + HalfSize, HalfSize);
    CalcD2Value(X + HalfSize, Y - HalfSize, HalfSize);
    CalcD2Value(X - HalfSize, Y + HalfSize, HalfSize);
    CalcD2Value(X - HalfSize, Y - HalfSize, HalfSize);
  end;
end;

function TTerrain.GetHeight(AX, AY: Integer): Single;
begin
  Result := FHeightmap[AX + AY * FWidth];
end;

procedure TTerrain.RecalcD2Values;
begin
  CalcD2Value(FWidth div 2, FHeight div 2, FWidth div 2);
  FD2Invalidated := False;
end;

procedure TTerrain.SetupLOD(const CameraPosition: TVector3; ResolutionFactor,
  ErrorReductionFactor: Double);
var
  K: Single;
  TriangleStorage: array of TTriangle3f;
  NormalStorage: array of TTriangle3f;
  StorageCapacity, Count: Integer;

  procedure Push(A, B, C: TVector3; NA, NB, NC: TVector3);
  begin
    if StorageCapacity = Count then
    begin
      Inc(StorageCapacity, FWidth div 4);
      SetLength(TriangleStorage, StorageCapacity);
      SetLength(NormalStorage, StorageCapacity);
    end;
    TriangleStorage[Count][0] := A;
    TriangleStorage[Count][1] := B;
    TriangleStorage[Count][2] := C;
    NormalStorage[Count][0] := NA;
    NormalStorage[Count][1] := NB;
    NormalStorage[Count][2] := NC;
    Inc(Count);
  end;

const
  Vertices: array [1..9] of array [0..1] of ShortInt = ((1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1));
  ChildPos: array [0..3] of array [0..1] of ShortInt = ((1, 1), (1, -1), (-1, -1), (-1, 1));
  NeighbourPos: array [0..3] of array [0..1] of ShortInt = ((1, 0), (0, -1), (-1, 0), (0, 1));

  function CellState(X, Y: Integer): Boolean;
  begin
    if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
      Exit(True)
    else
      Exit(FQuadTree[X + Y * FWidth].Subdivide);
  end;

  procedure CheckQuadCell(X, Y, Size: Integer);
  var
    CellCenter: TVector3;
    HSize: Integer;
    Cell: Integer;
  begin
    Cell := X + Y * FWidth;
    CellCenter := Vector3(X, Y, 0.0);
    if (VLength(CellCenter - CameraPosition)/(Size * 2 * ResolutionFactor * Max(ErrorReductionFactor * FQuadTree[Cell].D2Value, 1.0))) < 1.0 then
    begin
      // subdivide
      FQuadTree[Cell].Subdivide := True;
      if Size > 1 then
      begin
        HSize := Size div 2;
        CheckQuadCell(X + HSize, Y - HSize, HSize);
        CheckQuadCell(X + HSize, Y + HSize, HSize);
        CheckQuadCell(X - HSize, Y - HSize, HSize);
        CheckQuadCell(X - HSize, Y + HSize, HSize);
      end;
    end
    else
      FQuadTree[Cell].Subdivide := False;
  end;

  procedure MakeTriangles(X, Y, Size: Integer);
  var
    HSize: Integer;
    I: Integer;
    Children: array [0..4] of Boolean = (False, False, False, False, False);
    Neighbours: array [0..3] of Boolean;
    MidVertex: TVector3f;

    procedure PushFace(const V1, V2: Integer);
    var
      vStart, vEnd, vNormal: TVector3;
      xStart, xEnd, yStart, yEnd: Integer;
    begin
      (*WriteLn(V1, ' ', V2);
      WriteLn(V1 div 2);
      WriteLn(V2 div 2);*)
      if (V1 and 1 = 1) and (Children[V1 div 2]) then Exit;
      if (V2 and 1 = 1) and (Children[V2 div 2]) then Exit;

      xStart := X + Size * Vertices[V1][0];
      yStart := Y + Size * Vertices[V1][1];
      xEnd := X + Size * Vertices[V2][0];
      yEnd := Y + Size * Vertices[V2][1];

      vStart := Vector3f(xStart, yStart, GetHeight(xStart, yStart));
      vEnd := Vector3f(xEnd, yEnd, GetHeight(xEnd, yEnd));

      (*WriteLn(FormatVector(MidVertex));
      WriteLn(FormatVector(vStart));
      WriteLn(FormatVector(vEnd));
      WriteLn(FormatVector(MidVertex - vStart));
      WriteLn(FormatVector(vStart - vEnd));*)
      vNormal := Normalize((MidVertex - vStart) ** (vStart - vEnd));

      Push(MidVertex, vStart, vEnd, vNormal, vNormal, vNormal);
    end;

  var
    LastVertex: Integer;
  begin
    if not FQuadTree[X + Y * FWidth].Subdivide then
      Exit;

    HSize := Size div 2;
    MidVertex := Vector3f(X, Y, GetHeight(X, Y));

    if Size > 1 then
    begin
      for I := 0 to 3 do
        Children[I] := FQuadTree[X + HSize*ChildPos[I][0] + FWidth * (Y + HSize*ChildPos[I][1])].Subdivide;
      Children[4] := Children[0];
    end;

    for I := 0 to 3 do
      Neighbours[I] := CellState(X + 2*Size*NeighbourPos[I][0], Y + 2*Size*NeighbourPos[I][1]);

    LastVertex := 1;
    for I := 2 to 9 do
    begin
      if (I and 1) = 0 then
      begin
        if Neighbours[(I div 2)-1] then
        begin
          PushFace(LastVertex, I);
          LastVertex := I;
        end;
      end
      else
      begin
        PushFace(LastVertex, I);
        LastVertex := I;
      end;
    end;

    if Size > 1 then
    begin
      MakeTriangles(X + HSize, Y + HSize, HSize);
      MakeTriangles(X + HSize, Y - HSize, HSize);
      MakeTriangles(X - HSize, Y + HSize, HSize);
      MakeTriangles(X - HSize, Y - HSize, HSize);
    end;
  end;

var
  I, J, L: Integer;
  VertexCount: Integer;
  C: Single;
begin
  if FD2Invalidated then
    RecalcD2Values;

  K := ResolutionFactor / (2 * (ResolutionFactor - 1));

  CheckQuadCell((FWidth-1) div 2, (FHeight-1) div 2, (FWidth-1) div 2);
  StorageCapacity := 0;
  Count := 0;
  MakeTriangles((FWidth-1) div 2, (FHeight-1) div 2, (FWidth-1) div 2);

  VertexCount := Count * 3;
  if (FGeometry = nil) then
  begin
    FGeometry := TGLGeometryObjectDynamic.Create(FMaterial.GeometryBuffer, FMaterial.Format, VertexCount);
  end
  else
  begin
    if FGeometry.Count > VertexCount then
      FGeometry.ShrinkTo(VertexCount)
    else
      FGeometry.ExpandTo(VertexCount);
  end;

  J := 0;
  with FGeometry.Format as TGLGeometryFormatP4C4 do
  begin
    UseMap(FGeometry.Map);
    for I := 0 to Count - 1 do
    begin
      for L := 0 to 2 do
      begin
        Position[J+L] := Vector4f(TriangleStorage[I][L], 1.0);
        C := (TriangleStorage[I][L][2] + 1.0) / 2.0;
        Color[J+L] := Vector4f(C, C, C);
        //Normal[J+L] := NormalStorage[I][L];
      end;
      Inc(J, 3);
    end;
    UseMap(nil);
  end;
end;

procedure TTerrain.DrawDirect;
begin
  if FGeometry <> nil then
    FGeometry.DrawDirect(GL_TRIANGLES);
end;

procedure TTerrain.UpdateForFrustum(const CameraPosition: TVector3;
  HighResolutionRadius, GeneralResolutionFactor: Double);
begin
  SetupLOD(CameraPosition, HighResolutionRadius, GeneralResolutionFactor);
end;

end.

