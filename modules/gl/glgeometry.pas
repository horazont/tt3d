unit GLGeometry;
(**********************************************************************
File name: glgeometry.pas
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
  Classes, SysUtils, dglOpenGL, fgl, Geometry, math, GTIntMap, GTBase, GLBase;

type
  EGLGenericBufferError = class (Exception);
    EGLGeometryBufferError = class (EGLGenericBufferError);
      EGLGeometryBufferFormatError = class (EGLGeometryBufferError);
    EGLIndexBufferError = class (EGLGenericBufferError);


  TVertexIndex = TGLUint;
  TVertexIndexList = specialize TFPGList<Integer>;
  TVertexIndicies = array of TVertexIndex;
  TVertexIndiciesMap = array of TVertexIndicies;

  TIndexEntry = record
    Start: TVertexIndex;
    Count: Integer;
  end;
  PIndexEntry = ^TIndexEntry;

  TIndexEntryHandle = PIndexEntry;
  TIndexHandleList = specialize TFPGList<TIndexEntryHandle>;

  TGLBoundingBox = record
    Origin, Dimension: TVector3;
  end;

  { TGLGenericBuffer }

  TGLGenericBuffer = class (TObject)
  public
    constructor Create(const ADataSize: Integer; const APurpose: TGLenum);
    destructor Destroy; override;
  private
    FBufferKind: TGLenum;
    FBuffer: TGLUint;
    FBufferPurpose: TGLenum;
    FCapacity: Integer;
    FData: Pointer;
    FDataItemSize: sizeint;
  protected
    function DataPtr: Pointer;
    procedure DoExpandedBy(const AOldCapacity, ANewCapacity: Integer); virtual;
    procedure Expand;
    class function GetBufferKind: TGLenum; virtual; abstract;
    function NeedsFlush: Boolean; virtual;
    procedure RangeCheck(Index: Integer);
  public
    procedure BindForRendering; virtual;
    procedure Flush; virtual;
    procedure FlushRange(const MinItem, Count: Integer);
    procedure ForceFlush;
    procedure ReadBack;
    procedure UnbindForRendering; virtual;
  end;

  { TGLGeometryBuffer }

  TGLGeometryBuffer = class (TGLGenericBuffer)
  public
    constructor Create(const AVertexSize: Integer; const APurpose: TGLenum = GL_DYNAMIC_DRAW);
    destructor Destroy; override;
  private
    FDirtyVertices: TVertexIndexList;
    FFreeVertices: TVertexIndexList;
  protected
    procedure DoExpandedBy(const AOldCapacity, ANewCapacity: Integer);
       override;
    class function GetBufferKind: TGLenum; override;
    function GetVec1(Vertex: Cardinal; Offset: Integer): Single;
    function GetVec2(Vertex: Cardinal; Offset: Integer): TVector2f;
    function GetVec3(Vertex: Cardinal; Offset: Integer): TVector3f;
    function GetVec4(Vertex: Cardinal; Offset: Integer): TVector4f;
    function NeedsFlush: Boolean; override;
    procedure SetVec1(Vertex: Cardinal; Offset: Integer; Value: Single);
    procedure SetVec2(Vertex: Cardinal; Offset: Integer; Value: TVector2f);
    procedure SetVec3(Vertex: Cardinal; Offset: Integer; Value: TVector3f);
    procedure SetVec4(Vertex: Cardinal; Offset: Integer; Value: TVector4f);
  public
    function AllocateVertices(ACount: Integer): TVertexIndicies;
    procedure DeleteVertices(AVertices: TVertexIndicies);
    procedure Flush; override;
  end;

  { TGLGeometryBufferMap }

  TGLGeometryBufferMap = class (TObject)
  public
    constructor Create(const AParent: TGLGeometryBufferMap);
  private
    FParent: TGLGeometryBufferMap;
  public
    function MapVertex(Vertex: Cardinal): Cardinal; virtual;
  end;

  { TGLGeometryBufferDirectMap }

  TGLGeometryBufferDirectMap = class (TGLGeometryBufferMap)
  public
    constructor Create(const AParent: TGLGeometryBufferMap; const Indicies: TVertexIndicies);
  private
    FIndicies: TVertexIndicies;
  public
    property Indicies: TVertexIndicies read FIndicies;
  public
    function MapVertex(Vertex: Cardinal): Cardinal; override;
  end;

  { TGLGeometryBufferOffsetMap }

  TGLGeometryBufferOffsetMap = class (TGLGeometryBufferMap)
  private
    FOffset: Integer;
  public
    function MapVertex(Vertex: Cardinal): Cardinal; override;
  public
    property Offset: Integer read FOffset write FOffset;
  end;

  { TGLGeometryFormat }

  TGLGeometryFormat = class (TObject)
  public
    constructor Create(const ABuffer: TGLGeometryBuffer); virtual;
  protected
    FBuffer: TGLGeometryBuffer;
    FMap: TGLGeometryBufferMap;
    FNeededVertexSize: Integer;
  protected
    class function GetActualVertexSize: Integer; virtual; abstract;
    function GetVec1(Vertex: Cardinal; Offset: Integer): Single;
    function GetVec2(Vertex: Cardinal; Offset: Integer): TVector2f;
    function GetVec3(Vertex: Cardinal; Offset: Integer): TVector3f;
    function GetVec4(Vertex: Cardinal; Offset: Integer): TVector4f;
    function MapVertex(Vertex: Cardinal): Cardinal;
    procedure SetVec1(Vertex: Cardinal; Offset: Integer; Value: Single);
    procedure SetVec2(Vertex: Cardinal; Offset: Integer; Value: TVector2f);
    procedure SetVec3(Vertex: Cardinal; Offset: Integer; Value: TVector3f);
    procedure SetVec4(Vertex: Cardinal; Offset: Integer; Value: TVector4f);
  public
    property CurrentMap: TGLGeometryBufferMap read FMap;
  public
    procedure BindGLPointer; virtual;
    class function GetNeededVertexSize: Integer;
    procedure UnbindGLPointer; virtual;
    procedure UseMap(const AMap: TGLGeometryBufferMap);
  end;
  TGLGeometryFormatClass = class of TGLGeometryFormat;

  { TGLGeometryFormatP4 }

  TGLGeometryFormatP4 = class (TGLGeometryFormat)
  protected
    class function GetActualVertexSize: Integer; override;
  public
    procedure BindGLPointer; override;
    procedure UnbindGLPointer; override;
  public
    property Position[Vertex: TVertexIndex]: TVector4f index 0 read GetVec4 write SetVec4;
  end;

  { TGLGeometryFormatP4C4 }

  TGLGeometryFormatP4C4 = class (TGLGeometryFormatP4)
  protected
    class function GetActualVertexSize: Integer; override;
  public
    procedure BindGLPointer; override;
    procedure UnbindGLPointer; override;
  public
    property Color[Vertex: TVertexIndex]: TVector4f index 4 * SizeOf(Single) read GetVec4 write SetVec4;
  end;

  { TGLGeometryFormatP4C4T2 }

  TGLGeometryFormatP4C4T2 = class (TGLGeometryFormatP4C4)
  protected
    class function GetActualVertexSize: Integer; override;
  public
    procedure BindGLPointer; override;
    procedure UnbindGLPointer; override;
  public
    property TexCoord0[Vertex: TVertexIndex]: TVector2f index 8 * SizeOf(Single) read GetVec2 write SetVec2;
  end;

  { TGLGeometryFormatP4C4T2N3F }

  TGLGeometryFormatP4C4T2N3F = class (TGLGeometryFormatP4C4)
  protected
    class function GetActualVertexSize: Integer; override;
  public
    procedure BindGLPointer; override;
    procedure UnbindGLPointer; override;
  public
    property Normal[Vertex: TVertexIndex]: TVector3f index 10 * SizeOf(Single) read GetVec3 write SetVec3;
  end;
  TGLGeometryFormatDefault = TGLGeometryFormatP4C4T2N3F;

  { TGLStreamIndexBuffer }

  TGLStreamIndexBuffer = class (TGLGenericBuffer)
  public
    constructor Create(const APurpose: TGLenum = GL_STREAM_DRAW);
  private
    FCount: Integer;
  protected
    class function GetBufferKind: TGLenum; override;
    function NeedsFlush: Boolean; override;
  public
    property Count: Integer read FCount;
  public
    procedure Add(AIndicies: TVertexIndicies); virtual;
    procedure Clear; virtual;
    procedure Draw(mode: TGLenum);
    procedure Dump;
  end;

  { TGLIndexBuffer }

  TGLIndexBuffer = class (TGLStreamIndexBuffer)
  public
    constructor Create(const APurpose: TGLenum = GL_DYNAMIC_DRAW);
    destructor Destroy; override;
  private
    FHandles: TIndexHandleList;
  protected
    function NeedsFlush: Boolean; override;
    procedure UpdateHandles(AAfter: TVertexIndex; AOffset: Integer);
  public
    procedure Add(AIndicies: TVertexIndicies); override;
    function AddHandled(AIndicies: TVertexIndicies): TIndexEntryHandle;
    procedure RemoveIndicies(AHandle: TIndexEntryHandle);
  end;

  { TGLMaterial }

  TGLMaterial = class (TGTBaseObject)
  public
    constructor Create; override;
    constructor Create(const AGeometryBuffer: TGLGeometryBuffer;
      const AFormat: TGLGeometryFormatClass); virtual;
    destructor Destroy; override;
  private
    FBoundAsStream: Boolean;
    FFormat: TGLGeometryFormat;
    FGeometryBuffer: TGLGeometryBuffer;
    FStaticIndexBuffer: TGLIndexBuffer;
    FStreamIndexBuffer: TGLStreamIndexBuffer;
  public
    property Format: TGLGeometryFormat read FFormat;
    property GeometryBuffer: TGLGeometryBuffer read FGeometryBuffer;
    property StaticIndexBuffer: TGLIndexBuffer read FStaticIndexBuffer;
    property StreamIndexBuffer: TGLStreamIndexBuffer read FStreamIndexBuffer;
  public
    procedure BindForRendering(const UseStream: Boolean); virtual;
    procedure DumpStream;
    procedure Render(AsType: TGLenum);
    procedure UnbindForRendering; virtual;
  end;

  { TGLGeometryObject }

  TGLGeometryObject = class (TObject)
  public
    constructor Create(const AMaterial: TGLMaterial; AVertexCount: Integer);
    constructor Create(const ABuffer: TGLGeometryBuffer;
      const AFormat: TGLGeometryFormat; AVertexCount: Integer;
      AStaticIndexBuffer: TGLIndexBuffer = nil;
      AStreamIndexBuffer: TGLStreamIndexBuffer = nil);
    destructor Destroy; override;
  private
    FCount: Integer;
    FVBIndicies: TVertexIndicies;
    FIBIndicies: TVertexIndicies;
    FStaticIndexBuffer: TGLIndexBuffer;
    FStaticIndexHandle: TIndexEntryHandle;
    procedure SetStaticIndexBuffer(const AValue: TGLIndexBuffer);
  protected
    FFormat: TGLGeometryFormat;
    FGeometryBuffer: TGLGeometryBuffer;
    FMap: TGLGeometryBufferMap;
  protected
    function CreateMap: TGLGeometryBufferMap; virtual;
    function GetIndexBufferIndicies(const AVBIndicies: TVertexIndicies; const ACount: Integer): TVertexIndicies; virtual;
  public
    procedure AddToStreamBuffer(const AStreamBuffer: TGLStreamIndexBuffer);
    procedure DrawDirect(mode: TGLenum);
  public
    property Count: Integer read FCount;
    property GeometryBuffer: TGLGeometryBuffer read FGeometryBuffer;
    property Format: TGLGeometryFormat read FFormat;
    property Map: TGLGeometryBufferMap read FMap;
    property StaticIndexBuffer: TGLIndexBuffer read FStaticIndexBuffer write SetStaticIndexBuffer;
  end;

  { TGLGeometryObjectDynamic }

  TGLGeometryObjectDynamic = class (TGLGeometryObject)
  public
    constructor Create(const ABuffer: TGLGeometryBuffer;
       const AFormat: TGLGeometryFormat; AVertexCount: Integer;
       AStaticIndexBuffer: TGLIndexBuffer = nil;
       AStreamIndexBuffer: TGLStreamIndexBuffer = nil);
    constructor Create(const AMaterial: TGLMaterial; AVertexCount: Integer);
  private
    FCapacity: Integer;
  public
    procedure ExpandTo(const ANewCount: Integer);
    procedure ShrinkTo(const ANewCount: Integer);
  end;

  { TGLGeometryQuadsForTris }

  TGLGeometryQuadsForTris = class(TGLGeometryObject)
  public
    constructor Create(const ABuffer: TGLGeometryBuffer;
       const AFormat: TGLGeometryFormat; AQuadCount: Integer;
       AStaticIndexBuffer: TGLIndexBuffer = nil;
       AStreamIndexBuffer: TGLStreamIndexBuffer = nil);
  protected
    function GetIndexBufferIndicies(const AVBIndicies: TVertexIndicies;
      const ACount: Integer): TVertexIndicies; override;
  end;

  { TGLGeometryTerrainSectionForTris }

  TGLGeometryTerrainSectionForTris = class (TGLGeometryObject)
  public
    constructor Create(const ABuffer: TGLGeometryBuffer;
       const AFormat: TGLGeometryFormat; AWidth, AHeight: Integer;
       AStaticIndexBuffer: TGLIndexBuffer = nil;
       AStreamIndexBuffer: TGLStreamIndexBuffer = nil);
  private
    FWidth, FHeight: Integer;
  protected
    function GetIndexBufferIndicies(const AVBIndicies: TVertexIndicies;
      const ACount: Integer): TVertexIndicies; override;
  end;

implementation

{ TGLGenericBuffer }

constructor TGLGenericBuffer.Create(const ADataSize: Integer;
  const APurpose: TGLenum);
begin
  FBufferPurpose := APurpose;
  FBufferKind := GetBufferKind;
  FDataItemSize := ADataSize;
  if FDataItemSize <= 0 then
    raise EGLGenericBufferError.CreateFmt('Invalid data size: %d', [FDataItemSize]);
  FCapacity := 0;

  glGenBuffers(1, @FBuffer);
end;

destructor TGLGenericBuffer.Destroy;
begin
  glDeleteBuffers(1, @FBuffer);
  FreeMem(FData);
  inherited Destroy;
end;

function TGLGenericBuffer.DataPtr: Pointer;
begin
  Result := FData;
end;

procedure TGLGenericBuffer.DoExpandedBy(const AOldCapacity,
  ANewCapacity: Integer);
begin

end;

procedure TGLGenericBuffer.Expand;
var
  OldSize, NewSize, OldCapacity, I: Integer;
begin
  OldCapacity := FCapacity;
  OldSize := OldCapacity * FDataItemSize;
  if FCapacity > 0 then
    FCapacity *= 2
  else
    FCapacity := 128;
  NewSize := FCapacity * FDataItemSize;

  ReAllocMem(FData, NewSize);

  glBindBuffer(FBufferKind, FBuffer);
  glBufferData(FBufferKind, NewSize, nil, FBufferPurpose);
  glBufferSubData(FBufferKind, 0, OldSize, FData);
  DoExpandedBy(OldCapacity, FCapacity);
end;

function TGLGenericBuffer.NeedsFlush: Boolean;
begin
  Result := True;
end;

procedure TGLGenericBuffer.RangeCheck(Index: Integer);
begin
  if (Index < 0) or (Index >= FCapacity) then
    raise EGLGenericBufferError.CreateFmt('Index (%d) out of bounds (0..%d).', [Index, FCapacity-1]);
end;

procedure TGLGenericBuffer.BindForRendering;
begin
  if NeedsFlush then
    Flush;
  glBindBuffer(FBufferKind, FBuffer);
end;

procedure TGLGenericBuffer.Flush;
begin
  ForceFlush;
end;

procedure TGLGenericBuffer.FlushRange(const MinItem, Count: Integer);
begin
  RangeCheck(MinItem);
  RangeCheck(MinItem + (Count - 1));
  glBindBuffer(FBufferKind, FBuffer);
  //glBufferSubData(FBufferKind, MinItem * FDataItemSize, (MinItem + Count) * FDataItemSize, FData + (MinItem * FDataItemSize));
  glBufferSubData(FBufferKind, MinItem * FDataItemSize, Count * FDataItemSize, FData + (MinItem * FDataItemSize));
end;

procedure TGLGenericBuffer.ForceFlush;
begin
  glBindBuffer(FBufferKind, FBuffer);
  glBufferSubData(FBufferKind, 0, FCapacity * FDataItemSize, FData);
end;

procedure TGLGenericBuffer.ReadBack;
begin
  FillByte(FData^, FCapacity * FDataItemSize, 0);
  glBindBuffer(FBufferKind, FBuffer);
  glGetBufferSubData(FBufferKind, 0, FCapacity * FDataItemSize, FData);
end;

procedure TGLGenericBuffer.UnbindForRendering;
begin
  glBindBuffer(FBufferKind, 0);
end;

{ TGLGeometryBuffer }

constructor TGLGeometryBuffer.Create(const AVertexSize: Integer;
  const APurpose: TGLenum);
begin
  inherited;
  FDirtyVertices := TVertexIndexList.Create;
  FFreeVertices := TVertexIndexList.Create;
end;

destructor TGLGeometryBuffer.Destroy;
begin
  FFreeVertices.Free;
  FDirtyVertices.Free;
  inherited Destroy;
end;

procedure TGLGeometryBuffer.DoExpandedBy(const AOldCapacity,
  ANewCapacity: Integer);
var
  I: Integer;
begin
  FFreeVertices.Capacity := FFreeVertices.Count + (ANewCapacity - AOldCapacity) + 32;
  for I := ANewCapacity - 1 downto AOldCapacity do
    FFreeVertices.Add(I);
  // Automatically flushed
  FDirtyVertices.Clear;
end;

class function TGLGeometryBuffer.GetBufferKind: TGLenum;
begin
  Result := GL_ARRAY_BUFFER;
end;

function TGLGeometryBuffer.GetVec1(Vertex: Cardinal; Offset: Integer): Single;
begin
  RangeCheck(Vertex);
  Result := PSingle(FData + Vertex * FDataItemSize + Offset)^;
end;

function TGLGeometryBuffer.GetVec2(Vertex: Cardinal; Offset: Integer): TVector2f;
begin
  RangeCheck(Vertex);
  Result := PVector2f(FData + Vertex * FDataItemSize + Offset)^;
end;

function TGLGeometryBuffer.GetVec3(Vertex: Cardinal; Offset: Integer): TVector3f;
begin
  RangeCheck(Vertex);
  Result := PVector3f(FData + Vertex * FDataItemSize + Offset)^;
end;

function TGLGeometryBuffer.GetVec4(Vertex: Cardinal; Offset: Integer): TVector4f;
begin
  RangeCheck(Vertex);
  Result := PVector4f(FData + Vertex * FDataItemSize + Offset)^;
end;

function TGLGeometryBuffer.NeedsFlush: Boolean;
begin
  Result := FDirtyVertices.Count > 0;
end;

procedure TGLGeometryBuffer.SetVec1(Vertex: Cardinal; Offset: Integer; Value: Single);
begin
  RangeCheck(Vertex);
  FDirtyVertices.Add(Vertex);
  PSingle(FData + Vertex * FDataItemSize + Offset)^ := Value;
end;

procedure TGLGeometryBuffer.SetVec2(Vertex: Cardinal; Offset: Integer; Value: TVector2f);
begin
  RangeCheck(Vertex);
  FDirtyVertices.Add(Vertex);
  PVector2f(FData + Vertex * FDataItemSize + Offset)^ := Value;
end;

procedure TGLGeometryBuffer.SetVec3(Vertex: Cardinal; Offset: Integer; Value: TVector3f);
begin
  RangeCheck(Vertex);
  FDirtyVertices.Add(Vertex);
  PVector3f(FData + Vertex * FDataItemSize + Offset)^ := Value;
end;

procedure TGLGeometryBuffer.SetVec4(Vertex: Cardinal; Offset: Integer; Value: TVector4f);
begin
  RangeCheck(Vertex);
  FDirtyVertices.Add(Vertex);
  PVector4f(FData + Vertex * FDataItemSize + Offset)^ := Value;
end;

function TGLGeometryBuffer.AllocateVertices(ACount: Integer): TVertexIndicies;
var
  I, FVMax: Integer;
begin
  while FFreeVertices.Count < ACount do
    Expand;
  FVMax := FFreeVertices.Count - 1;
  SetLength(Result, ACount);
  for I := 0 to High(Result) do
  begin
    Result[I] := FFreeVertices[FVMax];
    FFreeVertices.Delete(FVMax);
    Dec(FVMax);
  end;
end;

procedure TGLGeometryBuffer.DeleteVertices(AVertices: TVertexIndicies);
var
  I: Integer;
begin
  for I := 0 to High(AVertices) do
    FFreeVertices.Add(I);
end;

procedure TGLGeometryBuffer.Flush;
var
  I: Integer;
  AData: Pointer;
  Curr, Min, Max: Integer;
begin
  if FDirtyVertices.Count = 0 then
    Exit;
  Max := -1;
  Min := FCapacity + 1;
  for I := 0 to FDirtyVertices.Count - 1 do
  begin
    Curr := FDirtyVertices[I];
    if Curr > Max then
      Max := Curr;
    if Curr < Min then
      Min := Curr;
  end;
  inherited FlushRange(Min, (Max - Min)+1);
  FDirtyVertices.Clear;
end;

{ TGLGeometryBufferMap }

constructor TGLGeometryBufferMap.Create(const AParent: TGLGeometryBufferMap);
begin
  FParent := AParent;
end;

function TGLGeometryBufferMap.MapVertex(Vertex: Cardinal): Cardinal;
begin
  if FParent <> nil then
    Result := FParent.MapVertex(Vertex)
  else
    Exit(Vertex);
end;

{ TGLGeometryBufferDirectMap }

constructor TGLGeometryBufferDirectMap.Create(
  const AParent: TGLGeometryBufferMap; const Indicies: TVertexIndicies);
begin
  inherited Create(AParent);
  FIndicies := Indicies;
end;

function TGLGeometryBufferDirectMap.MapVertex(Vertex: Cardinal): Cardinal;
begin
  Exit(inherited MapVertex(FIndicies[Vertex]));
end;

{ TGLGeometryBufferOffsetMap }

function TGLGeometryBufferOffsetMap.MapVertex(Vertex: Cardinal): Cardinal;
begin
  Exit(inherited MapVertex(Vertex + FOffset));
end;

{ TGLGeometryFormat }

constructor TGLGeometryFormat.Create(const ABuffer: TGLGeometryBuffer);
begin
  FNeededVertexSize := GetNeededVertexSize;
  FBuffer := ABuffer;
  if FBuffer.FDataItemSize <> FNeededVertexSize then
    raise EGLGeometryBufferFormatError.CreateFmt('This format needs %d bytes vertex size, buffer gives %d.', [FNeededVertexSize, FBuffer.FDataItemSize]);
  FMap := nil;
end;

function TGLGeometryFormat.GetVec1(Vertex: Cardinal; Offset: Integer
  ): Single;
begin
  Exit(FBuffer.GetVec1(MapVertex(Vertex), Offset));
end;

function TGLGeometryFormat.GetVec2(Vertex: Cardinal; Offset: Integer
  ): TVector2f;
begin
  Exit(FBuffer.GetVec2(MapVertex(Vertex), Offset));
end;

function TGLGeometryFormat.GetVec3(Vertex: Cardinal; Offset: Integer
  ): TVector3f;
begin
  Exit(FBuffer.GetVec3(MapVertex(Vertex), Offset));
end;

function TGLGeometryFormat.GetVec4(Vertex: Cardinal; Offset: Integer
  ): TVector4f;
begin
  Exit(FBuffer.GetVec4(MapVertex(Vertex), Offset));
end;

function TGLGeometryFormat.MapVertex(Vertex: Cardinal): Cardinal;
begin
  if FMap = nil then
    Exit(Vertex)
  else
    Exit(FMap.MapVertex(Vertex));
end;

procedure TGLGeometryFormat.SetVec1(Vertex: Cardinal; Offset: Integer;
  Value: Single);
begin
  FBuffer.SetVec1(MapVertex(Vertex), Offset, Value);
end;

procedure TGLGeometryFormat.SetVec2(Vertex: Cardinal; Offset: Integer;
  Value: TVector2f);
begin
  FBuffer.SetVec2(MapVertex(Vertex), Offset, Value);
end;

procedure TGLGeometryFormat.SetVec3(Vertex: Cardinal; Offset: Integer;
  Value: TVector3f);
begin
  FBuffer.SetVec3(MapVertex(Vertex), Offset, Value);
end;

procedure TGLGeometryFormat.SetVec4(Vertex: Cardinal; Offset: Integer;
  Value: TVector4f);
begin
  FBuffer.SetVec4(MapVertex(Vertex), Offset, Value);
end;

procedure TGLGeometryFormat.BindGLPointer;
begin

end;

class function TGLGeometryFormat.GetNeededVertexSize: Integer;
var
  M: Integer;
begin
  Result := GetActualVertexSize;
  M := Result mod 32;
  if M > 0 then
    Result += M;
end;

procedure TGLGeometryFormat.UnbindGLPointer;
begin

end;

procedure TGLGeometryFormat.UseMap(const AMap: TGLGeometryBufferMap);
begin
  FMap := AMap;
end;

{ TGLGeometryFormatP4 }

class function TGLGeometryFormatP4.GetActualVertexSize: Integer;
begin
  Result := SizeOf(Single) * 4;
end;

procedure TGLGeometryFormatP4.BindGLPointer;
begin
  inherited;
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(4, GL_FLOAT, FNeededVertexSize, nil);
end;

procedure TGLGeometryFormatP4.UnbindGLPointer;
begin
  glDisableClientState(GL_VERTEX_ARRAY);
  inherited;
end;

{ TGLGeometryFormatP4C4 }

class function TGLGeometryFormatP4C4.GetActualVertexSize: Integer;
begin
  Result := SizeOf(Single) * 8;
end;

procedure TGLGeometryFormatP4C4.BindGLPointer;
begin
  inherited BindGLPointer;
  glEnableClientState(GL_COLOR_ARRAY);
  glColorPointer(4, GL_FLOAT, FNeededVertexSize, Pointer(ptrint(SizeOf(Single)*4)));
end;

procedure TGLGeometryFormatP4C4.UnbindGLPointer;
begin
  glDisableClientState(GL_COLOR_ARRAY);
  inherited UnbindGLPointer;
end;

{ TGLGeometryFormatP4C4T2 }

class function TGLGeometryFormatP4C4T2.GetActualVertexSize: Integer;
begin
  Result := SizeOf(Single) * 10;
end;

procedure TGLGeometryFormatP4C4T2.BindGLPointer;
begin
  inherited BindGLPointer;
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glTexCoordPointer(2, GL_FLOAT, FNeededVertexSize, Pointer(ptrint(SizeOf(Single)*8)));
end;

procedure TGLGeometryFormatP4C4T2.UnbindGLPointer;
begin
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  inherited UnbindGLPointer;
end;

{ TGLGeometryFormatP4C4T2N3F }

class function TGLGeometryFormatP4C4T2N3F.GetActualVertexSize: Integer;
begin
  Result := SizeOf(Single) * 13;
end;

procedure TGLGeometryFormatP4C4T2N3F.BindGLPointer;
begin
  inherited;
  glEnableClientState(GL_NORMAL_ARRAY);
  glNormalPointer(GL_FLOAT, FNeededVertexSize, Pointer(ptrint(SizeOf(Single)*10)));
end;

procedure TGLGeometryFormatP4C4T2N3F.UnbindGLPointer;
begin
  glDisableClientState(GL_NORMAL_ARRAY);
  inherited;
end;

{ TGLStreamIndexBuffer }

constructor TGLStreamIndexBuffer.Create(const APurpose: TGLenum);
begin
  inherited Create(SizeOf(TGLuint), APurpose);
  FCount := 0;
end;

class function TGLStreamIndexBuffer.GetBufferKind: TGLenum;
begin
  Result := GL_ELEMENT_ARRAY_BUFFER;
end;

function TGLStreamIndexBuffer.NeedsFlush: Boolean;
begin
  Result := True;
end;

procedure TGLStreamIndexBuffer.Add(AIndicies: TVertexIndicies);
var
  AddCount: Integer;
begin
  AddCount := Length(AIndicies);
  while FCount + AddCount > FCapacity do
    Expand;
  Move(AIndicies[0], (FData + (FCount * SizeOf(TVertexIndex)))^, AddCount * SizeOf(TVertexIndex));
  Inc(FCount, AddCount);
end;

procedure TGLStreamIndexBuffer.Clear;
begin
  FCount := 0;
end;

procedure TGLStreamIndexBuffer.Draw(mode: TGLenum);
begin
  glDrawElements(mode, FCount, GL_UNSIGNED_INT, nil);
end;

procedure TGLStreamIndexBuffer.Dump;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    Write(PGLUint(FData)[I], ' ');
  WriteLn;
end;

{ TGLIndexBuffer }

constructor TGLIndexBuffer.Create(const APurpose: TGLenum);
begin
  inherited Create(APurpose);
  FHandles := TIndexHandleList.Create;
end;

destructor TGLIndexBuffer.Destroy;
begin
  FHandles.Free;
  inherited Destroy;
end;

function TGLIndexBuffer.NeedsFlush: Boolean;
begin
  Result := False;
end;

procedure TGLIndexBuffer.UpdateHandles(AAfter: TVertexIndex; AOffset: Integer);
var
  I: Integer;
  Handle: TIndexEntryHandle;
begin
  for I := 0 to FHandles.Count - 1 do
  begin
    Handle := FHandles[I];
    if Handle^.Start > AAfter then
      Handle^.Start += AOffset;
  end;
end;

procedure TGLIndexBuffer.Add(AIndicies: TVertexIndicies);
begin
  raise EGLIndexBufferError.Create('Must add handled vertices to this buffer.');
end;

function TGLIndexBuffer.AddHandled(AIndicies: TVertexIndicies
  ): TIndexEntryHandle;
var
  AddCount: Integer;
begin
  AddCount := Length(AIndicies);
  if AddCount = 0 then
    Exit(nil);
  while FCount + AddCount >= FCapacity do
    Expand;
  Result := GetMem(SizeOf(TIndexEntry));
  FHandles.Add(Result);
  Result^.Start := FCount;
  Result^.Count := AddCount;
  Move(AIndicies[0], (FData + FCount * SizeOf(TVertexIndex))^, AddCount * SizeOf(TVertexIndex));
  Inc(FCount, AddCount);
  FlushRange(Result^.Start, Result^.Count);
end;

procedure TGLIndexBuffer.RemoveIndicies(AHandle: TIndexEntryHandle);
var
  Idx: Integer;
begin
  Idx := FHandles.IndexOf(AHandle);
  if Idx < 0 then
    raise EGLIndexBufferError.CreateFmt('Invalid handle: %16.16x.', [ptrint(AHandle)]);
  RangeCheck(AHandle^.Start);
  RangeCheck(AHandle^.Start + (AHandle^.Count-1));
  FHandles.Delete(Idx);

  Move((FData + (AHandle^.Start + AHandle^.Count) * FDataItemSize)^, (FData + (AHandle^.Start) * FDataItemSize)^, AHandle^.Count * FDataItemSize);
  Dec(FCount, AHandle^.Count);
  if FCount > 0 then
    FlushRange(AHandle^.Start, FCount - AHandle^.Start);
  UpdateHandles(AHandle^.Start, -AHandle^.Count);
  FreeMem(AHandle);
end;

{ TGLMaterial }

constructor TGLMaterial.Create;
begin
  raise EGTCoreError.CreateFmt('Cannot create an instance of ''%s'' this way.', [ClassName]);
end;

constructor TGLMaterial.Create(const AGeometryBuffer: TGLGeometryBuffer;
  const AFormat: TGLGeometryFormatClass);
begin
  inherited Create;
  FGeometryBuffer := AGeometryBuffer;
  FFormat := AFormat.Create(FGeometryBuffer);
  FStreamIndexBuffer := TGLStreamIndexBuffer.Create(GL_STREAM_DRAW);
  FStaticIndexBuffer := TGLIndexBuffer.Create(GL_DYNAMIC_DRAW);
end;

destructor TGLMaterial.Destroy;
begin
  FFormat.Free;
  FStreamIndexBuffer.Free;
  FStaticIndexBuffer.Free;
  inherited Destroy;
end;

procedure TGLMaterial.BindForRendering(const UseStream: Boolean);
begin
  FBoundAsStream := UseStream;
  FGeometryBuffer.BindForRendering;
  if UseStream then
    FStreamIndexBuffer.BindForRendering
  else
    FStaticIndexBuffer.BindForRendering;
  FFormat.BindGLPointer;
end;

procedure TGLMaterial.DumpStream;
var
  I: Integer;
  V: TVertexIndex;
begin
  for I := 0 to FStreamIndexBuffer.Count - 1 do
  begin
    V := PGLUint(FStreamIndexBuffer.FData)[I];
    Write(FormatVector(TGLGeometryFormatP4(FFormat).Position[V]), ' ');
  end;
  WriteLn;
end;

procedure TGLMaterial.Render(AsType: TGLenum);
begin
  if FBoundAsStream then
  begin
    glDrawElements(AsType, FStreamIndexBuffer.Count, GL_UNSIGNED_INT, nil);
    FStreamIndexBuffer.Clear;
  end
  else
    glDrawElements(AsType, FStaticIndexBuffer.Count, GL_UNSIGNED_INT, nil);
end;

procedure TGLMaterial.UnbindForRendering;
begin
  FFormat.UnbindGLPointer;
  // As the procedure is the same for any index buffer, it does not matter which
  // one we unbind.
  FStreamIndexBuffer.UnbindForRendering;
  FGeometryBuffer.UnbindForRendering;
end;

{ TGLGeometryObject }

constructor TGLGeometryObject.Create(const AMaterial: TGLMaterial;
  AVertexCount: Integer);
begin
  Create(AMaterial.GeometryBuffer, AMaterial.FFormat, AVertexCount, AMaterial.StaticIndexBuffer, AMaterial.StreamIndexBuffer);
end;

constructor TGLGeometryObject.Create(const ABuffer: TGLGeometryBuffer;
  const AFormat: TGLGeometryFormat; AVertexCount: Integer;
  AStaticIndexBuffer: TGLIndexBuffer; AStreamIndexBuffer: TGLStreamIndexBuffer
    );
begin
  FCount := AVertexCount;
  FFormat := AFormat;
  FGeometryBuffer := ABuffer;
  FVBIndicies := FGeometryBuffer.AllocateVertices(AVertexCount);
  FIBIndicies := GetIndexBufferIndicies(FVBIndicies, AVertexCount);
  if AStaticIndexBuffer <> nil then
    SetStaticIndexBuffer(AStaticIndexBuffer);
  FMap := CreateMap;
end;

destructor TGLGeometryObject.Destroy;
begin
  if FFormat.CurrentMap = FMap then
    FFormat.UseMap(nil);
  FMap.Free;
  SetStaticIndexBuffer(nil);
  FGeometryBuffer.DeleteVertices(FVBIndicies);
  inherited Destroy;
end;

procedure TGLGeometryObject.SetStaticIndexBuffer(const AValue: TGLIndexBuffer);
begin
  if FStaticIndexBuffer = AValue then
    Exit;
  if FStaticIndexBuffer <> nil then
    FStaticIndexBuffer.RemoveIndicies(FStaticIndexHandle);
  FStaticIndexBuffer := AValue;
  if FStaticIndexBuffer <> nil then
    FStaticIndexHandle := FStaticIndexBuffer.AddHandled(FIBIndicies);
end;

function TGLGeometryObject.CreateMap: TGLGeometryBufferMap;
begin
  Result := TGLGeometryBufferDirectMap.Create(nil, FVBIndicies);
end;

function TGLGeometryObject.GetIndexBufferIndicies(
  const AVBIndicies: TVertexIndicies; const ACount: Integer): TVertexIndicies;
begin
  if ACount = 0 then
    Exit(nil);
  if ACount <> Length(AVBIndicies) then
  begin
    SetLength(Result, ACount);
    Move(AVBIndicies[0], Result[0], ACount * SizeOf(TVertexIndex));
  end
  else
    Result := AVBIndicies;
end;

procedure TGLGeometryObject.AddToStreamBuffer(
  const AStreamBuffer: TGLStreamIndexBuffer);
begin
  AStreamBuffer.Add(FIBIndicies);
end;

procedure TGLGeometryObject.DrawDirect(mode: TGLenum);
begin
  FGeometryBuffer.BindForRendering;
  FFormat.BindGLPointer;
  glDrawElements(mode, Length(FIBIndicies), GL_UNSIGNED_INT, @FIBIndicies[0]);
  FFormat.UnbindGLPointer;
  FGeometryBuffer.UnbindForRendering;
end;

{ TGLGeometryQuadsForTris }

constructor TGLGeometryQuadsForTris.Create(const ABuffer: TGLGeometryBuffer;
  const AFormat: TGLGeometryFormat; AQuadCount: Integer;
  AStaticIndexBuffer: TGLIndexBuffer; AStreamIndexBuffer: TGLStreamIndexBuffer);
begin
  inherited Create(ABuffer, AFormat, AQuadCount * 4, AStaticIndexBuffer, AStreamIndexBuffer);
end;

function TGLGeometryQuadsForTris.GetIndexBufferIndicies(
  const AVBIndicies: TVertexIndicies; const ACount: Integer): TVertexIndicies;
var
  I, L, IV, II: Integer;
begin
  L := ACount;
  Assert(L mod 4 = 0);
  SetLength(Result, L + L div 2);

  for I := 0 to ((L-1) div 4) do
  begin
    IV := I*4;
    II := I*6;
    Result[II] := AVBIndicies[IV];
    Result[II+1] := AVBIndicies[IV+1];
    Result[II+2] := AVBIndicies[IV+2];
    Result[II+3] := AVBIndicies[IV];
    Result[II+4] := AVBIndicies[IV+2];
    Result[II+5] := AVBIndicies[IV+3];
  end;
end;

{ TGLGeometryTerrainSectionForTris }

constructor TGLGeometryTerrainSectionForTris.Create(
  const ABuffer: TGLGeometryBuffer; const AFormat: TGLGeometryFormat; AWidth,
  AHeight: Integer; AStaticIndexBuffer: TGLIndexBuffer;
  AStreamIndexBuffer: TGLStreamIndexBuffer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  inherited Create(ABuffer, AFormat, AWidth * AHeight, AStaticIndexBuffer, AStreamIndexBuffer);
end;

function TGLGeometryTerrainSectionForTris.GetIndexBufferIndicies(
  const AVBIndicies: TVertexIndicies; const ACount: Integer): TVertexIndicies;
var
  X, Y: Integer;
  I, IJ: Integer;

  function IV(X, Y: Integer): Integer; inline;
  begin
    Result := X + Y * FWidth;
  end;

begin
  SetLength(Result, (FWidth - 1) * (FHeight - 1) * 6);

  IJ := 0;
  for Y := 0 to FHeight - 2 do
  begin
    for X := 0 to FWidth - 2 do
    begin
      if ((X and 1) = 1) xor ((Y and 1) = 1) then
      begin
        Result[IJ] := AVBIndicies[IV(X+1, Y+1)];
        Result[IJ+1] := AVBIndicies[IV(X, Y+1)];
        Result[IJ+2] := AVBIndicies[IV(X, Y)];
        Result[IJ+3] := AVBIndicies[IV(X+1, Y)];
        Result[IJ+4] := AVBIndicies[IV(X+1, Y+1)];
        Result[IJ+5] := AVBIndicies[IV(X, Y)];
      end
      else
      begin
        Result[IJ] := AVBIndicies[IV(X+1, Y)];
        Result[IJ+1] := AVBIndicies[IV(X, Y+1)];
        Result[IJ+2] := AVBIndicies[IV(X, Y)];
        Result[IJ+3] := AVBIndicies[IV(X+1, Y+1)];
        Result[IJ+4] := AVBIndicies[IV(X, Y+1)];
        Result[IJ+5] := AVBIndicies[IV(X+1, Y)];
      end;
      Inc(IJ, 6);
    end;
  end;
end;

{ TGLGeometryObjectDynamic }

constructor TGLGeometryObjectDynamic.Create(const ABuffer: TGLGeometryBuffer;
  const AFormat: TGLGeometryFormat; AVertexCount: Integer;
  AStaticIndexBuffer: TGLIndexBuffer; AStreamIndexBuffer: TGLStreamIndexBuffer
    );
begin
  inherited;
  FCapacity := FCount;
end;

constructor TGLGeometryObjectDynamic.Create(const AMaterial: TGLMaterial;
  AVertexCount: Integer);
begin
  Create(AMaterial.GeometryBuffer, AMaterial.Format, AVertexCount, AMaterial.StaticIndexBuffer, AMaterial.StreamIndexBuffer);
end;

procedure TGLGeometryObjectDynamic.ExpandTo(const ANewCount: Integer);
var
  AdditionalAllocation: TVertexIndicies;
begin
  if FCount = ANewCount then
    Exit;
  if FCapacity < ANewCount then
  begin
    AdditionalAllocation := FGeometryBuffer.AllocateVertices(ANewCount - FCapacity);
    SetLength(FVBIndicies, ANewCount);
    Move(AdditionalAllocation[0], FVBIndicies[FCapacity], SizeOf(TVertexIndex) * (ANewCount - FCapacity));
    FCapacity := ANewCount;
    FMap.Free;
    FMap := CreateMap;
  end;
  FCount := ANewCount;
  FIBIndicies := GetIndexBufferIndicies(FVBIndicies, FCount);
  if FStaticIndexBuffer <> nil then
  begin
    if FStaticIndexHandle <> nil then
      FStaticIndexBuffer.RemoveIndicies(FStaticIndexHandle);
    FStaticIndexHandle := FStaticIndexBuffer.AddHandled(FIBIndicies);
  end;
end;

procedure TGLGeometryObjectDynamic.ShrinkTo(const ANewCount: Integer);
begin
  if FCount = ANewCount then
    Exit;
  FCount := ANewCount;
  FIBIndicies := GetIndexBufferIndicies(FVBIndicies, FCount);
  if FStaticIndexBuffer <> nil then
  begin
    if FStaticIndexHandle <> nil then
      FStaticIndexBuffer.RemoveIndicies(FStaticIndexHandle);
    FStaticIndexHandle := FStaticIndexBuffer.AddHandled(FIBIndicies);
  end;
end;

end.
