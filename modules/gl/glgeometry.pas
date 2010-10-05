unit GLGeometry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dglOpenGL, fgl, Geometry;

type
  TVertexIndicies = array of Integer;
  EGLGeometryBufferError = class (Exception);
  EGLMaterialError = class (Exception);

  TGLMaterialKind = (mkPoints = 1, mkLines = 2, mkTriangles = 3);
  TGLIndexBlockHandle = PInteger;


  { TGLGeometryBuffer }

  TGLGeometryBuffer = class (TObject)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FActualVertexSize: sizeint;
    FBuffer: TGLuint;
    FEditLocks: Integer;
    FEditPtr: Pointer;
    FCapacity, FCount: Integer;
    FFreeMap: array of Boolean;
  protected
    procedure Expand;
    procedure ForceEditing;
    procedure ForceNotEditing;
    class function GetVertexSize: sizeint; virtual; abstract;
    function GetVec1(Offset, Vertex: Integer): PSingle;
    function GetVec2(Offset, Vertex: Integer): PVector2f;
    function GetVec3(Offset, Vertex: Integer): PVector3f;
    function GetVec4(Offset, Vertex: Integer): PVector4f;
  public
    procedure BeginEdit;
    procedure BindForDrawing;
    procedure Clear;
    procedure DeleteVertex(AIndex: Integer);
    function Editing: Boolean;
    procedure EndEdit;
    function NewVertices(ACount: Integer): TVertexIndicies;
  end;

  { TGLGeometrySimpleBuffer }

  TGLGeometrySimpleBuffer = class (TGLGeometryBuffer)
  protected
    class function GetVertexSize: sizeint; override;
  public
    property Vector[Vertex: Integer]: PVector4f index 0 read GetVec4;
    property Color[Vertex: Integer]: PVector4f index 4*SizeOf(Single) read GetVec4;
    property TexCoord0[Vertex: Integer]: PVector2f index 8*SizeOf(Single) read GetVec2;
    property Normal[Vertex: Integer]: PVector3f index 10*SizeOf(Single) read GetVec3;
  end;

  TGLIndexBlockHandles = specialize TFPGList<TGLIndexBlockHandle>;

  { TGLIndexBuffer }

  TGLIndexBuffer = class(TObject)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FBuffer: TGLUint;
    FCapacity, FCount: Integer;
    FEditLocks: Integer;
    FEditPtr: Pointer;
    FIndexBlockHandles: TGLIndexBlockHandles;
  protected
    procedure Expand;
    procedure ForceEditing;
    procedure ForceNotEditing;
  public
    function AddIndicies(AIndicies: TVertexIndicies): TGLIndexBlockHandle;
    procedure BeginEdit;
    function Editing: Boolean;
    procedure EndEdit;
    procedure RemoveIndicies(AHandle: TGLIndexBlockHandle);
  end;

  { TGLMaterial }

  TGLMaterial = class (TObject)
  public
    constructor Create(const ABuffer: TGLGeometryBuffer;
      const AKind: TGLMaterialKind);
    destructor Destroy; override;
  private
    FBuffer: TGLGeometryBuffer;
    FKind: TGLMaterialKind;
  public
    property Buffer: TGLGeometryBuffer read FBuffer;
    property Kind: TGLMaterialKind read FKind;
  public
    procedure BindForDrawing;
    procedure BeginEdit;
    procedure EndEdit;
  end;

  TGLGeometry = class (TObject)
  public
    constructor Create(const AMaterial: TGLMaterial); virtual;
    destructor Destroy; override;
  private
    FBuffer: TGLGeometryBuffer;
    FCount: Integer;
    FMaterial: TGLMaterial;
    FIndicies: TVertexIndicies;
    function GetVertexIndex(Index: Integer): Integer;
  protected
    class function GetVertexCount: Integer; virtual; abstract;
  public
    property Count: Integer read FCount;
    property Material: TGLMaterial read FMaterial;
    property VertexIndex[Index: Integer]: Integer read GetVertexIndex; default;
  end;

  { TGLLine }

  TGLLine = class (TGLGeometry)
  protected
    class function GetVertexCount: Integer; override;
  end;

implementation

{ TGLGeometryBuffer }

constructor TGLGeometryBuffer.Create;
begin
  FBuffer := 0;
  FCapacity := 0;
  FCount := 0;
  FEditPtr := nil;
  SetLength(FFreeMap, 0);
  FActualVertexSize := GetVertexSize;
  if FActualVertexSize mod 32 > 0 then
    FActualVertexSize := FActualVertexSize + (32 - (FActualVertexSize mod 32));
end;

destructor TGLGeometryBuffer.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TGLGeometryBuffer.Expand;
var
  Data: Pointer;
  OldSize, OldCapacity: Integer;
begin
  if FBuffer = 0 then
  begin
    glGenBuffers(1, @FBuffer);
    glBindBuffer(GL_ARRAY_BUFFER, FBuffer);
    glBufferData(GL_ARRAY_BUFFER, 1024*FActualVertexSize, nil, GL_DYNAMIC_DRAW);
    FCapacity := 1024;
    FCount := 0;
    SetLength(FFreeMap, FCapacity);
    FillByte(FFreeMap[0], SizeOf(Boolean) * FCapacity, 1);
  end
  else
  begin
    OldSize := FCapacity * FActualVertexSize;
    Data := GetMem(OldSize);
    glBindBuffer(GL_ARRAY_BUFFER, FBuffer);
    glGetBufferSubData(GL_ARRAY_BUFFER, 0, OldSize, Data);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glDeleteBuffers(1, @FBuffer);
    FCapacity := FCapacity shl 2; // quadruple the buffer size
    glGenBuffers(1, @FBuffer);
    glBindBuffer(GL_ARRAY_BUFFER, FBuffer);
    glBufferData(GL_ARRAY_BUFFER, FCapacity * FActualVertexSize, nil, GL_DYNAMIC_DRAW);
    glBufferSubData(GL_ARRAY_BUFFER, 0, OldSize, Data);
    FreeMem(Data);

    OldCapacity := Length(FFreeMap);
    SetLength(FFreeMap, FCapacity);
    FillByte(FFreeMap[OldCapacity], SizeOf(Boolean) * (FCapacity - OldCapacity), 1);
  end;
end;

procedure TGLGeometryBuffer.ForceEditing;
begin
  if not Editing then
    raise EGLGeometryBufferError.Create('Buffer must be in editing mode to perform this operation.');
end;

procedure TGLGeometryBuffer.ForceNotEditing;
begin
  if Editing then
    raise EGLGeometryBufferError.Create('Buffer must not be in editing mode to perform this operation.');
end;

function TGLGeometryBuffer.GetVec1(Offset, Vertex: Integer): PSingle;
begin
  ForceEditing;
  Result := PSingle(FEditPtr + Offset + Vertex * FActualVertexSize);
end;

function TGLGeometryBuffer.GetVec2(Offset, Vertex: Integer): PVector2f;
begin
  ForceEditing;
  Result := PVector2f(FEditPtr + Offset + Vertex * FActualVertexSize);
end;

function TGLGeometryBuffer.GetVec3(Offset, Vertex: Integer): PVector3f;
begin
  ForceEditing;
  Result := PVector3f(FEditPtr + Offset + Vertex * FActualVertexSize);
end;

function TGLGeometryBuffer.GetVec4(Offset, Vertex: Integer): PVector4f;
begin
  ForceEditing;
  Result := PVector4f(FEditPtr + Offset + Vertex * FActualVertexSize);
end;

procedure TGLGeometryBuffer.BeginEdit;
begin
  if FEditLocks > 0 then
    Inc(FEditLocks)
  else
  begin
    if FBuffer = 0 then
      Expand;
    FEditLocks := 1;
    if FEditPtr <> nil then
      Exit;
    glBindBuffer(GL_ARRAY_BUFFER, FBuffer);
    FEditPtr := glMapBuffer(GL_ARRAY_BUFFER, GL_READ_WRITE);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
  end;
end;

procedure TGLGeometryBuffer.BindForDrawing;
begin
  ForceNotEditing;
  glBindBuffer(GL_ARRAY_BUFFER, FBuffer);
  if FEditPtr <> nil then
  begin
    glUnmapBuffer(GL_ARRAY_BUFFER);
    FEditPtr := nil;
  end;
end;

procedure TGLGeometryBuffer.Clear;
begin
  ForceEditing;
end;

procedure TGLGeometryBuffer.DeleteVertex(AIndex: Integer);
begin
  FFreeMap[AIndex] := True;
end;

function TGLGeometryBuffer.Editing: Boolean;
begin
  Result := FEditLocks > 0;
end;

procedure TGLGeometryBuffer.EndEdit;
begin
  ForceEditing;
  if FEditLocks > 0 then
    Dec(FEditLocks);
end;

function TGLGeometryBuffer.NewVertices(ACount: Integer): TVertexIndicies;
var
  I: Integer;
  J: Integer;
begin
  if FCount + ACount > FCapacity then
    Expand;
  SetLength(Result, ACount);
  J := 0;
  for I := 0 to High(FFreeMap) do
  begin
    if FFreeMap[I] then
    begin
      Result[J] := I;
      FFreeMap[I] := False;
      Inc(J);
      if J = ACount then
        Break;
    end;
  end;
end;

{ TGLGeometrySimpleBuffer }

class function TGLGeometrySimpleBuffer.GetVertexSize: sizeint;
begin
  Result := SizeOf(Single) * 13;
end;

{ TGLIndexBuffer }

constructor TGLIndexBuffer.Create;
begin
  FCapacity := 0;
  FBuffer := 0;
  FCount := 0;
  FEditLocks := 0;
  FEditPtr := nil;
  FIndexBlockHandles := TGLIndexBlockHandles.Create;
end;

destructor TGLIndexBuffer.Destroy;
var
  I: Integer;
begin
  if FEditLocks > 0 then
  begin
    FEditLocks := 1;
    EndEdit;
  end;
  for I := 0 to FIndexBlockHandles.Count - 1 do
    FreeMem(FIndexBlockHandles[I]);
  FIndexBlockHandles.Free;
  inherited Destroy;
end;

procedure TGLIndexBuffer.Expand;
var
  OldData: Pointer;
  OldSize: sizeint;
begin

end;

procedure TGLIndexBuffer.ForceEditing;
begin

end;

procedure TGLIndexBuffer.ForceNotEditing;
begin

end;

function TGLIndexBuffer.AddIndicies(AIndicies: TVertexIndicies
  ): TGLIndexBlockHandle;
begin

end;

procedure TGLIndexBuffer.BeginEdit;
begin

end;

function TGLIndexBuffer.Editing: Boolean;
begin
  Result := FEditLocks > 0;
end;

procedure TGLIndexBuffer.EndEdit;
begin

end;

procedure TGLIndexBuffer.RemoveIndicies(AHandle: TGLIndexBlockHandle);
begin

end;

{ TGLMaterial }

constructor TGLMaterial.Create(const ABuffer: TGLGeometryBuffer;
  const AKind: TGLMaterialKind);
begin
  if ABuffer = nil then
    raise EGLMaterialError.Create('Must be given a valid buffer.');
  FBuffer := ABuffer;
  FKind := AKind;
end;

destructor TGLMaterial.Destroy;
begin
  inherited Destroy;
end;

{ TGLGeometry }

constructor TGLGeometry.Create(const AMaterial: TGLMaterial);
begin
  FBuffer := AMaterial.FBuffer;
  FMaterial := AMaterial;
  FIndicies := FBuffer.NewVertices(GetVertexCount);
end;

destructor TGLGeometry.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FIndicies) do
    FBuffer.DeleteVertex(FIndicies[I]);
  inherited Destroy;
end;

function TGLGeometry.GetVertexIndex(Index: Integer): Integer;
begin
  Result := FIndicies[Index];
end;

{ TGLLine }

class function TGLLine.GetVertexCount: Integer;
begin
  Exit(2);
end;

end.
