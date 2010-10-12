unit uiGL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GLGeometry, GLMaterials, GTBase, fgl, xml, Geometry,
  XMLGeometry, dglOpenGL, ioSDL, sdl;

type
  EuiError = class (Exception);

  TuiQuadVertex = (qvTopLeft, qvTopRight, qvBottomLeft, qvBottomRight);
  TuiSurface3x3Position = (poTopLeft, poTopCenter, poTopRight, poMiddleLeft, poCenter, poMiddleRight, poBottomLeft, poBottomCenter, poBottomRight);

  TuiRender = procedure (Sender: TObject) of object;
  TuiUpdate = procedure (Sender: TObject; const ATimeInterval: Double) of object;
  TuiMouseMotion = procedure (Sender: TObject; Motion: TsdlMouseMotionEventData) of object;
  TuiMouseButton = procedure (Sender: TObject; Button: TsdlMouseButtonEventData; Mode: TsdlKeyActionMode) of object;
  TuiKeypress = procedure (Sender: TObject; Sym: TSDL_KeySym; Mode: TsdlKeyActionMode; var Handled: Boolean) of object;

  TuiWidgetState = (wsNormal, wsHover, wsPressing, wsDown);

  TuiRect = record
    X, Y, W, H: Integer;
  end;

  { TuiMaterials }

  TuiMaterials = class (TObject)
  public
    constructor Create(ABufferToUse: TGLGeometryBuffer = nil; ABufferOwned: Boolean = False);
    destructor Destroy; override;
  private
    FBuffer: TGLGeometryBuffer;
    FBufferOwned: Boolean;
    FFormat: TGLGeometryFormatP4C4T2;
  public
    property Buffer: TGLGeometryBuffer read FBuffer;
    property BufferOwned: Boolean read FBufferOwned write FBufferOwned;
    property Format: TGLGeometryFormatP4C4T2 read FFormat;
  public
    procedure BindGeometry;
    procedure UnbindGeometry;
  end;

  { TuiQuadVertexInfo }

  TuiQuadVertexInfo = class (TGTBaseObject)
  private
    FColor: TVector4;
    FTexCoord0: TVector2;
  public
    procedure LoadFromXML(const XMLNode: TxmlNode; Context: IGTLoaderContext =
       nil); override;
    procedure SaveToXML(const XMLNode: TxmlNode); override;
  public
    property Color: TVector4 read FColor write FColor;
    property TexCoord0: TVector2 read FTexCoord0 write FTexCoord0;
  end;

  { TuiQuadInfo }

  TuiQuadInfo = class (TGTBaseObject)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FDummyC: TVector4;
    FDummyTC: TVector2;
    FVertices: array [TuiQuadVertex] of TuiQuadVertexInfo;
    function GetVertex(I: TuiQuadVertex): TuiQuadVertexInfo;
    procedure SetColor(const AValue: TVector4);
    procedure SetTexCoord(const AValue: TVector2);
  public
    property Color: TVector4 read FDummyC write SetColor;
    property TexCoord0: TVector2 read FDummyTC write SetTexCoord;
    property Vertex[I: TuiQuadVertex]: TuiQuadVertexInfo read GetVertex;
  public
    procedure SetupQuad(const AMappedFormat: TGLGeometryFormatP4C4T2);
  published
    property TopLeft: TuiQuadVertexInfo index qvTopLeft read GetVertex;
    property TopRight: TuiQuadVertexInfo index qvTopRight read GetVertex;
    property BottomLeft: TuiQuadVertexInfo index qvBottomLeft read GetVertex;
    property BottomRight: TuiQuadVertexInfo index qvBottomRight read GetVertex;
  end;

  TuiWidget = class;

  TuiWidgets = specialize TFPGList<TuiWidget>;

  TuiSurface = class;

  { TuiSurface }

  TuiSurface = class (TGTMultiRefObject)
  public
    procedure UpdateGeometry(const ARect: TuiRect;
      const ABuffer: TGLGeometryBuffer; const AFormat: TGLGeometryFormatP4C4T2;
      var Geometry: TGLGeometryObject); virtual; abstract;
  end;
  TuiSurfaceReference = specialize TGTReference<TuiSurface>;

  { TuiSurface3x3 }

  TuiSurface3x3 = class (TuiSurface)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FQuadInfos: array [TuiSurface3x3Position] of TuiQuadInfo;
    FTopMargin: TVectorFloat;
    FRightMargin: TVectorFloat;
    FLeftMargin: TVectorFloat;
    FBottomMargin: TVectorFloat;
    function GetQuadInfo(I: TuiSurface3x3Position): TuiQuadInfo;
  public
    property QuadInfo[I: TuiSurface3x3Position]: TuiQuadInfo read GetQuadInfo;
  public
    procedure UpdateGeometry(const ARect: TuiRect;
      const ABuffer: TGLGeometryBuffer; const AFormat: TGLGeometryFormatP4C4T2;
      var Geometry: TGLGeometryObject); override;
  published
    property TopLeft: TuiQuadInfo index poTopLeft read GetQuadInfo;
    property TopCenter: TuiQuadInfo index poTopCenter read GetQuadInfo;
    property TopRight: TuiQuadInfo index poTopRight read GetQuadInfo;
    property MiddleLeft: TuiQuadInfo index poMiddleLeft read GetQuadInfo;
    property Center: TuiQuadInfo index poCenter read GetQuadInfo;
    property MiddleRight: TuiQuadInfo index poMiddleRight read GetQuadInfo;
    property BottomLeft: TuiQuadInfo index poBottomLeft read GetQuadInfo;
    property BottomCenter: TuiQuadInfo index poBottomCenter read GetQuadInfo;
    property BottomRight: TuiQuadInfo index poBottomRight read GetQuadInfo;

    property TopMargin: TVectorFloat read FTopMargin write FTopMargin;
    property BottomMargin: TVectorFloat read FBottomMargin write FBottomMargin;
    property RightMargin: TVectorFloat read FRightMargin write FRightMargin;
    property LeftMargin: TVectorFloat read FLeftMargin write FLeftMargin;
  end;

  { TuiWidget }

  TuiWidget = class (TGTBaseObject)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FAbsHeight: Integer;
    FAbsLeft: Integer;
    FAbsTop: Integer;
    FAbsWidth: Integer;
    FChildren: TuiWidgets;
    FEnabled: Boolean;
    FFlex: Word;
    FFlexSum: Integer;
    FHeight: Integer;
    FInvalidated: Boolean;
    FMaterials: TuiMaterials;
    FName: String;
    FOnKeypress: TuiKeypress;
    FOnMouseButton: TuiMouseButton;
    FOnMouseMotion: TuiMouseMotion;
    FParent: TuiWidget;
    FRelLeft: Integer;
    FRelTop: Integer;
    FVisible: Boolean;
    FWidth: Integer;
    function GetChild(Index: Integer): TuiWidget;
    function GetCount: Integer;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetFlex(const AValue: Word);
    procedure SetHeight(const AValue: Integer);
    procedure SetMaterials(const AValue: TuiMaterials);
    procedure SetParent(const AValue: TuiWidget);
    procedure SetVisible(const AValue: Boolean);
    procedure SetWidth(const AValue: Integer);
  protected
    FAnimated: Boolean;
  protected
    procedure AddChild(const AChild: TuiWidget); virtual;
    procedure DoAbsMetricsChanged; virtual;
    function DoAcceptChild(const AChild: TuiWidget): Boolean; virtual;
    function DoAcceptFocus: Boolean; virtual;
    procedure DoAlign; virtual;
    function DoHitTest(const APoint: TPoint): TuiWidget; virtual;
    procedure DoKeypress(Sym: TSDL_KeySym; Mode: TsdlKeyActionMode; var Handled: Boolean); virtual;
    procedure DoMaterialChanged; virtual;
    procedure DoMouseButton(Button: TsdlMouseButtonEventData; Mode: TsdlKeyActionMode); virtual;
    procedure DoMouseMotion(Motion: TsdlMouseMotionEventData); virtual;
    procedure DoRelMetricsChanged; virtual;
    procedure DoRenderBackground; virtual;
    procedure DoRenderChildren;
    procedure DoRenderForeground; virtual;
    procedure DoUpdate(const ATimeInterval: Double); virtual;
    procedure LinkParent(const AParent: TuiWidget); virtual;
    procedure RemoveChild(const AChild: TuiWidget); virtual;
    procedure SetAbsMetrics(const ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetRelMetrics(const ALeft, ATop: Integer);
    procedure UnlinkParent(const AParent: TuiWidget); virtual;
  public
    property AbsHeight: Integer read FAbsHeight;
    property AbsLeft: Integer read FAbsLeft;
    property AbsTop: Integer read FAbsTop;
    property AbsWidth: Integer read FAbsWidth;
    property Child[Index: Integer]: TuiWidget read GetChild; default;
    property Count: Integer read GetCount;
    property Materials: TuiMaterials read FMaterials write SetMaterials;
    property Parent: TuiWidget read FParent write SetParent;
    property RelLeft: Integer read FRelLeft;
    property RelTop: Integer read FRelTop;
  public
    function ClientToAbsolute(const APoint: TPoint): TPoint;
    function ClientToParent(const APoint: TPoint): TPoint;
    procedure DeleteChildren;
    function GetUIRect: TuiRect;
    procedure Invalidate;
    procedure LoadFromXML(const XMLNode: TxmlNode; Context: IGTLoaderContext =
       nil); override;
    function ParentToClient(const APoint: TPoint): TPoint;
    procedure SaveToXML(const XMLNode: TxmlNode); override;
    procedure Render;
    procedure Update(const ATimeInterval: Double); virtual;
  published
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Flex: Word read FFlex write SetFlex;
    property Height: Integer read FHeight write SetHeight;
    property Name: String read FName write FName;
    property OnKeypress: TuiKeypress read FOnKeypress write FOnKeypress;
    property OnMouseButton: TuiMouseButton read FOnMouseButton write FOnMouseButton;
    property OnMouseMotion: TuiMouseMotion read FOnMouseMotion write FOnMouseMotion;
    property Visible: Boolean read FVisible write SetVisible;
    property Width: Integer read FWidth write SetWidth;
  end;

  { TuiBox }

  TuiBox = class (TuiWidget)
  private
    FSpacing: Word;
    procedure SetSpacing(const AValue: Word);
  public
    property Spacing: Word read FSpacing write SetSpacing;
  end;

  { TuiHBox }

  TuiHBox = class (TuiBox)
  protected
    procedure DoAlign; override;
  end;

  { TuiVBox }

  TuiVBox = class (TuiBox)
  protected
    procedure DoAlign; override;
  end;

  { TuiButton }

  TuiButton = class (TuiWidget)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FGeometry: TGLGeometryObject;
    FSurface: TuiSurfaceReference;
  protected
    function DoAcceptChild(const AChild: TuiWidget): Boolean; override;
    function DoAcceptFocus: Boolean; override;
    procedure DoRenderBackground; override;
  public
    procedure Update;
  published
    property Surface: TuiSurfaceReference read FSurface;
  end;

  { TuiRootLayer }

  TuiRootLayer = class (TuiWidget)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FBackground: TuiSurfaceReference;
    FBackgroundGeometry: TGLGeometryObject;
    FFocused: TuiWidget;
    FMouseGrabber: TuiWidget;
    FMouseGrabButton: UInt8;
    FOnRender: TuiRender;
    FOnUpdate: TuiUpdate;
  protected
    function DoAcceptChild(const AChild: TuiWidget): Boolean; override;
    procedure DoAlign; override;
    procedure DoRenderBackground; override;
    procedure DoRenderBackgroundGeometry;
    procedure DoUpdate(const ATimeInterval: Double); override;
    procedure DoUpdateBackgroundGeometry;
  public
    property Background: TuiSurfaceReference read FBackground;
    property Focused: TuiWidget read FFocused;
  public
    procedure DeliverKeypress(Sym: TSDL_KeySym; Mode: TsdlKeyActionMode);
    procedure DeliverMouseButton(Button: TsdlMouseButtonEventData; Mode: TsdlKeyActionMode);
    procedure DeliverMouseMotion(Motion: TsdlMouseMotionEventData);
    procedure SetAbsMetrics(const ALeft, ATop, AWidth, AHeight: Integer);
  published
    property OnRender: TuiRender read FOnRender write FOnRender;
    property OnUpdate: TuiUpdate read FOnUpdate write FOnUpdate;
  end;

function uiRect(X, Y, W, H: Integer): TuiRect; inline;

implementation

function uiRect(X, Y, W, H: Integer): TuiRect; inline;
begin
  Result.X := X;
  Result.Y := Y;
  Result.W := W;
  Result.H := H;
end;

{ TuiMaterials }

constructor TuiMaterials.Create(ABufferToUse: TGLGeometryBuffer;
  ABufferOwned: Boolean);
begin
  if ABufferToUse <> nil then
  begin
    FBuffer := ABufferToUse;
    FBufferOwned := ABufferOwned;
  end
  else
  begin
    FBuffer := TGLGeometryBuffer.Create(TGLGeometryFormatP4C4T2.GetNeededVertexSize, GL_DYNAMIC_DRAW);
    FBufferOwned := True;
  end;
  FFormat := TGLGeometryFormatP4C4T2.Create(FBuffer);
end;

destructor TuiMaterials.Destroy;
begin
  FFormat.Free;
  if FBufferOwned then
    FBuffer.Free;
  inherited Destroy;
end;

procedure TuiMaterials.BindGeometry;
begin
  FBuffer.BindForRendering;
  FFormat.BindGLPointer;
end;

procedure TuiMaterials.UnbindGeometry;
begin
  FFormat.UnbindGLPointer;
  FBuffer.UnbindForRendering;
end;

{ TuiQuadVertexInfo }

procedure TuiQuadVertexInfo.LoadFromXML(const XMLNode: TxmlNode;
  Context: IGTLoaderContext);
begin
  FColor := LoadVector4(XMLNode.Node['Color'], Vector4(1.0, 1.0, 1.0, 1.0));
  FTexCoord0 := LoadVector2(XMLNode.Node['TexCoord0'], Vector2(0.0, 0.0));
end;

procedure TuiQuadVertexInfo.SaveToXML(const XMLNode: TxmlNode);
begin
  SaveVector(XMLNode.AddNode('Color'), FColor);
  SaveVector(XMLNode.AddNode('TexCoord0'), FTexCoord0);
end;

{ TuiQuadInfo }

constructor TuiQuadInfo.Create;
var
  I: TuiQuadVertex;
begin
  inherited Create;
  for I := Low(TuiQuadVertex) to High(TuiQuadVertex) do
    FVertices[I] := TuiQuadVertexInfo.Create;
end;

destructor TuiQuadInfo.Destroy;
var
  I: TuiQuadVertex;
begin
  for I := Low(TuiQuadVertex) to High(TuiQuadVertex) do
    FVertices[I].Free;
  inherited Destroy;
end;

function TuiQuadInfo.GetVertex(I: TuiQuadVertex): TuiQuadVertexInfo;
begin
  Result := FVertices[I];
end;

procedure TuiQuadInfo.SetColor(const AValue: TVector4);
var
  I: TuiQuadVertex;
begin
  if FDummyC = AValue then exit;
  FDummyC := AValue;
  for I := Low(TuiQuadVertex) to High(TuiQuadVertex) do
    FVertices[I].FColor := AValue;
end;

procedure TuiQuadInfo.SetTexCoord(const AValue: TVector2);
var
  I: TuiQuadVertex;
begin
  if FDummyTC = AValue then exit;
  FDummyTC := AValue;
  for I := Low(TuiQuadVertex) to High(TuiQuadVertex) do
    FVertices[I].FTexCoord0 := AValue;
end;

procedure TuiQuadInfo.SetupQuad(const AMappedFormat: TGLGeometryFormatP4C4T2);
begin
  with AMappedFormat do
  begin
    Color[0] := FVertices[qvTopLeft].Color;
    TexCoord0[0] := FVertices[qvTopLeft].TexCoord0;
    Color[1] := FVertices[qvBottomLeft].Color;
    TexCoord0[1] := FVertices[qvBottomLeft].TexCoord0;
    Color[2] := FVertices[qvBottomRight].Color;
    TexCoord0[2] := FVertices[qvBottomRight].TexCoord0;
    Color[3] := FVertices[qvTopRight].Color;
    TexCoord0[3] := FVertices[qvTopRight].TexCoord0;
  end;
end;

{ TuiSurface3x3 }

constructor TuiSurface3x3.Create;
var
  I: TuiSurface3x3Position;
begin
  inherited Create;
  for I := Low(TuiSurface3x3Position) to HigH(TuiSurface3x3Position) do
    FQuadInfos[I] := TuiQuadInfo.Create;
end;

destructor TuiSurface3x3.Destroy;
var
  I: TuiSurface3x3Position;
begin
  for I := Low(TuiSurface3x3Position) to HigH(TuiSurface3x3Position) do
    FQuadInfos[I].Free;
  inherited Destroy;
end;

function TuiSurface3x3.GetQuadInfo(I: TuiSurface3x3Position): TuiQuadInfo;
begin
  Result := FQuadInfos[I];
end;

procedure TuiSurface3x3.UpdateGeometry(const ARect: TuiRect;
  const ABuffer: TGLGeometryBuffer; const AFormat: TGLGeometryFormatP4C4T2;
  var Geometry: TGLGeometryObject);
var
  I: TuiSurface3x3Position;
  Map: TGLGeometryBufferOffsetMap;
  Format: TGLGeometryFormatP4C4T2;
  vTopLeft: TVector2;
  vInnerTL, vInnerTR, vInnerBL, vInnerBR: TVector4;
begin
  if (Geometry = nil) then
    Geometry := TGLGeometryQuadsForTris.Create(ABuffer, AFormat, 9);
  vTopLeft := Vector2(ARect.X, ARect.Y);
  Format := Geometry.Format as TGLGeometryFormatP4C4T2;
  Map := TGLGeometryBufferOffsetMap.Create(Geometry.Map);
  try
    vInnerTL := Vector4(vTopLeft + Vector2(FLeftMargin, FTopMargin), 0.0, 1.0);
    vInnerTR := Vector4(vInnerTL.Vec2 + Vector2(ARect.W - (FLeftMargin + FRightMargin), 0.0), 0.0, 1.0);
    vInnerBL := Vector4(vInnerTL.Vec2 + Vector2(0.0, ARect.H - (FTopMargin + FBottomMargin)), 0.0, 1.0);
    vInnerBR := Vector4(vInnerTR.X, vInnerBL.Y, 0.0, 1.0);
    with Format do
    begin
      UseMap(Map);

      // Top left
      Map.Offset := 0;
      Position[0] := Vector4(vTopLeft, 0, 1);
      Position[1] := Vector4(vTopLeft.X, vInnerTL.Y, 0, 1);
      Position[2] := vInnerTL;
      Position[3] := Vector4(vInnerTL.X, vTopLeft.Y, 0, 1);
      FQuadInfos[poTopLeft].SetupQuad(Format);

      // Top center
      Map.Offset := 4;
      Position[0] := Vector4(vInnerTL.X, vTopLeft.Y, 0, 1);
      Position[1] := vInnerTL;
      Position[2] := vInnerTR;
      Position[3] := Vector4(vInnerTR.X, vTopLeft.Y, 0, 1);
      FQuadInfos[poTopCenter].SetupQuad(Format);

      // Top right
      Map.Offset := 8;
      Position[0] := Vector4(vInnerTR.X, vTopLeft.Y, 0, 1);
      Position[1] := vInnerTR;
      Position[2] := Vector4(vInnerTR.X + FRightMargin, vInnerTR.Y, 0, 1);
      Position[3] := Vector4(vInnerTR.X + FRightMargin, vTopLeft.Y, 0, 1);
      FQuadInfos[poTopRight].SetupQuad(Format);

      // Middle right
      Map.Offset := 12;
      Position[0] := vInnerTR;
      Position[1] := vInnerBR;
      Position[2] := Vector4(vInnerTR.X + FRightMargin, vInnerBR.Y, 0, 1);
      Position[3] := Vector4(vInnerTR.X + FRightMargin, vTopLeft.Y, 0, 1);
      FQuadInfos[poMiddleRight].SetupQuad(Format);

      // Center
      Map.Offset := 16;
      Position[0] := vInnerTL;
      Position[1] := vInnerBL;
      Position[2] := vInnerBR;
      Position[3] := vInnerTR;
      FQuadInfos[poCenter].SetupQuad(Format);

      // Middle left
      Map.Offset := 20;
      Position[0] := Vector4(vTopLeft.X, vInnerTL.Y, 0, 1);
      Position[1] := Vector4(vTopLeft.X, vInnerBL.Y, 0, 1);
      Position[2] := vInnerBL;
      Position[3] := vInnerTL;
      FQuadInfos[poMiddleLeft].SetupQuad(Format);

      // Bottom left
      Map.Offset := 24;
      Position[0] := Vector4(vTopLeft.X, vInnerBL.Y, 0, 1);
      Position[1] := Vector4(vTopLeft.X, vInnerBL.Y + FBottomMargin, 0, 1);
      Position[2] := Vector4(vInnerBL.X, vInnerBL.Y + FBottomMargin, 0, 1);
      Position[3] := Vector4(vInnerBL.X, vInnerBL.Y, 0, 1);
      FQuadInfos[poBottomLeft].SetupQuad(Format);

      // Bottom center
      Map.Offset := 28;
      Position[0] := vInnerBL;
      Position[1] := Vector4(vInnerBL.X, vInnerBL.Y + FBottomMargin, 0, 1);
      Position[2] := Vector4(vInnerBR.X, vInnerBR.Y + FBottomMargin, 0, 1);
      Position[3] := vInnerBR;
      FQuadInfos[poBottomCenter].SetupQuad(Format);

      // Bottom right
      Map.Offset := 32;
      Position[0] := vInnerBR;
      Position[1] := Vector4(vInnerBR.X, vInnerBR.Y + FBottomMargin, 0, 1);
      Position[2] := Vector4(vInnerBR.X + FRightMargin, vInnerBR.Y + FBottomMargin, 0, 1);
      Position[3] := Vector4(vInnerBR.X + FRightMargin, vInnerBR.Y, 0, 1);
      FQuadInfos[poBottomRight].SetupQuad(Format);
    end;
  finally
    Format.UseMap(nil);
    Map.Free;
  end
end;

{ TuiWidget }

constructor TuiWidget.Create;
begin
  inherited Create;
  FChildren := TuiWidgets.Create;
  FMaterials := nil;
  FParent := nil;
  FName := '';
  FInvalidated := True;
  FFlex := 1;
end;

destructor TuiWidget.Destroy;
begin
  DeleteChildren;
  FChildren.Free;
  SetMaterials(nil);
  inherited Destroy;
end;

function TuiWidget.GetChild(Index: Integer): TuiWidget;
begin
  Result := FChildren[Index];
end;

function TuiWidget.GetCount: Integer;
begin
  Result := FChildren.Count;
end;

procedure TuiWidget.SetEnabled(const AValue: Boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
  Invalidate;
end;

procedure TuiWidget.SetFlex(const AValue: Word);
begin
  if AValue < 1 then
  begin
    SetFlex(1);
    Exit;
  end;
  if FFlex = AValue then
    Exit;
  if FParent <> nil then
  begin
    FParent.FFlexSum -= FFlex;
    FParent.FFlexSum += AValue;
    FFlex := AValue;
    FParent.DoAlign;
  end
  else
    FFlex := AValue;
end;

procedure TuiWidget.SetHeight(const AValue: Integer);
begin
  if FHeight = AValue then
    Exit;
  FHeight := AValue;
  if FParent <> nil then
    FParent.DoAlign;
end;

procedure TuiWidget.SetMaterials(const AValue: TuiMaterials);
var
  I: Integer;
begin
  if FMaterials = AValue then
    Exit;
  for I := 0 to FChildren.Count - 1 do
    FChildren[I].Materials := AValue;
  FMaterials := AValue;
  DoMaterialChanged;
end;

procedure TuiWidget.SetParent(const AValue: TuiWidget);
begin
  if FParent = AValue then
    Exit;
  if FParent <> nil then
    UnlinkParent(FParent);
  FParent := AValue;
  if FParent <> nil then
    LinkParent(FParent);
end;

procedure TuiWidget.SetVisible(const AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  Invalidate;
end;

procedure TuiWidget.SetWidth(const AValue: Integer);
begin
  if FWidth = AValue then
    Exit;
  FWidth := AValue;
  if FParent <> nil then
    FParent.DoAlign;
end;

procedure TuiWidget.AddChild(const AChild: TuiWidget);
begin
  if AChild = nil then
    raise EuiError.CreateFmt('Cannot make nil a child of ''%s''.', [AChild.ClassName, ClassName]);
  if not DoAcceptChild(AChild) then
    raise EuiError.CreateFmt('Cannot make ''%s'' a child of ''%s''.', [AChild.ClassName, ClassName]);
  FChildren.Add(AChild);
  FFlexSum += AChild.FFlex;
  DoAlign;
end;

procedure TuiWidget.DoAbsMetricsChanged;
begin
  Invalidate;
  DoAlign;
end;

function TuiWidget.DoAcceptChild(const AChild: TuiWidget): Boolean;
begin
  Result := True;
end;

function TuiWidget.DoAcceptFocus: Boolean;
begin
  Result := False;
end;

procedure TuiWidget.DoAlign;
begin

end;

function TuiWidget.DoHitTest(const APoint: TPoint): TuiWidget;
var
  I: Integer;
  Hit: TuiWidget;
begin
  if (APoint.X < FAbsLeft) or (APoint.Y < FAbsTop) or (APoint.X >= FAbsWidth) or (APoint.Y >= FAbsHeight) then
    Exit(nil);

  for I := 0 to FChildren.Count - 1 do
  begin
    Hit := FChildren[I].DoHitTest(APoint);
    if Hit <> nil then
      Exit(Hit);
  end;
  Exit(Self);
end;

procedure TuiWidget.DoKeypress(Sym: TSDL_KeySym; Mode: TsdlKeyActionMode;
  var Handled: Boolean);
begin
  if Assigned(FOnKeypress) then
    FOnKeypress(Self, Sym, Mode, Handled);
end;

procedure TuiWidget.DoMaterialChanged;
begin
  Invalidate;
end;

procedure TuiWidget.DoMouseButton(Button: TsdlMouseButtonEventData;
  Mode: TsdlKeyActionMode);
begin
  if Assigned(FOnMouseButton) then
    FOnMouseButton(Self, Button, Mode);
end;

procedure TuiWidget.DoMouseMotion(Motion: TsdlMouseMotionEventData);
begin
  if Assigned(FOnMouseMotion) then
    FOnMouseMotion(Self, Motion);
end;

procedure TuiWidget.DoRelMetricsChanged;
begin

end;

procedure TuiWidget.DoRenderBackground;
begin

end;

procedure TuiWidget.DoRenderChildren;
var
  I: Integer;
begin
  for I := 0 to FChildren.Count - 1 do
    FChildren[I].Render;
end;

procedure TuiWidget.DoRenderForeground;
begin

end;

procedure TuiWidget.DoUpdate(const ATimeInterval: Double);
begin

end;

procedure TuiWidget.LinkParent(const AParent: TuiWidget);
begin
  AParent.AddChild(Self);
  Materials := AParent.Materials;
end;

procedure TuiWidget.RemoveChild(const AChild: TuiWidget);
begin
  FFlexSum -= AChild.FFlex;
  FChildren.Remove(AChild);
end;

procedure TuiWidget.SetAbsMetrics(const ALeft, ATop, AWidth, AHeight: Integer);
begin
  if (FAbsLeft = ALeft) and (FAbsTop = ATop) and (FAbsWidth = AWidth) and (FAbsHeight = AHeight) then
    Exit;
  FAbsLeft := ALeft;
  FAbsTop := ATop;
  FAbsWidth := AWidth;
  FAbsHeight := AHeight;
  DoAbsMetricsChanged;
end;

procedure TuiWidget.SetRelMetrics(const ALeft, ATop: Integer);
begin
  if (FRelLeft = ALeft) and (FRelTop = ATop) then
    Exit;
  FRelLeft := ALeft;
  FRelTop := ATop;
  DoRelMetricsChanged;
end;

procedure TuiWidget.UnlinkParent(const AParent: TuiWidget);
begin
  AParent.RemoveChild(Self);
end;

function TuiWidget.ClientToAbsolute(const APoint: TPoint): TPoint;
begin
  Result.X := APoint.X + FAbsLeft;
  Result.Y := APoint.Y + FAbsTop;
end;

function TuiWidget.ClientToParent(const APoint: TPoint): TPoint;
begin
  Result := APoint;
  Result.X += FRelLeft;
  Result.Y += FRelTop;
end;

procedure TuiWidget.DeleteChildren;
var
  I: Integer;
begin
  for I := 0 to FChildren.Count - 1 do
    FChildren[I].Free;
  FChildren.Clear;
end;

function TuiWidget.GetUIRect: TuiRect;
begin
  Result := uiRect(FAbsLeft, FAbsTop, FAbsWidth, FAbsHeight);
end;

procedure TuiWidget.Invalidate;
begin
  FInvalidated := True;
end;

procedure TuiWidget.LoadFromXML(const XMLNode: TxmlNode;
  Context: IGTLoaderContext);
var
  ChildrenNode: TxmlNode;
  ChildNodes: TxmlNodeArray;
  ChildNode: TxmlNode;
  I: Integer;
  AChild: TuiWidget;
  AClass: TGTBaseClass;
begin
  inherited LoadFromXML(XMLNode, Context);
  ChildrenNode := XMLNode.Node['Children'];
  if ChildrenNode <> nil then
  begin
    ChildNodes := ChildrenNode.Nodes['Child'];
    for I := 0 to High(ChildNodes) do
    begin
      ChildNode := ChildNodes[I];
      AClass := FindGTClass(ChildNode.Attribute['class']);
      if AClass = nil then
        raise EGTXMLInvalidProperty.CreateFmt('Unknown class: ''%s''.', [ChildNode.Attribute['class']]);
      if not AClass.InheritsFrom(TuiWidget) then
        Continue;
      AChild := TuiWidget(AClass.Create);
      AChild.Materials := FMaterials;
      AChild.LoadFromXML(ChildNode, Context);
      AChild.Parent := Self;
    end;
  end;
end;

function TuiWidget.ParentToClient(const APoint: TPoint): TPoint;
begin
  Result.X := APoint.X - FRelLeft;
  Result.Y := APoint.Y - FRelTop;
end;

procedure TuiWidget.SaveToXML(const XMLNode: TxmlNode);
var
  ChildrenNode: TxmlNode;
  I: Integer;
begin
  inherited SaveToXML(XMLNode);
  ChildrenNode := XMLNode.AddNode('Children');
  for I := 0 to FChildren.Count - 1 do
    FChildren[I].SaveToXML(ChildrenNode.AddNode('Child'));
end;

procedure TuiWidget.Render;
begin
  DoRenderBackground;
  DoRenderChildren;
  DoRenderForeground;
end;

procedure TuiWidget.Update(const ATimeInterval: Double);
var
  I: Integer;
begin
  if FAnimated then
    DoUpdate(ATimeInterval)
  else if FInvalidated then
    DoUpdate(0.0);
  FInvalidated := False;
  for I := 0 to FChildren.Count - 1 do
    FChildren[I].Update(ATimeInterval);
end;

{ TuiBox }

procedure TuiBox.SetSpacing(const AValue: Word);
begin
  if FSpacing = AValue then
    Exit;
  FSpacing := AValue;
  DoAlign;
end;

{ TuiHBox }

procedure TuiHBox.DoAlign;
var
  I, X, W, H: Integer;
  AChild: TuiWidget;
  FreeSpace: Integer;
  SpacePerFlex: Double;
begin
  X := FAbsTop;
  if FChildren.Count = 0 then
    Exit;
  FreeSpace := FAbsWidth - (FChildren.Count - 1) * FSpacing;
  SpacePerFlex := FreeSpace / FFlexSum;
  for I := 0 to FChildren.Count - 1 do
  begin
    AChild := FChildren[I];
    W := Round(SpacePerFlex * AChild.Flex);
    H := AChild.Height;
    if (H > FHeight) or (H <= 0) then
      H := FHeight;
    AChild.SetAbsMetrics(X, FAbsTop, W, H);
    X += W + FSpacing;
  end;
end;

{ TuiVBox }

procedure TuiVBox.DoAlign;
var
  I, Y, W, H: Integer;
  AChild: TuiWidget;
  FreeSpace: Integer;
  SpacePerFlex: Double;
begin
  Y := FAbsTop;
  if FChildren.Count = 0 then
    Exit;
  FreeSpace := FAbsHeight - (FChildren.Count - 1) * FSpacing;
  SpacePerFlex := FreeSpace / FFlexSum;
  for I := 0 to FChildren.Count - 1 do
  begin
    AChild := FChildren[I];
    H := Round(SpacePerFlex * AChild.Flex);
    W := AChild.Width;
    if (W > FWidth) or (W <= 0) then
      W := FWidth;
    AChild.SetAbsMetrics(FAbsLeft, Y, W, H);
    Y += H + FSpacing;
  end;
end;

{ TuiButton }

constructor TuiButton.Create;
begin
  inherited Create;
  FSurface := TuiSurfaceReference.Create;
end;

destructor TuiButton.Destroy;
begin
  FGeometry.Free;
  FSurface.Free;
  inherited Destroy;
end;

function TuiButton.DoAcceptChild(const AChild: TuiWidget): Boolean;
begin
  Result := False;
end;

function TuiButton.DoAcceptFocus: Boolean;
begin
  Result := True;
end;

procedure TuiButton.DoRenderBackground;
begin
  if FGeometry <> nil then
    FGeometry.DrawDirect(GL_TRIANGLES);
end;

procedure TuiButton.Update;
begin
  if (FSurface.Obj <> nil) and (FMaterials <> nil) then
    FSurface.Obj.UpdateGeometry(uiRect(FAbsLeft, FAbsTop, FAbsWidth, FAbsHeight), FMaterials.Buffer, FMaterials.Format, FGeometry);
end;

{ TuiRootLayer }

constructor TuiRootLayer.Create;
begin
  inherited Create;
  FAnimated := True;
  FBackground := TuiSurfaceReference.Create;
end;

destructor TuiRootLayer.Destroy;
begin
  FreeAndNil(FBackgroundGeometry);
  FBackground.Free;
  inherited Destroy;
end;

function TuiRootLayer.DoAcceptChild(const AChild: TuiWidget): Boolean;
begin
  Result := True;
end;

procedure TuiRootLayer.DoAlign;
var
  I: Integer;
begin
  for I := 0 to FChildren.Count - 1 do
    FChildren[I].SetAbsMetrics(FAbsLeft, FAbsTop, FAbsWidth, FAbsHeight);
end;

procedure TuiRootLayer.DoRenderBackground;
begin
  DoRenderBackgroundGeometry;
  if Assigned(FOnRender) then
    FOnRender(Self);
end;

procedure TuiRootLayer.DoRenderBackgroundGeometry;
begin
  if FBackgroundGeometry <> nil then
    FBackgroundGeometry.DrawDirect(GL_TRIANGLES);
end;

procedure TuiRootLayer.DoUpdate(const ATimeInterval: Double);
begin
  DoUpdateBackgroundGeometry;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self, ATimeInterval);
end;

procedure TuiRootLayer.DoUpdateBackgroundGeometry;
begin
  if FBackground.Obj <> nil then
    FBackground.Obj.UpdateGeometry(GetUIRect, FMaterials.Buffer, FMaterials.Format, FBackgroundGeometry);
end;

procedure TuiRootLayer.DeliverKeypress(Sym: TSDL_KeySym; Mode: TsdlKeyActionMode
  );
var
  Handled: Boolean;
begin
  Handled := False;
  if FFocused <> nil then
    FFocused.DoKeypress(Sym, Mode, Handled);
  if not Handled then
    DoKeypress(Sym, Mode, Handled);
end;

procedure TuiRootLayer.DeliverMouseButton(Button: TsdlMouseButtonEventData;
  Mode: TsdlKeyActionMode);
var
  Hit: TuiWidget;
begin
  if FMouseGrabber <> nil then
  begin
    if (FMouseGrabButton = Button.button) and (Mode = kmRelease) then
      FMouseGrabber := nil
    else
    begin
      FMouseGrabber.DoMouseButton(Button, Mode);
      Exit;
    end;
  end;
  Hit := DoHitTest(Point(Button.x, Button.y));
  if Button.button = 1 then
    FFocused := Hit;
  if Hit <> nil then
  begin
    if (FMouseGrabber = nil) and (Mode = kmPress) then
    begin
      FMouseGrabber := Hit;
      FMouseGrabButton := Button.button;
    end;
    Button.x -= Hit.AbsLeft;
    Button.y -= Hit.AbsTop;
    Hit.DoMouseButton(Button, Mode);
  end;
end;

procedure TuiRootLayer.DeliverMouseMotion(Motion: TsdlMouseMotionEventData);
var
  Hit: TuiWidget;
  P: TPoint;
begin
  if FMouseGrabber <> nil then
  begin
    Motion.x -= FMouseGrabber.AbsLeft;
    Motion.y -= FMouseGrabber.AbsTop;
    FMouseGrabber.DoMouseMotion(Motion);
  end
  else
  begin
    P := Point(Motion.x, Motion.y);
    Hit := DoHitTest(P);
    if Hit <> nil then
    begin
      Motion.x -= Hit.AbsLeft;
      Motion.y -= Hit.AbsTop;
      Hit.DoMouseMotion(Motion);
    end;
  end;
end;

procedure TuiRootLayer.SetAbsMetrics(const ALeft, ATop, AWidth, AHeight: Integer
  );
begin
  inherited;
end;

initialization
RegisterGTClass(TuiButton, 'uiButton');

end.

