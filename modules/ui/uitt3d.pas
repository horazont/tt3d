unit uiTT3D;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uiGL, GLGeometry, Geometry, dglOpenGL, GTBase, XMLGeometry,
  GeometryColors, math;

type

  { TuiTT3DTheme }

  TuiTT3DTheme = class (TGTBaseObject)
  public
    constructor Create; override;
  private
    FColors: array of TVector4;
    function GetColor(AIndex: integer): TVector4;
    procedure SetColor(AIndex: Integer; const AValue: TVector4);
  public
    property ActiveColor: TVector4 index 1 read GetColor write SetColor;
    property BaseColor: TVector4 index 0 read GetColor write SetColor;
  end;

  { TuiTT3DSurface }

  TuiTT3DSurface = class (TuiSurface)
  public
    destructor Destroy; override;
  private
    FTheme: TuiTT3DTheme;
  protected
    function ForceTheme: TuiTT3DTheme;
    procedure LinkTheme(const ATheme: TuiTT3DTheme);
    procedure SetTheme(const ATheme: TuiTT3DTheme);
    procedure ThemeChanged(Sender: TObject);
    procedure ThemeDeleting(Sender: TObject);
    procedure UnlinkTheme(const ATheme: TuiTT3DTheme);
  public
    property Theme: TuiTT3DTheme read FTheme write SetTheme;
  end;

  { TuiTT3DTabSurface }

  TuiTT3DTabSurface = class (TuiTT3DSurface)
  private
    FShear: Double;
    FState: TuiWidgetState;
    procedure SetShear(const AValue: Double);
    procedure SetState(const AValue: TuiWidgetState);
  public
    procedure UpdateGeometry(const ARect: TuiRect;
       const ABuffer: TGLGeometryBuffer;
       const AFormat: TGLGeometryFormatP4C4T2; var Geometry: TGLGeometryObject
       ); override;
  public
    property Shear: Double read FShear write SetShear;
    property State: TuiWidgetState read FState write SetState;
  end;

  { TuiTT3DRootTabWidget }

  TuiTT3DRootTabWidget = class (TuiWidget)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FBackgroundGeometry: TGLGeometryObject;
    FBackgroundSurface: TuiSurfaceReference;
    FTabGeometry: TGLGeometryObject;
    FTabSurfaces: array [TuiWidgetState] of TuiTT3DTabSurface;
    function GetTabSurface(State: TuiWidgetState): TuiTT3DTabSurface;
  protected
    function DoHitTest(const APoint: TPoint): TuiWidget; override;
    procedure DoUpdate(const ATimeInterval: Double); override;
    procedure DoRenderBackground; override;
    procedure SurfaceChanged(Sender: TObject);
  public
    property TabSurface[State: TuiWidgetState]: TuiTT3DTabSurface read GetTabSurface;
  published
    property BackgroundSurface: TuiSurfaceReference read FBackgroundSurface;
  end;

implementation

{ TuiTT3DTheme }

constructor TuiTT3DTheme.Create;
begin
  inherited Create;
  SetLength(FColors, 2);
end;

function TuiTT3DTheme.GetColor(AIndex: integer): TVector4;
begin
  Result := FColors[AIndex];
end;

procedure TuiTT3DTheme.SetColor(AIndex: Integer; const AValue: TVector4);
begin
  if FColors[AIndex] = AValue then
    Exit;
  FColors[AIndex] := AValue;
  DoChange;
end;

{ TuiTT3DSurface }

destructor TuiTT3DSurface.Destroy;
begin
  SetTheme(nil);
  inherited Destroy;
end;

function TuiTT3DSurface.ForceTheme: TuiTT3DTheme;
begin
  Result := FTheme;
  if FTheme = nil then
    raise Exception.Create('Need a theme for this.');
end;

procedure TuiTT3DSurface.LinkTheme(const ATheme: TuiTT3DTheme);
begin
  ATheme.OnDestruction.RegisterHandler(@ThemeDeleting);
  ATheme.OnChange.RegisterHandler(@ThemeChanged);
end;

procedure TuiTT3DSurface.SetTheme(const ATheme: TuiTT3DTheme);
begin
  if FTheme = ATheme then
    Exit;
  if FTheme <> nil then
    UnlinkTheme(FTheme);
  FTheme := ATheme;
  if FTheme <> nil then
  begin
    LinkTheme(FTheme);
    DoChange;
  end;
end;

procedure TuiTT3DSurface.ThemeChanged(Sender: TObject);
begin
  DoChange;
end;

procedure TuiTT3DSurface.ThemeDeleting(Sender: TObject);
begin
  SetTheme(nil);
end;

procedure TuiTT3DSurface.UnlinkTheme(const ATheme: TuiTT3DTheme);
begin
  ATheme.OnDestruction.UnRegisterHandler(@ThemeDeleting);
  ATheme.OnChange.UnRegisterHandler(@ThemeChanged);
end;

{ TuiTT3DTabSurface }

procedure TuiTT3DTabSurface.SetState(const AValue: TuiWidgetState);
begin
  if FState = AValue then
    Exit;
  FState := AValue;
  DoChange;
end;

procedure TuiTT3DTabSurface.SetShear(const AValue: Double);
begin
  if FShear = AValue then
    Exit;
  FShear := AValue;
  DoChange;
end;

procedure TuiTT3DTabSurface.UpdateGeometry(const ARect: TuiRect;
  const ABuffer: TGLGeometryBuffer; const AFormat: TGLGeometryFormatP4C4T2;
  var Geometry: TGLGeometryObject);
var
  ATheme: TuiTT3DTheme;
  cBase, cTabBorder, cTabTop, cTabBottom: TVector4;
  vTL, vTR, vBL, vBR: TVector4;
begin
  ATheme := ForceTheme;
  if Geometry = nil then
    Geometry := TGLGeometryQuadsForTris.Create(ABuffer, AFormat, 4);

  case FState of
    wsNormal: cBase := ATheme.BaseColor;
    wsHover, wsPressing: cBase := (ATheme.ActiveColor + ATheme.BaseColor) / 2.0;
    wsDown: cBase := ATheme.ActiveColor;
  end;

  cTabBorder := ATheme.BaseColor;
  cTabBorder.W := 1.0;
  cTabBorder.Y := 1.0;
  cTabBorder := HSVToRGB(cTabBorder);

  cTabTop := ATheme.BaseColor;
  cTabTop.W := 1.0;
  cTabTop.Z := Min(1.0, cTabTop.Z + 0.5);
  cTabTop := HSVToRGB(cTabTop);

  cTabBottom := ATheme.BaseColor;
  cTabBottom.W := 1.0;
  cTabBottom := HSVToRGB(cTabBottom);

  vTL := Vector4(ARect.X, ARect.Y, 0, 1);
  vTR := Vector4(vTL.X + ARect.W, vTL.Y, 0, 1);
  vBL := Vector4(vTL.X + FShear, vTL.Y + ARect.H, 0, 1);
  vBR := Vector4(vTR.X + FShear, vBL.Y, 0, 1);

  with AFormat do
  begin
    UseMap(Geometry.Map);

    Position[0] := Vector4(vTL.X, vTL.Y + 1.0, 0, 1);
    Color[0] := cTabBorder;
    Position[1] := vBL;
    Color[1] := cTabBorder;
    Position[2] := Vector4(vBL.X + 1.0, vBL.Y, 0, 1);
    Color[2] := cTabBorder;
    Position[3] := Vector4(vTL.X + 1.0, vTL.Y + 1.0, 0, 1);
    Color[3] := cTabBorder;

    Position[4] := vTL;
    Color[4] := cTabBorder;
    Position[5] := Vector4(vTL.X, vTL.Y + 1.0, 0, 1);
    Color[5] := cTabBorder;
    Position[6] := Vector4(vTR.X, vTR.Y + 1.0, 0, 1);
    Color[6] := cTabBorder;
    Position[7] := vTR;
    Color[7] := cTabBorder;

    Position[8] := Vector4(vTR.X - 1.0, vTR.Y + 1.0, 0, 1);
    Color[8] := cTabBorder;
    Position[9] := Vector4(vBR.X - 1.0, vBR.Y, 0, 1);
    Color[9] := cTabBorder;
    Position[10] := vBR;
    Color[10] := cTabBorder;
    Position[11] := Vector4(vTR.X, vTR.Y + 1.0, 0, 1);
    Color[11] := cTabBorder;

    Position[12] := Vector4(vTL.X + 1.0, vTL.Y + 1.0, 0, 1);
    Color[12] := cTabTop;
    Position[13] := Vector4(vBL.X + 1.0, vBL.Y, 0, 1);
    Color[13] := cTabBottom;
    Position[14] := Vector4(vBR.X - 1.0, vBR.Y, 0, 1);
    Color[14] := cTabBottom;
    Position[15] := Vector4(vTR.X - 1.0, vTR.Y + 1.0, 0, 1);
    Color[15] := cTabTop;

    UseMap(nil);
  end;
end;

{ TuiTT3DRootTabWidget }

constructor TuiTT3DRootTabWidget.Create;
var
  S: TuiWidgetState;
begin
  inherited Create;
  for S := Low(TuiWidgetState) to High(TuiWidgetState) do
  begin
    FTabSurfaces[S] := TuiTT3DTabSurface.Create;
    FTabSurfaces[S].OnChange.RegisterHandler(@SurfaceChanged);
  end;
  FBackgroundSurface := TuiSurfaceReference.Create;
end;

destructor TuiTT3DRootTabWidget.Destroy;
var
  S: TuiWidgetState;
begin
  for S := Low(TuiWidgetState) to High(TuiWidgetState) do
    FTabSurfaces[S].Free;
  FBackgroundSurface.Free;
  FreeAndNil(FBackgroundGeometry);
  FreeAndNil(FTabGeometry);
  inherited Destroy;
end;

function TuiTT3DRootTabWidget.GetTabSurface(State: TuiWidgetState
  ): TuiTT3DTabSurface;
begin
  Result := FTabSurfaces[State];
end;

function TuiTT3DRootTabWidget.DoHitTest(const APoint: TPoint): TuiWidget;
var
  Client: TPoint;
begin
  Result := inherited;
  if Result = Self then
  begin
    Client.X := APoint.X - AbsLeft;
    Client.Y := APoint.Y - AbsTop;
    if (Client.X > 27) and (Client.Y > 16) and (Client.X <= AbsWidth-2) and (Client.Y <= AbsHeight-2) then
      Exit(nil);
  end;
end;

procedure TuiTT3DRootTabWidget.DoUpdate(const ATimeInterval: Double);
begin
  FTabSurfaces[wsNormal].UpdateGeometry(uiRect(AbsLeft + 12, AbsTop + 1, 256, 16), Materials.Buffer, Materials.Format, FTabGeometry);
  if FBackgroundSurface.Obj <> nil then
    FBackgroundSurface.Obj.UpdateGeometry(uiRect(AbsLeft + 27, AbsTop + 16, AbsWidth - 27, AbsHeight - 16), Materials.Buffer, Materials.Format, FBackgroundGeometry);
end;

procedure TuiTT3DRootTabWidget.DoRenderBackground;
begin
  if FBackgroundGeometry <> nil then
    FBackgroundGeometry.DrawDirect(GL_TRIANGLES);
  if FTabGeometry <> nil then
    FTabGeometry.DrawDirect(GL_TRIANGLES);
end;

procedure TuiTT3DRootTabWidget.SurfaceChanged(Sender: TObject);
begin
  if Sender is TuiTT3DTabSurface then
    FreeAndNil(FTabGeometry);
  Invalidate;
end;

end.

