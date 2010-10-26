unit coreScene;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uiGL, GLGeometry, Geometry, ioSDL, dglOpenGL, GLHelpers,
  ioConfig, sdl, TerrainGeometryShaded, TerrainSourcePerlinNoise, GLCamera,
  GTVFS, TerrainSource, GLShaderMaterial, GLShader, glBitmap;

type

  { TTT3DScene }

  TTT3DScene = class (TuiRootLayer)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FDebugMaterial: TGLMaterial;
    FDebugBuffer: TGLGeometryBuffer;

    FAxis: TGLGeometryObject;
    FGrid: TGLGeometryObject;

    FVFS: TGTVFS;

    (*FMove, FMoveSpeed, FMoveAccel: TVector3;
    FMoveTarget: TVector2;
    FMoving: Boolean;

    FRot, FRotSpeed, FRotAccel: TVector2;
    FRotateTarget: TVector2;
    FRotating: Boolean;
    FZ: TVectorFloat;*)
    FCamera: TGLCameraFreeSmooth;
    FCameraMoved: Boolean;

    FTerrain: TTerrain;

    FTerrainMaterial: TTerrainMaterial;
    FTerrainBuffer: TGLGeometryBuffer;
  protected
    procedure CameraMoved(Sender: TObject);
    procedure DoAbsMetricsChanged; override;
    procedure DoKeypress(Sym: TSDL_KeySym; Mode: TsdlKeyActionMode;
       var Handled: Boolean); override;
    procedure DoMouseButton(Button: TsdlMouseButtonEventData;
       Mode: TsdlKeyActionMode); override;
    procedure DoMouseMotion(Motion: TsdlMouseMotionEventData); override;
    procedure DoUpdate(const ATimeInterval: Double); override;
    procedure DoRenderBackground; override;
  end;

implementation

{ TTT3DScene }

constructor TTT3DScene.Create;
var
  I: Integer;
  v1, v2, o: TVector2;
  Src: TTerrainSource;
begin
  inherited Create;
  (*FZ := 0.0;
  FMoving := False;
  FRotating := False;

  FMove := Vector3(0.0, 0.0, -5.0);
  FMoveSpeed := Vector3(0.0, 0.0, 0.0);
  FMoveAccel := Vector3(0.0, 0.0, 0.0);

  FRot := Vector2(0.0, 0.0);
  FRotSpeed := Vector2(0.0, 0.0);
  FRotAccel := Vector2(0.0, 0.0);     *)

  FVFS := TGTVFS.Create;
  FVFS.AddMount(TGTMountDirectory.Create(FVFS, ExtractFilePath(ParamStr(0))), fpFileSystem);

  FCamera := TGLCameraFreeSmooth.Create;

  FDebugBuffer := TGLGeometryBuffer.Create(SizeOf(Single) * 8, GL_DYNAMIC_DRAW);
  FDebugMaterial := TGLMaterial.Create(FDebugBuffer, TGLGeometryFormatP4C4);

  FAxis := TGLGeometryObject.Create(FDebugMaterial, 6);
  with FAxis.Format as TGLGeometryFormatP4C4 do
  begin
    UseMap(FAxis.Map);

    Position[0] :=  Vector4(-0.1, 0.0, 0.0, 1.0);
    Color[0] :=     Vector4(0.1, 0.0, 0.0, 1.0);
    Position[1] :=  Vector4(1.0, 0.0, 0.0, 1.0);
    Color[1] :=     Vector4(1.0, 0.0, 0.0, 1.0);

    Position[2] :=  Vector4(0.0, -0.1, 0.0, 1.0);
    Color[2] :=     Vector4(0.0, 0.1, 0.0, 1.0);
    Position[3] :=  Vector4(0.0, 1.0, 0.0, 1.0);
    Color[3] :=     Vector4(0.0, 1.0, 0.0, 1.0);

    Position[4] :=  Vector4(0.0, 0.0, -0.1, 1.0);
    Color[4] :=     Vector4(0.0, 0.0, 0.1, 1.0);
    Position[5] :=  Vector4(0.0, 0.0, 1.0, 1.0);
    Color[5] :=     Vector4(0.0, 0.0, 1.0, 1.0);

    UseMap(nil);
  end;

  FGrid := TGLGeometryObject.Create(FDebugMaterial, 84);
  with FGrid.Format as TGLGeometryFormatP4C4 do
  begin
    UseMap(FGrid.Map);

    for I := -10 to 10 do
    begin
      Position[(I + 10)*2]    := Vector4(I, -10.0, 0.0, 1.0);
      Color[(I + 10)*2]       := Vector4(0.25, 0.25, 0.25, 1.0);
      Position[(I + 10)*2+1]  := Vector4(I, 10.0, 0.0, 1.0);
      Color[(I + 10)*2+1]     := Vector4(0.25, 0.25, 0.25, 1.0);
    end;

    for I := -10 to 10 do
    begin
      Position[(I + 31)*2]    := Vector4(-10.0, I, 0.0, 1.0);
      Color[(I + 31)*2]       := Vector4(0.25, 0.25, 0.25, 1.0);
      Position[(I + 31)*2+1]  := Vector4(10.0, I, 0.0, 1.0);
      Color[(I + 31)*2+1]     := Vector4(0.25, 0.25, 0.25, 1.0);
    end;

    UseMap(nil);
  end;

  FCamera.FOV := Config.Video.FOV;
  FCamera.NearZ := 1.0;
  FCamera.FarZ := 1000.0;
  FCamera.Viewport.Left := AbsLeft;
  FCamera.Viewport.Top := AbsTop;
  FCamera.Viewport.Right := AbsLeft + AbsWidth;
  FCamera.Viewport.Bottom := AbsTop + AbsHeight;
  FCamera.OnMoved := @CameraMoved;

  v1 := Vector2(1.0, 0.0);
  v2 := Vector2(0.0, -1.0);
  o := Vector2(1.0, 1.0);
  WriteLn(FormatVector(Intersection(v1, v2, o)));

  FTerrainBuffer := TGLGeometryBuffer.Create(TTerrainFormat.GetNeededVertexSize);
  FTerrainMaterial := TTerrainMaterial.Create(FTerrainBuffer, TTerrainFormat);
  FTerrainMaterial.ColorMap := TglBitmap2D.Create(FVFS.OpenFile('textures/terrain/temperate.png'));
  FTerrainMaterial.ColorMap.GenTexture();
  FTerrainMaterial.NormalMap := TglBitmap2D.Create(FVFS.OpenFile('textures/terrain/normalmap.png'));
  FTerrainMaterial.NormalMap.GenTexture();
  FTerrainMaterial.NormalMap.SetWrap(GL_REPEAT, GL_REPEAT);
  FTerrainMaterial.NormalMap.MipMap := mmMipmap;
  FTerrainMaterial.NormalMap.SetFilter(GL_LINEAR_MIPMAP_LINEAR, GL_LINEAR);
  glGetError;
  FTerrainMaterial.Shader.LoadShader(FVFS.OpenFile('shader/terrain.vs', fmOpenRead), FVFS.OpenFile('shader/terrain.fs', fmOpenRead));

  Src := TTerrainSourcePerlinNoise.Create(640, 640, 5297, 3215, 0.50, 9, 0.05, 0.05, 12.0, -2.0);
  FTerrain := TTerrain.Create(640, 640, Src, FTerrainMaterial);
  RaiseLastGLError;
  FTerrain.Generate;
  RaiseLastGLError;

  //FTerrain.UpdateForFrustum({Vector3(FCamera.Pos.Vec2, FCamera.Zoom)}FCamera.TransformedPos, {Max(64.0 / Max(VLength(FCamera.Velocity) / 10.0 + FCamera.ZoomVelocity / 10.0, 1.0), 2.0)} 512.0, 0.9);
end;

destructor TTT3DScene.Destroy;
begin
  FGrid.Free;
  FAxis.Free;
  FDebugMaterial.Free;
  FDebugBuffer.Free;
  inherited Destroy;
end;

procedure TTT3DScene.CameraMoved(Sender: TObject);
begin
  FCameraMoved := True;
end;

procedure TTT3DScene.DoAbsMetricsChanged;
begin
  FCamera.Viewport.SetAll(AbsTop, AbsLeft, AbsTop + AbsHeight, AbsLeft + AbsWidth);
  inherited DoAbsMetricsChanged;
end;

procedure TTT3DScene.DoKeypress(Sym: TSDL_KeySym; Mode: TsdlKeyActionMode;
  var Handled: Boolean);
begin
  case Sym.sym of
    SDLK_KP0:
    begin
      if Mode = kmPress then
      begin
        (*FMoving := True;
        FMoveTarget := Vector2(0.0, 0.0);
        FRotating := True;
        FRotateTarget := Vector2(-45.0, 45.0);*)
        FCamera.IssueMoveTo(Vector2(0.0, 0.0));
        FCamera.IssueRotateTo(Vector2(0.0, 0.0));
      end;
      Handled := True;
    end;
  end;
end;

procedure TTT3DScene.DoMouseButton(Button: TsdlMouseButtonEventData;
  Mode: TsdlKeyActionMode);
begin
  case Button.button of
    4: if Mode = kmPress then
    begin
      if SDL_GetKeyState(nil)[SDLK_LCTRL] or SDL_GetKeyState(nil)[SDLK_RCTRL] <> 0 then
        FCamera.AccelerateRotation(Vector2(-4*Pi, 0.0))
      else
        FCamera.AccelerateZoom(-50.0 * FCamera.Zoom);
    end;

    5: if Mode = kmPress then
    begin
      if SDL_GetKeyState(nil)[SDLK_LCTRL] or SDL_GetKeyState(nil)[SDLK_RCTRL] <> 0 then
        FCamera.AccelerateRotation(Vector2(4*Pi, 0.0))
      else
        FCamera.AccelerateZoom(50.0 * FCamera.Zoom);
    end;

    6: if Mode = kmPress then
    begin
      if SDL_GetKeyState(nil)[SDLK_LSHIFT] or SDL_GetKeyState(nil)[SDLK_RSHIFT] <> 0 then
        FCamera.Accelerate(FCamera.FlatRight * 10.0 * FCamera.Zoom * 2.0)
      else
        FCamera.AccelerateRotation(Vector2(0.0, -16*Pi))
    end;

    7: if Mode = kmPress then
    begin
      if SDL_GetKeyState(nil)[SDLK_LSHIFT] or SDL_GetKeyState(nil)[SDLK_RSHIFT] <> 0 then
        FCamera.Accelerate(-FCamera.FlatRight * 10.0 * FCamera.Zoom * 2.0)
      else
        FCamera.AccelerateRotation(Vector2(0.0, 16*pi))
    end;
  end;
end;

procedure TTT3DScene.DoMouseMotion(Motion: TsdlMouseMotionEventData);
var
  Factor: TVectorFloat;
begin
  if Motion.state and SDL_BUTTON(3) <> 0 then
  begin
    FCamera.AccelerateRotation(Vector2(Motion.yrel, Motion.xrel) * Pi);
    FCamera.StopRotation(False);
  end
  else if (Motion.state and SDL_BUTTON(2) <> 0) or ((Motion.state and SDL_BUTTON(1) <> 0) and (Motion.state and SDL_BUTTON(3) <> 0)) then
  begin
    Factor := FCamera.Zoom * 2.0;
    FCamera.Accelerate((FCamera.FlatRight * Motion.xrel + FCamera.FlatFront * Motion.yrel) * Factor);
    FCamera.StopMovement(False);
  end;
end;

procedure TTT3DScene.DoUpdate(const ATimeInterval: Double);
var
  X, Y: Integer;
  XF, YF, Z1, Z2: TVectorFloat;
  V: TVector2;
begin
  FCameraMoved := False;
  FCamera.Update(ATimeInterval);
  V := FCamera.Pos.Vec2;

  if V.X < 0 then
    V.X := 0;
  if V.X > FTerrain.Width - 1 then
    V.X := FTerrain.Width - 1;
  if V.Y < 0 then
    V.Y := 0;
  if V.Y > FTerrain.Height - 1 then
    V.Y := FTerrain.Height - 1;
  X := Trunc(V.X);
  XF := V.X - X;
  Y := Trunc(V.Y);
  YF := V.Y - Y;

  Z1 := (FTerrain.Heightfield[X, Y] * (1-XF) + FTerrain.Heightfield[X+1, Y] * (XF));
  Z2 := (FTerrain.Heightfield[X, Y+1] * (1-XF) + FTerrain.Heightfield[X+1, Y+1] * (XF));

  FCamera.Pos := Vector3(V, Z1 * (1-YF) + Z2 * (YF));
  if FCameraMoved then
  begin
//    FTerrain.UpdateForFrustum({Vector3(FCamera.Pos.Vec2, FCamera.Zoom)}FCamera.TransformedPos, {Max(64.0 / Max(VLength(FCamera.Velocity) / 10.0 + FCamera.ZoomVelocity / 10.0, 1.0), 2.0)} 64.0, 0.5);
    //WriteLn(FormatVector(IntersectionPlane(FCamera.TransformedPos, FCamera.Front, Vector3(0.0, 0.0, 1.0))));
  end;
  //WriteLn(FormatVector(FCamera.TransformedPos));

  DoUpdateBackgroundGeometry;
end;

procedure TTT3DScene.DoRenderBackground;
(*var
  I: Integer;
  V: TVector4f;*)
const
  mat_specular   : Array[0..3] of GlFloat = (0.0, 0.0, 0.0, 0.0);
  mat_shininess  : Array[0..0] of GlFloat = (0.0);
  mat_ambient    : Array[0..3] of GlFloat = (0.15, 0.15, 0.15, 1.0);
  mat_diffuse    : Array[0..3] of GlFloat = (0.8, 0.8, 0.8, 1.0);

  P: TVector4f = (-0.75, 0.35, 0.75, 0.0);
  Ambient: TVector4f = (0.15, 0.20, 0.25, 1.0);
  Diffuse: TVector4f = (0.85, 0.80, 0.75, 1.0);
var
  Pos: TVector4f;
begin
  //SetupPerspective(AbsLeft, AbsWidth, AbsTop, AbsHeight, 1.0, 1000.0, Config.Video.FOV);
  glClear(GL_DEPTH_BUFFER_BIT);
  glDisable(GL_SCISSOR_TEST);
  glEnable(GL_DEPTH_TEST);
  glDisable(GL_BLEND);
  glEnable(GL_CULL_FACE);

  (*glTranslatef(0.0, 0.0, FMove.Z);
  glRotatef(FRot.X, 1.0, 0.0, 0.0);
  glRotatef(FRot.Y, 0.0, 0.0, 1.0);
  glTranslatef(FMove.X, FMove.Y, FZ);      *)
  glLoadIdentity;
  Pos := Normalize(P);
  glLightfv(GL_LIGHT0, GL_POSITION, @Pos[0]);
  glLightfv(GL_LIGHT0, GL_AMBIENT, @Ambient[0]);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @Diffuse[0]);
  FCamera.Load;

  FDebugMaterial.BindForRendering(False);
  FDebugMaterial.Render(GL_LINES);
  FDebugMaterial.UnbindForRendering;

  //glPointSize(2.0);
  //FTerrain.DrawDirect;

  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR,  @mat_specular[0]);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, @mat_shininess[0]);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT,   @mat_ambient[0]);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE,   @mat_diffuse[0]);

  //glEnable(GL_LIGHTING);
  //glEnable(GL_LIGHT0);
  //glEnable(GL_COLOR_MATERIAL);
  //glEnable(GL_NORMALIZE);


  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  FTerrain.Draw(FCamera.Pos, FCamera.TransformedPos, FCamera.Front);
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  glColor4f(1, 1, 1, 1);
  glDisable(GL_LIGHTING);
  //FTerrainMaterial.UnbindForRendering;

  (*glColor4f(1, 1, 1, 1);
  glBegin(GL_TRIANGLES);
    glVertex2f(-1.0, -1.0);
    glVertex2f(-1.0, 1.0);
    glVertex2f(1.0, -1.0);

    with FTerrain.Geometry.Format as TGLGeometryFormatP4C4 do
    begin
      UseMap(FTerrain.Geometry.Map);

      for I := 0 to FTerrain.Geometry.Count - 1 do
      begin
        V := Color[I];
        glColor4fv(@V[0]);
        V := Position[I];
        glVertex4fv(@V[0]);
        WriteLn(FormatVector(V));
      end;

      UseMap(nil);
    end;
  glEnd;
    Halt(0);       *)

  SetupOrthoDefault(0, Config.Video.Width, 0, Config.Video.Height);
  glClear(GL_DEPTH_BUFFER_BIT);
  glDisable(GL_DEPTH_TEST);
  glEnable(GL_SCISSOR_TEST);
  glDisable(GL_CULL_FACE);

  (*glBegin(GL_QUADS);
    glVertex2f(0, 0, 0, 0);
    glVertex2f(0, 0, 128, 128);
    glVertex2f(0, 0, 128, 128);
    glVertex2f(0, 0, 128, 128);
  glEnd;*)

  glEnable(GL_BLEND);

  DoRenderBackgroundGeometry;
end;

end.

