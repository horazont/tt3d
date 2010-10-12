unit coreScene;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uiGL, GLGeometry, Geometry, ioSDL, dglOpenGL, GLHelpers,
  ioConfig, sdl, TerrainGeometry;

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

    FMove, FMoveSpeed, FMoveAccel: TVector3;
    FMoveTarget: TVector2;
    FMoving: Boolean;

    FRot, FRotSpeed, FRotAccel: TVector2;
    FRotateTarget: TVector2;
    FRotating: Boolean;
    FZ: TVectorFloat;

    FTerrain: TTerrain;
  protected
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
begin
  inherited Create;
  FZ := 0.0;
  FMoving := False;
  FRotating := False;

  FMove := Vector3(0.0, 0.0, -5.0);
  FMoveSpeed := Vector3(0.0, 0.0, 0.0);
  FMoveAccel := Vector3(0.0, 0.0, 0.0);

  FRot := Vector2(0.0, 0.0);
  FRotSpeed := Vector2(0.0, 0.0);
  FRotAccel := Vector2(0.0, 0.0);

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

  FTerrain := TTerrain.Create(128, 128);
end;

destructor TTT3DScene.Destroy;
begin
  FTerrain.Free;
  FGrid.Free;
  FAxis.Free;
  FDebugMaterial.Free;
  FDebugBuffer.Free;
  inherited Destroy;
end;

procedure TTT3DScene.DoKeypress(Sym: TSDL_KeySym; Mode: TsdlKeyActionMode;
  var Handled: Boolean);
begin
  case Sym.sym of
    SDLK_KP0:
    begin
      if Mode = kmPress then
      begin
        FMoving := True;
        FMoveTarget := Vector2(0.0, 0.0);
        FRotating := True;
        FRotateTarget := Vector2(-45.0, 45.0);
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
        FRotAccel.X -= 1000.0
      else
        FMoveAccel.Z -= 50.0 * FMove.Z;
    end;

    5: if Mode = kmPress then
    begin
      if SDL_GetKeyState(nil)[SDLK_LCTRL] or SDL_GetKeyState(nil)[SDLK_RCTRL] <> 0 then
        FRotAccel.X += 1000.0
      else
        FMoveAccel.Z += 50.0 * FMove.Z;
    end;

    6: if Mode = kmPress then
    begin
      if SDL_GetKeyState(nil)[SDLK_LSHIFT] or SDL_GetKeyState(nil)[SDLK_RSHIFT] <> 0 then
        FMoveAccel -= Vector3(AngleToVec(-FRot.Y * Pi / 180.0) * 10.0 * FMove.Z * 2.0, 0.0)
      else
        FRotAccel.Y -= 1000.0;
    end;

    7: if Mode = kmPress then
    begin
      if SDL_GetKeyState(nil)[SDLK_LSHIFT] or SDL_GetKeyState(nil)[SDLK_RSHIFT] <> 0 then
        FMoveAccel += Vector3(AngleToVec(-FRot.Y * Pi / 180.0) * 10.0 * FMove.Z * 2.0, 0.0)
      else
        FRotAccel.Y += 1000.0;
    end;
  end;
end;

procedure TTT3DScene.DoMouseMotion(Motion: TsdlMouseMotionEventData);
var
  V: TVector2;
begin
  if Motion.state and SDL_BUTTON(3) <> 0 then
  begin
    FRotAccel += Vector2(Motion.yrel, Motion.xrel) * 180.0;
    FRotating := False;
  end
  else if (Motion.state and SDL_BUTTON(2) <> 0) or ((Motion.state and SDL_BUTTON(1) <> 0) and (Motion.state and SDL_BUTTON(3) <> 0)) then
  begin
    V := AngleToVec(-FRot.Y * Pi / 180.0);
    FMoveAccel -= Vector3(V * Motion.xrel * FMove.Z * 2.0, 0.0);

    V := AngleToVec(-(FRot.Y + 90.0) * Pi / 180.0);
    FMoveAccel -= Vector3(V * Motion.yrel * FMove.Z * 2.0, 0.0);
    FMoving := False;
  end;
end;

procedure TTT3DScene.DoUpdate(const ATimeInterval: Double);
var
  D: TVector2;
  L: TVectorFloat;
begin
  if FMoving then
  begin
    D := FMoveTarget - FMove.Vec2;
    L := VLength(D);
    if L <= 0.05 then
    begin
      FMoveSpeed := Vector3(D * 10.0, FMoveAccel.Z);
      FMoving := False;
    end
    else
      FMoveSpeed := Vector3(D * 10.0, FMoveAccel.Z);
  end;

  if FRotating then
  begin
    D := FRotateTarget - FRot;
    if Abs(D.Y) > 180.0 then
      D.Y := -D.Y;
    L := VLength(D);
    if L <= 0.05 then
    begin
      FRotating := False;
      FRotSpeed := D * 7.5;
    end
    else
    begin
      FRotSpeed := D * 7.5;
    end;
  end;

  FMove += 0.5 * FMoveAccel * ATimeInterval * ATimeInterval + FMoveSpeed * ATimeInterval;
  FMoveSpeed += FMoveAccel * ATimeInterval;
  FMoveAccel /= 180 * ATimeInterval;
  FMoveSpeed /= 110 * ATimeInterval;

  FRot += 0.5 * FRotAccel * ATimeInterval * ATimeInterval + FRotSpeed * ATimeInterval;
  FRotSpeed += FRotAccel * ATimeInterval;
  FRotAccel /= 180 * ATimeInterval;
  FRotSpeed /= 110 * ATimeInterval;

  if FRot.X < -80.0 then
  begin
    FRotAccel.X := 0.0;
    FRot.X := -80.0;
  end;

  if FRot.X > -10.0 then
  begin
    FRotAccel.X := 0.0;
    FRot.X := -10.0;
  end;

  if FRot.Y > 360.0 then
    FRot.Y -= 360.0;
  if FRot.Y < 0.0 then
    FRot.Y += 360.0;

  if FMove.Z >= -4.3 then
  begin
    FMove.Z := -4.3;
    FMoveAccel.Z := 0.0;
  end;

  DoUpdateBackgroundGeometry;
end;

procedure TTT3DScene.DoRenderBackground;
begin
  SetupPerspective(AbsLeft, AbsWidth, AbsTop, AbsHeight, 1.0, 1000.0, Config.Video.FOV);
  glDisable(GL_SCISSOR_TEST);
  glEnable(GL_DEPTH_TEST);

  glTranslatef(0.0, 0.0, FMove.Z);
  glRotatef(FRot.X, 1.0, 0.0, 0.0);
  glRotatef(FRot.Y, 0.0, 0.0, 1.0);
  glTranslatef(FMove.X, FMove.Y, FZ);

  FDebugMaterial.BindForRendering(False);
  FDebugMaterial.Render(GL_LINES);
  FDebugMaterial.UnbindForRendering;

  FTerrain.BindForRendering;
  FTerrain.Render;
  FTerrain.UnbindForRendering;

  SetupOrthoDefault(0, Config.Video.Width, 0, Config.Video.Height);
  glClear(GL_DEPTH_BUFFER_BIT);
  glDisable(GL_DEPTH_TEST);
  glEnable(GL_SCISSOR_TEST);

  DoRenderBackgroundGeometry;
end;

end.

