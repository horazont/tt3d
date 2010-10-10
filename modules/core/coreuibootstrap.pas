unit coreUIBootstrap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ioSDL, sdl, ioLog, dglOpenGL, ioConfig, TransportGraph,
  GTXDG, GLGeometry, Geometry, TransportGeometryGL, uiGL;

type

  { TTT3D }

  TTT3D = class (TsdlApplication)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FSettings: TTT3DSettings;
    FVideoInfo: PSDL_VideoInfo;
    FVideoFlags: Cardinal;
    FVideoSurface: PSDL_Surface;
(*  private
    FGeometryManager: TTransportGeometryManager;
    FNodeA, FNodeB, FNodeC: TPathNode;*)
  private
    FUI: TuiRootLayer;
    FUIBuffer: TGLGeometryBuffer;
    FUIMaterials: TuiMaterials;
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
  protected
    function GetApplicationName: String; override;
    function GetApplicationVersion: TsdlApplicationVersion; override;
    function GetWindowTitle: String;
  public
    procedure InitSettings; override;
    procedure InitSDL; override;
    procedure InitDGL; override;
    procedure InitData; override;

    procedure PerFrame(TimeInterval: Double); override;

    procedure FreeData; override;
    procedure FreeDGL; override;
    procedure FreeSDL; override;
  public
    property Settings: TTT3DSettings read FSettings;
  public
    procedure HandleKeypress(Sym: TSDL_KeySym; Mode: TsdlKeyActionMode);
       override;
    procedure HandleMouseMotion(Motion: TsdlMouseMotionEventData); override;
    procedure HandleMouseButton(Button: TsdlMouseButtonEventData;
       Mode: TsdlKeyActionMode); override;
    procedure SetupOrthoDefault(const Left, Right, Top, Bottom: Integer);
    procedure SetupPerspective(const Left, Right, Top, Bottom: Integer;
      const NearClip, FarClip: Double);
  end;

implementation

{ TTT3D }

constructor TTT3D.Create;
begin
  inherited Create(SDL_INIT_VIDEO);
end;

destructor TTT3D.Destroy;
begin
  FSettings.Save;
  FSettings.Free;
  inherited Destroy;
end;

function TTT3D.GetApplicationName: String;
begin
  Result := XDG.GetAppName;
end;

function TTT3D.GetApplicationVersion: TsdlApplicationVersion;
begin
  Result := sdlApplicationVersion(0, 0, 0, avsDevelopmentVersion);
end;

function TTT3D.GetWindowTitle: String;
begin
  Result := 'tt3D';
end;

procedure TTT3D.InitSettings;
begin
  FSettings := TTT3DSettings.Create;
  FSettings.Load;
end;

procedure TTT3D.InitSDL;
begin
  FVideoInfo := SDL_GetVideoInfo;
  if FVideoInfo = nil then
  begin
    GeneralLog.LogFmt(lmetFatal, 'Couldn''t get video info. %s', 'TTT3D.InitSDL', [SDL_GetError]);
    SDL_Quit;
    Terminate;
    Exit;
  end;

  FVideoFlags := SDL_OPENGL or SDL_HWPALETTE{ or SDL_DOUBLEBUF or SDL_FULLSCREEN};

  if Settings.Video.Fullscreen then
    FVideoFlags := FVideoFlags or SDL_FULLSCREEN;
  if FVideoInfo^.hw_available <> 0 then
  begin
    FVideoFlags := FVideoFlags or SDL_HWSURFACE;
    GeneralLog.Log(lmetInfo, 'hw available.', 'TTT3D.InitSDL');
    GeneralLog.LogFmt(lmetInfo, 'Hardware Info. Memory: %.2f MiBytes', 'TTT3D.InitSDL', [(FVideoInfo^.video_mem)/1024/1024]);
  end
  else
  begin
    FVideoFlags := FVideoFlags or SDL_SWSURFACE;
    GeneralLog.Log(lmetInfo, 'hw not available.', 'TTT3D.InitSDL');
  end;

  if FVideoInfo^.blit_hw <> 0 then FVideoFlags := SDL_HWACCEL;

  SDL_WM_SetCaption(PChar(GetWindowTitle), nil);

  //SetGLAttributes(24, 8, 32, 0, 0);

  SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);
//  SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 1);
//  SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, 4);
(*  {$define Antialias}
  {$ifdef Antialias}*)
//  {$endif}

  FVideoSurface := SDL_SetVideoMode(Settings.Video.Width, Settings.Video.Height, Settings.Video.BPP, FVideoFlags);
  if FVideoSurface = nil then
  begin
    GeneralLog.LogFmt(lmetFatal, 'Couldn''t get video surface. Please install latest graphics drivers and retry. %s', 'TsdlStrategyGame.InitSDL', [SDL_GetError]);
    SDL_Quit;
    Terminate;
    Exit;
  end;

  SDL_enableUNICODE(1);
  //SDL_EnableKeyRepeat(250, 250);
  SDL_ShowCursor(1);
  {$ifdef grabinput}
  SDL_WM_GrabInput(SDL_GRAB_ON);
  {$endif}
  inherited InitSDL;
end;

procedure TTT3D.InitDGL;
begin
  if not InitOpenGL then
  begin
    GeneralLog.Log(lmetFatal, 'Couldn''t initialize OpenGL', 'TTT3D.InitDGL');
    Terminate;
    Exit;
  end
  else
    GeneralLog.Log(lmetInfo, 'dglOpenGL loaded.', 'TTT3D.InitDGL');

  ReadOpenGLCore;
  GeneralLog.Log(lmetInfo, 'OpenGL Core read.', 'TTT3D.InitDGL');
  ReadExtensions;
  GeneralLog.Log(lmetInfo, 'Extensions read.', 'TTT3D.InitDGL');

  glClearColor(0.5, 0.5, 0.5, 1.0);
  glClearDepth(1.0);
  glDisable(GL_CULL_FACE);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL);
  GeneralLog.Log(lmetInfo, 'OpenGL settings applied', 'TTT3D.InitDGL');

  GeneralLog.OutputExtensions;

  GeneralLog.Log(lmetInfo, 'Infos read', 'TTT3D.InitDGL');

  GeneralLog.Log(lmetInfo, 'OpenGL initialized', 'TTT3D.InitDGL');
  inherited;
end;

procedure TTT3D.InitData;
var
  I: Integer;
begin
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

  FUIBuffer := TGLGeometryBuffer.Create(TGLGeometryFormatP4C4T2.GetNeededVertexSize, GL_DYNAMIC_DRAW);
  FUIMaterials := TuiMaterials.Create;

  FUI := TuiRootLayer.Create;
  FUI.SetAbsMetrics(0, 0, FSettings.Video.Width, FSettings.Video.Height);

  (*FUI.Surface.Obj := TuiSurface3x3.Create;
  with TuiSurface3x3(FUI.Surface.Obj) do
  begin
    TopMargin := 5.0;
    LeftMargin := 5.0;
    RightMargin := 5.0;
    BottomMargin := 5.0;

    TopLeft.Color := Vector4(1.0, 0.0, 0.0, 1.0);
    TopCenter.Color := Vector4(1.0, 0.0, 0.0, 1.0);

    TopRight.Color := Vector4(0.0, 1.0, 0.0, 1.0);
    MiddleRight.Color := Vector4(0.0, 1.0, 0.0, 1.0);

    BottomRight.Color := Vector4(1.0, 1.0, 1.0, 1.0);
    BottomCenter.Color := Vector4(1.0, 1.0, 1.0, 1.0);

    BottomLeft.Color := Vector4(0.0, 0.0, 1.0, 1.0);
    MiddleLeft.Color := Vector4(0.0, 0.0, 1.0, 1.0);

    Center.TopLeft.Color := Vector4(0.5, 0.0, 0.0, 1.0);
    Center.TopRight.Color := Vector4(0.0, 0.5, 0.0, 1.0);
    Center.BottomLeft.Color := Vector4(0.0, 0.0, 0.5, 1.0);
    Center.BottomRight.Color := Vector4(0.5, 0.5, 0.5, 1.0);
  end;        *)

  FUI.Materials := FUIMaterials;
  FUI.Invalidate;

  (*
  FGeometryManager := TTransportGeometryManager.Create;
  FGeometryManager.Materials.DebugLine := FDebugMaterial;

  FNodeA := TPathNode.Create;
  FNodeA.Location := Vector3(-1.0, -0.5, 0.0);
  FNodeA.SidePairs[PAIR_DEFAULT].Tangent := Vector3(1.0, 0.0, 0.0);
  FNodeB := TPathNode.Create;
  FNodeB.Location := Vector3(1.0, 0.5, 0.0);
  FNodeB.SidePairs[PAIR_DEFAULT].Tangent := Vector3(1.0, 0.0, 0.0);
  FNodeC := TPathNode.Create;
  FNodeC.Location := Vector3(1.0, -0.5, 0.0);
  FNodeC.SidePairs[PAIR_DEFAULT].Tangent := Vector3(-1.0, 0.0, 0.0);

  FNodeA.Connect(
    SideDefinition(PAIR_DEFAULT, sdA),
    SideDefinition(PAIR_DEFAULT, sdB),
    FNodeB,
    TPathLinkBezier);

  FNodeB.Connect(
    SideDefinition(PAIR_DEFAULT, sdA),
    SideDefinition(PAIR_DEFAULT, sdB),
    FNodeC,
    TPathLinkArc);

  FNodeC.Connect(
    SideDefinition(PAIR_DEFAULT, sdA),
    SideDefinition(PAIR_DEFAULT, sdB),
    FNodeA,
    TPathLinkBezier
  );     *)



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

  inherited InitData;
end;

procedure TTT3D.PerFrame(TimeInterval: Double);
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

  FMove += 0.5 * FMoveAccel * TimeInterval * TimeInterval + FMoveSpeed * TimeInterval;
  FMoveSpeed += FMoveAccel * TimeInterval;
  FMoveAccel /= 180 * TimeInterval;
  FMoveSpeed /= 110 * TimeInterval;

  FRot += 0.5 * FRotAccel * TimeInterval * TimeInterval + FRotSpeed * TimeInterval;
  FRotSpeed += FRotAccel * TimeInterval;
  FRotAccel /= 180 * TimeInterval;
  FRotSpeed /= 110 * TimeInterval;

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

  SetupPerspective(0, Settings.Video.Width, 0, Settings.Video.Height, 1.0, 1000.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glEnable(GL_DEPTH_TEST);

  glTranslatef(0.0, 0.0, FMove.Z);
  glRotatef(FRot.X, 1.0, 0.0, 0.0);
  glRotatef(FRot.Y, 0.0, 0.0, 1.0);
  glTranslatef(FMove.X, FMove.Y, FZ);

  FDebugMaterial.BindForRendering(False);
  FDebugMaterial.Render(GL_LINES);
  FDebugMaterial.UnbindForRendering;

  SetupOrthoDefault(0, FSettings.Video.Width, 0, FSettings.Video.Height);
  glClear(GL_DEPTH_BUFFER_BIT);
  glDisable(GL_DEPTH_TEST);

  FUI.Update(TimeInterval);
  FUI.Render;

  SDL_GL_SwapBuffers;
end;

procedure TTT3D.FreeData;
begin
  FUI.Free;
  FUIMaterials.Free;
  FGrid.Free;
  FAxis.Free;
  FDebugMaterial.Free;
  FDebugBuffer.Free;
  (*FGeometryManager.Free;
  FNodeA.Free;
  FNodeB.Free;
  FNodeC.Free;*)
  inherited FreeData;
end;

procedure TTT3D.FreeDGL;
begin
  inherited FreeDGL;
end;

procedure TTT3D.FreeSDL;
begin
  SDL_Quit;
  inherited FreeSDL;
end;

procedure TTT3D.HandleKeypress(Sym: TSDL_KeySym; Mode: TsdlKeyActionMode);
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
        //FMoveAccel := Vector3((-2*(FMove.Vec2-Vector2(0.0, 0.0))*power(180, T))/(T*T), FMoveAccel.Z);
        (*FMoveAccel := Vector3((-2*(FMove.Vec2-Vector2(0.0, 0.0)*power(1980, T)))/(T * (2*power(18, T)+T*power(11, T))), FMoveAccel.Z);;
        FMoveSpeed := Vector3(0.0, 0.0, FMoveSpeed.Z);*)
      end;
    end;
    SDLK_ESCAPE:
    begin
      Terminate;
    end;
  end;
end;

procedure TTT3D.HandleMouseMotion(Motion: TsdlMouseMotionEventData);
var
  V: TVector2;
begin
  if Motion.state and SDL_BUTTON(3) <> 0 then
  begin
(*    FRotationX += Motion.yrel;
    FRotationZ += Motion.xrel;
    if FRotationX > -10.0 then
      FRotationX := -10.0;
    if FRotationX < -80.0 then
      FRotationX := -80.0;*)

    FRotAccel += Vector2(Motion.yrel, Motion.xrel) * 180.0;
    FRotating := False;
  end
  else if (Motion.state and SDL_BUTTON(2) <> 0) or ((Motion.state and SDL_BUTTON(1) <> 0) and (Motion.state and SDL_BUTTON(3) <> 0)) then
  begin
    V := AngleToVec(-FRot.Y * Pi / 180.0);
    (*FX -= V.X * Motion.xrel * FZoom * 0.0025;
    FY -= V.Y * Motion.xrel * FZoom * 0.0025;*)
    FMoveAccel -= Vector3(V * Motion.xrel * FMove.Z * 2.0, 0.0);

    V := AngleToVec(-(FRot.Y + 90.0) * Pi / 180.0);
    FMoveAccel -= Vector3(V * Motion.yrel * FMove.Z * 2.0, 0.0);
    FMoving := False;
    (*FX -= V.X * Motion.yrel * FZoom * 0.0025;
    FY -= V.Y * Motion.yrel * FZoom * 0.0025;*)
  end;
end;

procedure TTT3D.HandleMouseButton(Button: TsdlMouseButtonEventData;
  Mode: TsdlKeyActionMode);
begin
  //WriteLn(Button.button);
  case Button.button of
    4: if Mode = kmPress then
    begin
      FMoveAccel.Z -= 50.0 * FMove.Z;
    end;

    5: if Mode = kmPress then
    begin
      FMoveAccel.Z += 50.0 * FMove.Z;
    end;
  end;
end;

procedure TTT3D.SetupOrthoDefault(const Left, Right, Top, Bottom: Integer);
begin
  glViewport(Left, Top, Right - Left, Bottom - Top);

  glMatrixMode( GL_PROJECTION );
    glLoadIdentity;
    (*case ProjectionType of
      ptPerspective: gluPerspective( FFOV, (Right - Left) / (Bottom - Top), 0.1, 100.0 );
      ptOrthogonal: *)
    glOrtho(Left, Right, Bottom, Top, -10.0, 10.0);
    //end;
  glMatrixMode( GL_MODELVIEW );

  glLoadIdentity;
end;

procedure TTT3D.SetupPerspective(const Left, Right, Top, Bottom: Integer;
  const NearClip, FarClip: Double);
begin
  glViewport(Left, Top, Right - Left, Bottom - Top);

  glMatrixMode( GL_PROJECTION );
    glLoadIdentity;
    gluPerspective( Settings.Video.FOV, (Right - Left) / (Bottom - Top), NearClip, FarClip );
  glMatrixMode( GL_MODELVIEW );

  glLoadIdentity;
end;


end.

