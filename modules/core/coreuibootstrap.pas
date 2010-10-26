unit coreUIBootstrap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ioSDL, sdl, ioLog, dglOpenGL, ioConfig, TransportGraph,
  GTXDG, GLGeometry, Geometry, TransportGeometryGL, uiGL, coreScene, GLHelpers,
  uiTT3D, GeometryColors, math;

type

  { TTT3D }

  TTT3D = class (TsdlApplication)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FVideoInfo: PSDL_VideoInfo;
    FVideoFlags: Cardinal;
    FVideoSurface: PSDL_Surface;
  private
    FUI: TuiRootLayer;
    FUIBuffer: TGLGeometryBuffer;
    FUIMaterials: TuiMaterials;
    FUITabs: TuiTT3DRootTabWidget;
    FUIBackground: TuiSurface3x3;
    FUITabsBG: TuiSurface3x3;
    FUITheme: TuiTT3DTheme;
  protected
    function GetApplicationName: String; override;
    function GetApplicationVersion: TsdlApplicationVersion; override;
    function GetWindowTitle: String;
  public
    procedure InitSDL; override;
    procedure InitDGL; override;
    procedure InitData; override;

    procedure PerFrame(TimeInterval: Double); override;

    procedure FreeData; override;
    procedure FreeDGL; override;
    procedure FreeSDL; override;
  public
    procedure HandleKeypress(Sym: TSDL_KeySym; Mode: TsdlKeyActionMode);
       override;
    procedure HandleMouseMotion(Motion: TsdlMouseMotionEventData); override;
    procedure HandleMouseButton(Button: TsdlMouseButtonEventData;
       Mode: TsdlKeyActionMode); override;
  end;

implementation

{ TTT3D }

constructor TTT3D.Create;
begin
  inherited Create(SDL_INIT_VIDEO);
end;

destructor TTT3D.Destroy;
begin
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

  if Config.Video.Fullscreen then
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

  FVideoSurface := SDL_SetVideoMode(Config.Video.Width, Config.Video.Height, Config.Video.BPP, FVideoFlags);
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

  glClearColor(0.35, 0.5, 0.75, 1.0);
  //glClearColor(0.5, 0.5, 0.5, 1.0);
  glClearDepth(1.0);
  glDisable(GL_CULL_FACE);
  glDepthFunc(GL_LEQUAL);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  SetupOrthoDefault(0, Config.Video.Width, 0, Config.Video.Height);
  GeneralLog.Log(lmetInfo, 'OpenGL settings applied', 'TTT3D.InitDGL');

  GeneralLog.OutputExtensions;

  GeneralLog.Log(lmetInfo, 'Infos read', 'TTT3D.InitDGL');

  GeneralLog.Log(lmetInfo, 'OpenGL initialized', 'TTT3D.InitDGL');
  inherited;
end;

procedure TTT3D.InitData;
var
  BaseColorRGB, DarkerColorRGB: TVector4;
begin
  FUIBuffer := TGLGeometryBuffer.Create(TGLGeometryFormatP4C4T2.GetNeededVertexSize, GL_DYNAMIC_DRAW);
  FUIMaterials := TuiMaterials.Create;

  FUITheme := TuiTT3DTheme.Create;
  FUITheme.BaseColor := Vector4(Pi*4/3, 0.5, 0.5, 1.0);
  FUITheme.ActiveColor := Vector4(Pi*2/3, 0.5, 0.5, 1.0);

  FUI := TTT3DScene.Create;
  FUI.Materials := FUIMaterials;
  FUI.SetAbsMetrics(0, 0, Config.Video.Width, Config.Video.Height);

  BaseColorRGB := Vector4(FUITheme.BaseColor.Vec3, 0.75);
  BaseColorRGB.Y := 0.0;
  DarkerColorRGB := HSVToRGB(Vector4(BaseColorRGB.X, 0.0, Min(1.0, BaseColorRGB.Z + 0.25), 0.75));
  BaseColorRGB := HSVToRGB(BaseColorRGB);

  FUIBackground := TuiSurface3x3.Create;
  with FUIBackground do
  begin
    TopLeft.Color := DarkerColorRGB;
(*    TopLeft.TopLeft.Color := DarkerColorRGB;
    TopLeft.TopRight.Color := DarkerColorRGB;*)
    TopCenter.Color := BaseColorRGB;
    TopCenter.TopLeft.Color := DarkerColorRGB;
    TopCenter.BottomLeft.Color := DarkerColorRGB;
    TopRight.Color := BaseColorRGB;
    MiddleLeft.Color := BaseColorRGB;
    MiddleLeft.TopLeft.Color := DarkerColorRGB;
    MiddleLeft.TopRight.Color := DarkerColorRGB;
    BottomLeft.Color := BaseColorRGB;
    Center.Color := Vector4(0.0, 0.0, 0.0, 0.0);
    TopMargin := 17;
    LeftMargin := 28;
  end;

  FUITabsBG := TuiSurface3x3.Create;
  with FUITabsBG do
  begin
    BaseColorRGB := HSVToRGB(FUITheme.BaseColor);
    TopLeft.Color := BaseColorRGB;
    TopCenter.Color := BaseColorRGB;
    TopRight.Color := BaseColorRGB;
    MiddleLeft.Color := BaseColorRGB;
    MiddleRight.Color := BaseColorRGB;
    BottomLeft.Color := BaseColorRGB;
    BottomCenter.Color := BaseColorRGB;
    BottomRight.Color := BaseColorRGB;
    Center.Color := Vector4(0.0, 0.0, 0.0, 0.0);
    TopMargin := 1;
    LeftMargin := 1;
    RightMargin := 1;
    BottomMargin := 1;
  end;

  FUITabs := TuiTT3DRootTabWidget.Create;
  FUITabs.Parent := FUI;
  FUITabs.TabSurface[wsNormal].Theme := FUITheme;
  FUITabs.TabSurface[wsNormal].Shear := 16.0;
  FUITabs.BackgroundSurface.Obj := FUITabsBG;

  FUI.Background.Obj := FUIBackground;

  inherited InitData;
end;

procedure TTT3D.PerFrame(TimeInterval: Double);
begin
  glClear(GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT);
  glDisable(GL_DEPTH_TEST);

  FUI.Update(TimeInterval);
  FUI.Render;

  SDL_GL_SwapBuffers;
end;

procedure TTT3D.FreeData;
begin
  FUI.Free;
  FUIMaterials.Free;
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
    SDLK_ESCAPE:
    begin
      Terminate;
    end;
  else
    FUI.DeliverKeypress(Sym, Mode);
  end;
end;

procedure TTT3D.HandleMouseMotion(Motion: TsdlMouseMotionEventData);
begin
  FUI.DeliverMouseMotion(Motion);
end;

procedure TTT3D.HandleMouseButton(Button: TsdlMouseButtonEventData;
  Mode: TsdlKeyActionMode);
begin
  FUI.DeliverMouseButton(Button, Mode);
end;


end.

