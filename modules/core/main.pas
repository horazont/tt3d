unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ioSDL, sdl, ioLog, dglOpenGL, math, ioConfig, TransportRail,
  GTXDG, GLGeometry;

type

  { TTT3D }

  TTT3D = class (TsdlApplication)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FRotationX, FRotationY: Double;
    FSettings: TTT3DSettings;
    FVideoInfo: PSDL_VideoInfo;
    FVideoFlags: Cardinal;
    FVideoSurface: PSDL_Surface;
    FZ: Double;
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
    procedure HandleMouseMotion(Motion: TsdlMouseMotionEventData); override;
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
    GeneralLog.LogFmt(lmetFatal, 'Couldn''t get video info. %s', 'TSomething.InitSDL', [SDL_GetError]);
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
    GeneralLog.Log(lmetInfo, 'hw available.', 'TSomething.InitSDL');
    GeneralLog.LogFmt(lmetInfo, 'Hardware Info. Memory: %.2f MiBytes', 'TSomething.InitSDL', [(FVideoInfo^.video_mem)/1024/1024]);
  end
  else
  begin
    FVideoFlags := FVideoFlags or SDL_SWSURFACE;
    GeneralLog.Log(lmetInfo, 'hw not available.', 'TSomething.InitSDL');
  end;

  if FVideoInfo^.blit_hw <> 0 then FVideoFlags := SDL_HWACCEL;

  SDL_WM_SetCaption(PChar(GetWindowTitle), nil);

  //SetGLAttributes(24, 8, 32, 0, 0);

  SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);
  {$ifdef Antialias}
  SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 1);
  SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, 4);
  {$endif}

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
    GeneralLog.Log(lmetFatal, 'Couldn''t initialize OpenGL', 'TSomething.InitDGL');
    Terminate;
    Exit;
  end
  else
    GeneralLog.Log(lmetInfo, 'dglOpenGL loaded.', 'TSomething.InitDGL');

  ReadOpenGLCore;
  GeneralLog.Log(lmetInfo, 'OpenGL Core read.', 'TSomething.InitDGL');
  ReadExtensions;
  GeneralLog.Log(lmetInfo, 'Extensions read.', 'TSomething.InitDGL');

  glClearColor(0.0, 0.0, 0.15, 1.0);
  glClearDepth(1.0);
  glDisable(GL_CULL_FACE);
  GeneralLog.Log(lmetInfo, 'OpenGL settings applied', 'TSomething.InitDGL');

  GeneralLog.OutputExtensions;

  GeneralLog.Log(lmetInfo, 'Infos read', 'TSomething.InitDGL');

  GeneralLog.Log(lmetInfo, 'OpenGL initialized', 'TSomething.InitDGL');
  inherited;
end;

procedure TTT3D.InitData;
begin
  FRotationX := 0.0;
  FRotationY := 0.0;
  FZ := 5.0;
  inherited InitData;
end;

procedure TTT3D.PerFrame(TimeInterval: Double);
begin
  SetupPerspective(0, Settings.Video.Width, 0, Settings.Video.Height, 1.0, 100.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glTranslatef(0.0, 0.0, -FZ);
  glRotatef(FRotationX, 1.0, 0.0, 0.0);
  glRotatef(FRotationY, 0.0, 1.0, 0.0);

  glColor4f(1, 1, 1, 1);
  glBegin(GL_QUADS);
    glVertex3f(-1.0, 0.0, -1.0);
    glVertex3f(-1.0, 0.0, 1.0);
    glVertex3f(1.0, 0.0, 1.0);
    glVertex3f(1.0, 0.0, -1.0);
  glEnd;

  SDL_GL_SwapBuffers;
end;

procedure TTT3D.FreeData;
begin
  inherited FreeData;
end;

procedure TTT3D.FreeDGL;
begin
  inherited FreeDGL;
end;

procedure TTT3D.FreeSDL;
begin
  inherited FreeSDL;
end;

procedure TTT3D.HandleMouseMotion(Motion: TsdlMouseMotionEventData);
begin
  if Motion.state and SDL_BUTTON(1) <> 0 then
  begin
    FRotationX += Motion.yrel;
    FRotationY += Motion.xrel;
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

