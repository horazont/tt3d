unit ioSDL;

{$mode objfpc}{$H+}
{$define DebugMessages}

interface

uses
  Classes, SysUtils, dglOpenGL, sdl, sdl_net, sdl_ttf, sdl_image, ioLog,
  CustApp, GTXDG;
  
type
  TsdlKeyActionMode = (kmPress, kmRelease);
  TsdlMouseMotionEventData = record
    type_: UInt8; // SDL_MOUSEMOTION
    which: UInt8; // The mouse device index
    state: UInt8; // The current button state
    x, y: SInt32; // The X/Y coordinates of the mouse
    xrel: SInt16; // The relative motion in the X direction
    yrel: SInt16; // The relative motion in the Y direction
  end;
  TsdlMouseButtonEventData = record
    type_: UInt8;  // SDL_MOUSEBUTTONDOWN or SDL_MOUSEBUTTONUP
    which: UInt8;  // The mouse device index
    button: UInt8; // The mouse button index
    state: UInt8;  // SDL_PRESSED or SDL_RELEASED
    x: SInt32;     // The X coordinates of the mouse at press time
    y: SInt32;     // The Y coordinates of the mouse at press time
  end;
  
  TsdlApplicationVersionStatus = (avsDevelopmentVersion, avsPreAlpha, avsAlpha,
    avsBeta, avsRelease);
  // TsdlApplicationVersionNumber
  //  The numbers are ordered by their weight, descending. This means, that the
  //  most significant version is on index 0.
  TsdlApplicationVersionNumber = array [0..2] of Word;
  TsdlApplicationVersion = record
    Number: TsdlApplicationVersionNumber;
    Status: TsdlApplicationVersionStatus;
  end;

  TsdlInitLevel = (ilSDL, ilDGL, ilData);
  TsdlInitLevels = set of TsdlInitLevel;

  TsdlSettings = class (TObject)
  private
    FScreenWidth, FScreenHeight, FBPP: Word;
    FFullscreen: Boolean;
  public
    property ScreenWidth: Word read FScreenWidth write FScreenWidth;
    property ScreenHeight: Word read FScreenHeight write FScreenHeight;
    property BPP: Word read FBPP write FBPP;
    property Fullscreen: Boolean read FFullscreen write FFullscreen;
  
    procedure LoadDefaults; virtual;
    procedure LoadFromFile(const FileName: String; DefaultsOnError: Boolean = True);
    procedure SaveToFile(const FileName: String);
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
  end;

  TsdlApplication = class (TCustomApplication)
  public
    constructor Create(SDLFlags: UInt32);
    destructor Destroy; override;
  private
    FTerminated: Boolean;
    FInitLevelsMutex: PSDL_Mutex;
    FInitLevels: TsdlInitLevels;
    FSettingsMutex: PSDL_Mutex;
    FLastUpdate: UInt32;
    
    FFPS: Double;
    FEffectiveFPS: Double;
    FCurrentFrameCount: UInt32;
    FCurrentFPSTime: UInt32;
    FCurrentEffectiveFrameCount: UInt32;
    FCurrentEffectiveFPSTime: UInt32;
    FSettings: TsdlSettings;
    
    function GetInitLevels: TsdlInitLevels;
    procedure SetInitLevels(AInitLevels: TsdlInitLevels);
    
    function GetSettings: TsdlSettings;
  protected

    FStartTime: UInt32;
    
    function GetApplicationName: String; virtual; abstract;
    function GetApplicationVersion: TsdlApplicationVersion; virtual; abstract;
    function GetApplicationVersionStr: String; virtual;
    
    procedure InitApp;
    procedure ProcessParams; virtual;
    procedure InitSettings; virtual;
    procedure InitSDL; virtual;
    procedure InitDGL; virtual;
    procedure InitData; virtual;
    
    procedure PerIteration; virtual;
    procedure PerFrame(TimeInterval: Double); virtual;
    procedure PerInsertedFrame; virtual;

    procedure FreeApp;
    procedure FreeData; virtual;
    procedure FreeDGL; virtual;
    procedure FreeSDL; virtual;
    
    procedure AddInitLevels(AInitLevels: TsdlInitLevels);
    procedure RemInitLevels(AInitLevels: TsdlInitLevels);
  public
    property InitLevels: TsdlInitLevels read GetInitLevels write SetInitLevels;
    property Settings: TsdlSettings read GetSettings;

    procedure RunApp;
    procedure HandleUnhandledEvent(AEvent: TSDL_Event); virtual;
    procedure HandleKeypress(Sym: TSDL_KeySym; Mode: TsdlKeyActionMode); virtual;
    procedure HandleMouseMotion(Motion: TsdlMouseMotionEventData); virtual;
    procedure HandleMouseButton(Button: TsdlMouseButtonEventData; Mode: TsdlKeyActionMode); virtual;
    procedure HandleQuit; virtual;

    function GetFPS: Double;
    function GetEffectiveFPS: Double;
    
    procedure Terminate;
    
    procedure SaveSettings(const FileName: String);
  end;
  
function sdlApplicationVersion(Major, Minor, Release: Word; Status: TsdlApplicationVersionStatus): TsdlApplicationVersion;
function sdlAVSToStr(AAVS: TsdlApplicationVersionStatus): String;

var
  AppPath: String;

implementation

{ TsdlSettings }

procedure TsdlSettings.LoadDefaults;
begin
  FScreenWidth := 800;
  FScreenHeight := 600;
  FBPP := 32;
  FFullscreen := False;
end;

procedure TsdlSettings.LoadFromFile(const FileName: String; DefaultsOnError: Boolean = True);
var
  FileStream: TFileStream;
begin
  if not FileExists(FileName) then
  begin
    if DefaultsOnError then
      LoadDefaults;
    Exit;
  end;
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    try
      LoadFromStream(FileStream);
    except
      if DefaultsOnError then
        LoadDefaults;
    end;
  finally
    FileStream.Destroy;
  end;
end;

procedure TsdlSettings.SaveToFile(const FileName: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Destroy;
  end;
end;

procedure TsdlSettings.LoadFromStream(Stream: TStream);
begin
  Stream.Read(FScreenWidth, SizeOf(Word));
  Stream.Read(FScreenHeight, SizeOf(Word));
  Stream.Read(FBPP, SizeOf(Word));
  Stream.Read(FFullscreen, SizeOf(Boolean));
end;

procedure TsdlSettings.SaveToStream(Stream: TStream);
begin
  Stream.Write(FScreenWidth, SizeOf(Word));
  Stream.Write(FScreenHeight, SizeOf(Word));
  Stream.Write(FBPP, SizeOf(Word));
  Stream.Write(FFullscreen, SizeOf(Boolean));
end;

{ TsdlApplication }

constructor TsdlApplication.Create(SDLFlags: UInt32);
begin
  inherited Create(nil);
  FTerminated := False;
  if SDL_Init(SDLFlags) < 0 then
  begin
    if System.IsConsole then
      WriteLn(StrPas(SDL_GetError));
  end;
  FInitLevelsMutex := SDL_CreateMutex;
  FSettingsMutex := SDL_CreateMutex;
  
  FFPS := 0.0;
  FEffectiveFPS := 0.0;
  FCurrentFrameCount := 0;
  FCurrentFPSTime := 0;
  FCurrentEffectiveFrameCount := 0;
  FCurrentEffectiveFPSTime := 0;
  
  InitLevels := [];
end;

destructor TsdlApplication.Destroy;
begin
  WriteLn('FreeApp');
  FreeApp;
  WriteLn('Mutexes');
  SDL_LockMutex(FInitLevelsMutex);
  SDL_DestroyMutex(FInitLevelsMutex);
  SDL_LockMutex(FSettingsMutex);
  SDL_DestroyMutex(FSettingsMutex);
  WriteLn('GeneralLog');
  FreeAndNil(GeneralLog);
  WriteLn('SDL');
  SDL_Quit;
  WriteLn('Self');
  inherited Destroy;
  WriteLn('Done');
end;

function TsdlApplication.GetInitLevels: TsdlInitLevels;
begin
  SDL_LockMutex(FInitLevelsMutex);
  Result := FInitLevels;
  SDL_UnlockMutex(FInitLevelsMutex);
end;

procedure TsdlApplication.SetInitLevels(AInitLevels: TsdlInitLevels);
begin
  SDL_LockMutex(FInitLevelsMutex);
  FInitLevels := AInitLevels;
  SDL_UnlockMutex(FInitLevelsMutex);
end;

function TsdlApplication.GetSettings: TsdlSettings;
begin
  SDL_LockMutex(FSettingsMutex);
  Result := FSettings;
  SDL_UnlockMutex(FSettingsMutex);
end;

function TsdlApplication.GetApplicationVersionStr: String;
var
  Suffix: String;
  Rec: TsdlApplicationVersion;
begin
  Rec := GetApplicationVersion;
  Suffix := sdlAVSToStr(Rec.Status);
  if Suffix <> '' then
    Suffix := ' ' + Suffix;
  Result := Format('V%d.%d.%d%s', [Rec.Number[0], Rec.Number[1], Rec.Number[2], Suffix]);
end;

procedure TsdlApplication.InitApp;
begin
  ProcessParams;
  InitSettings;
  InitSDL;
  InitDGL;
  InitData;
end;

procedure TsdlApplication.ProcessParams;
begin

end;

procedure TsdlApplication.InitSettings;
begin
  FSettings := TsdlSettings.Create;
  FSettings.LoadFromFile(AppPath + 'settings.dat', True);
end;

procedure TsdlApplication.InitSDL;
begin
  AddInitLevels([ilSDL]);
end;

procedure TsdlApplication.InitDGL;
begin
  AddInitLevels([ilDGL]);
end;

procedure TsdlApplication.InitData;
begin
  AddInitLevels([ilData]);
end;

procedure TsdlApplication.PerIteration;
begin
  // On every cyclus and before every PerFrame / PerInsertedFrame this is called.
end;

procedure TsdlApplication.PerFrame(TimeInterval: Double);
begin
  // Do some rendering here...
end;

procedure TsdlApplication.PerInsertedFrame;
begin
  // This is called when the last frame took not enough time.
end;

procedure TsdlApplication.FreeApp;
begin
  if (ilData in InitLevels) then
    FreeData;
  if (ilDGL in InitLevels) then
    FreeDGL;
  if (ilSDL in InitLevels) then
    FreeSDL;
  FreeAndNil(FSettings);
end;

procedure TsdlApplication.FreeData;
begin
  RemInitLevels([ilData]);
end;

procedure TsdlApplication.FreeDGL;
begin
  RemInitLevels([ilDGL]);
end;

procedure TsdlApplication.FreeSDL;
begin
  RemInitLevels([ilSDL]);
end;

procedure TsdlApplication.AddInitLevels(AInitLevels: TsdlInitLevels);
begin
  SetInitLevels(GetInitLevels + AInitLevels);
end;

procedure TsdlApplication.RemInitLevels(AInitLevels: TsdlInitLevels);
begin
  SetInitLevels(GetInitLevels - AInitLevels);
end;

procedure TsdlApplication.RunApp;

function MkMouseMotionData(SDLEvent: TSDL_MouseMotionEvent): TsdlMouseMotionEventData;
begin
  Result.type_ := SDLEvent.type_;
  Result.state := SDLEvent.state;
  Result.which := SDLEvent.which;
  Result.x := SDLEvent.x;
  Result.xrel := SDLEvent.xrel;
  Result.y := SDLEvent.y;
  Result.yrel := SDLEvent.yrel;
end;

function MkMouseButtonData(SDLEvent: TSDL_MouseButtonEvent): TsdlMouseButtonEventData;
begin
  Result.state := SDLEvent.state;
  Result.button := SDLEvent.button;
  Result.type_ := SDLEvent.type_;
  Result.which := SDLEvent.which;
  Result.x := SDLEvent.x;
  Result.y := SDLEvent.y;
end;

var
  Event: TSDL_Event;
  CurrentUpdate: UInt32;
  TimeInterval: UInt32;
  
  ThisIteration: UInt32;
  ThisFrame: UInt32;
begin
  GeneralLog := TlmLogFile.Create(AppPath + 'log.txt', lmlmOverwrite, AppPath + 'log.html');
  try
    FStartTime := SDL_GetTicks;
    {$ifdef DebugMessages}
    GeneralLog.LogLevel := lmetDebug;
    {$else}
    GeneralLog.LogLevel := lmetDebug;
    {$endif}
    GeneralLog.MirrorLogToConsole := True;
    GeneralLog.Log(lmetInfo, GetApplicationName + ' ' + GetApplicationVersionStr + ' starting up.', 'TsdlApplication.RunApp');

    try
      InitApp;
      GeneralLog.Log(lmetInfo, GetApplicationName + ' ' + GetApplicationVersionStr + ' initialized.', 'TsdlApplication.RunApp');
    except
      on E: Exception do
      begin
        //GeneralLog.LogFmt(lmetFatal, 'Exception occured during initialization.'+#13#10+'Class: %s'+#13#10+'Message: %s', 'TsdlApplication.RunApp', [E.ClassName, E.Message]);
        GeneralLog.LogException(lmetFatal, E, 'TsdlApplication.RunApp', 'during initialization.');
        FreeApp;
        raise;
      end
      else
      begin
        GeneralLog.Log(lmetFatal, 'Exception occured during initialization. Couldn''t identify exception.', 'TsdlApplication.RunApp');
        FreeApp;
        raise;
      end;
    end;
    try
      FLastUpdate := SDL_GetTicks;
      while (not FTerminated) do
      begin
        ThisIteration := SDL_GetTicks;
        while (SDL_PollEvent(@Event) = 1) do
        begin
          case Event.type_ of
            SDL_KEYDOWN:
              HandleKeypress(Event.key.keysym, kmPress);
            SDL_KEYUP:
              HandleKeypress(Event.key.keysym, kmRelease);
            SDL_MOUSEMOTION:
              HandleMouseMotion(MkMouseMotionData(Event.motion));
            SDL_MOUSEBUTTONDOWN:
              HandleMouseButton(MkMouseButtonData(Event.button), kmPress);
            SDL_MOUSEBUTTONUP:
              HandleMouseButton(MkMouseButtonData(Event.button), kmRelease);
            SDL_QUITEV:
              HandleQuit;
          else
            HandleUnhandledEvent(Event);
          end;
        end;
        PerIteration;
        CurrentUpdate := SDL_GetTicks;
        TimeInterval := CurrentUpdate - FLastUpdate;
        if (TimeInterval >= 10) then
        begin
          FLastUpdate := CurrentUpdate;
          ThisFrame := SDL_GetTicks;
          PerFrame(TimeInterval / 1000);
          Inc(FCurrentFrameCount);
          Inc(FCurrentEffectiveFrameCount);
          FCurrentFPSTime += (SDL_GetTicks - ThisFrame);
          if (FCurrentFPSTime >= 1000) then
          begin
            FFPS := FCurrentFrameCount - ((FCurrentFPSTime - 1000) / 1000);
            FCurrentFrameCount := 0;
            FCurrentFPSTime := 0;
          end;
        end
        else
        begin
          PerInsertedFrame;
        end;
        FCurrentEffectiveFPSTime += (SDL_GetTicks - ThisIteration);
        if (FCurrentEffectiveFPSTime >= 1000) then
        begin
          FEffectiveFPS := FCurrentEffectiveFrameCount - ((FCurrentEffectiveFPSTime - 1000) / 1000);
          FCurrentEffectiveFrameCount := 0;
          FCurrentEffectiveFPSTime := 0;
        end;
      end;
      GeneralLog.LogFmt(lmetInfo, 'Runtime: %0.2f secs', 'TsdlApplication.RunApp', [(SDL_GetTicks - FStartTime) / 1000]);
    except
      //GeneralLog.LogFmt(lmetFatal, 'Exception occured during runtime
      {on E: Exception do
      begin
        GeneralLog.LogFmt(lmetFatal, 'Exception occured during runtime.'#13#10'%s : %s', 'TsdlApplication.RunApp', [E.ClassName, E.Message]);
        //raise E;
      end;}
      on E: Exception do
      begin
        GeneralLog.LogException(lmetFatal, E, 'TsdlApplication.RunApp', '');
        //FreeApp;
        GeneralLog.Log(lmetInfo, GetApplicationName + ' ' + GetApplicationVersionStr + ' shut down.', 'TsdlApplication.RunApp');
      end;
    end;
  finally
    {on E: Exception do
    begin
      GeneralLog.LogException(lmetFatal, E, 'TsdlManager.RunApp', 'at runtime.');
    end;}
  end;
end;

procedure TsdlApplication.HandleUnhandledEvent(AEvent: TSDL_Event);
begin

end;

procedure TsdlApplication.HandleKeypress(Sym: TSDL_KeySym; Mode: TsdlKeyActionMode);
begin

end;

procedure TsdlApplication.HandleMouseMotion(Motion: TsdlMouseMotionEventData);
begin

end;

procedure TsdlApplication.HandleMouseButton(Button: TsdlMouseButtonEventData; Mode: TsdlKeyActionMode);
begin

end;

procedure TsdlApplication.HandleQuit;
begin
  Terminate;
end;

function TsdlApplication.GetFPS: Double;
begin
  Result := FFPS;
end;

function TsdlApplication.GetEffectiveFPS: Double;
begin
  Result := FEffectiveFPS;
end;

procedure TsdlApplication.Terminate;
begin
  FTerminated := True;
end;

procedure TsdlApplication.SaveSettings(const FileName: String);
begin
  if FSettings <> nil then
    FSettings.SaveToFile(FileName);
end;

function sdlApplicationVersion(Major, Minor, Release: Word; Status: TsdlApplicationVersionStatus): TsdlApplicationVersion;
begin
  Result.Number[0] := Major;
  Result.Number[1] := Minor;
  Result.Number[2] := Release;
  Result.Status := Status;
end;

function sdlAVSToStr(AAVS: TsdlApplicationVersionStatus): String;
begin
  case AAVS of
    avsDevelopmentVersion: Result := 'development edition';
    avsPreAlpha: Result := 'pre-alpha';
    avsAlpha: Result := 'alpha';
    avsBeta: Result := 'beta';
    avsRelease: Result := '';
  end;
end;

initialization
AppPath := ExtractFilePath(ParamStr(0));

end.

