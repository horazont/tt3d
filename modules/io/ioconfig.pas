unit ioConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTConfig, GTPaths, GTBase;

type

  { TTT3DPaths }

  TTT3DPaths = class (TGTPaths)
  public
    class function GetAppName: String; override;
  end;

  { TTT3DSettingsVideo }

  TTT3DSettingsVideo = class (TGTBaseObject)
  public
    constructor Create; override;
  private
    FFOV: Double;
    FBPP: Integer;
    FFullscreen: Boolean;
    FHeight: Integer;
    FWidth: Integer;
  published
    property Fullscreen: Boolean read FFullscreen write FFullscreen default False;
    property FOV: Double read FFOV write FFOV;
    property BPP: Integer read FBPP write FBPP default 32;
    property Height: Integer read FHeight write FHeight default 768;
    property Width: Integer read FWidth write FWidth default 1024;
  end;

  { TTT3DSettings }

  TTT3DSettings = class (TGTConfig)
  private
    FVideo: TTT3DSettingsVideo;
  public
    constructor Create; override;
    destructor Destroy; override;
  protected
    function DefaultNodeName: String; override;
  published
    property Video: TTT3DSettingsVideo read FVideo;
  end;

var
  Config: TTT3DSettings;

implementation

{ TTT3DPaths }

class function TTT3DPaths.GetAppName: String;
begin
  Result := 'tt3d';
end;

{ TTT3DSettings }

constructor TTT3DSettings.Create;
begin
  inherited Create;
  FVideo := TTT3DSettingsVideo.Create;
end;

destructor TTT3DSettings.Destroy;
begin
  FVideo.Free;
  inherited Destroy;
end;

function TTT3DSettings.DefaultNodeName: String;
begin
  Result := 'tt3DSettings';
end;

{ TTT3DSettingsVideo }

constructor TTT3DSettingsVideo.Create;
begin
  inherited Create;
  FFOV := 60.0;
  FFullscreen := False;
  FBPP := 32;
  FHeight := 768;
  FWidth := 1024;
end;

initialization
Paths := TTT3DPaths;

Config := TTT3DSettings.Create;

finalization
Config.Save;
FreeAndNil(Config);

end.

