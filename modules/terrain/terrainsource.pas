unit TerrainSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  TERRAIN_GEOMETRY_CHUNK_SIZE = 64;
  TERRAIN_GEOMETRY_BLOCK_SIZE = 64;

type
  ETerrainError = class (Exception);

  { TTerrainSource }

  TTerrainSource = class (TObject)
  public
    function Height: Integer; virtual; abstract;
    procedure GetData(const X, Y, W, H: Integer; const TargetBuffer: PSingle); virtual; abstract;
    function Width: Integer; virtual; abstract;
  end;

implementation

end.

