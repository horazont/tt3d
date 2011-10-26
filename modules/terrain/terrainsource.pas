unit TerrainSource;
(**********************************************************************
File name: terrainsource.pas
This file is part of: tt3d

LICENSE

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
License for the specific language governing rights and limitations under
the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public license (the  "GPL License"), in which case  the
provisions of GPL License are applicable instead of those above.

FEEDBACK & QUESTIONS

For feedback and questions about tt3d please e-mail one of the authors:
    Jonas Wielicki <j.wielicki@sotecware.net>
**********************************************************************)

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

