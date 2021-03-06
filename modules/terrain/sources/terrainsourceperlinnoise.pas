unit TerrainSourcePerlinNoise;
(**********************************************************************
File name: terrainsourceperlinnoise.pas
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
  Classes, SysUtils, TerrainSource, Geometry;

type

  { TTerrainSourcePerlinNoise }

  TTerrainSourcePerlinNoise = class (TTerrainSource)
  public
    constructor Create(const AWidth, AHeight, PNXOffset, PNYOffset: Integer;
      const Persistence: Double; const Octaves: Cardinal; AXScale, AYScale, AZScale, AZOffset: Double);
  private
    FWidth, FHeight: Integer;
    FPNXOffset, FPNYOffset: Double;
    FXScale, FYScale, FZScale, FZOffset: Double;
    FPersistence: Double;
    FOctaves: Cardinal;
  protected
    class function InterpolateCos(const A, B: TVectorFloat; const F: TVectorFloat): TVectorFloat; inline;
    class function Noise(const X, Y: Integer): TVectorFloat; inline;
    class function SmoothedNoise(const X, Y: Integer): TVectorFloat; inline;
  public
    class function InterpolatedNoise(const Pos: TVector2): TVectorFloat; inline;
    class function PerlinNoise(const Pos: TVector2; const Persistence: Double; const Octaves: Cardinal): TVectorFloat;
  public
    function Height: Integer; override;
    procedure GetData(const AX, AY, AW, AH: Integer; const TargetBuffer: PSingle);
       override;
    function Width: Integer; override;
  end;

implementation

{ TTerrainSourcePerlinNoise }

constructor TTerrainSourcePerlinNoise.Create(const AWidth, AHeight, PNXOffset,
  PNYOffset: Integer; const Persistence: Double; const Octaves: Cardinal;
  AXScale, AYScale, AZScale, AZOffset: Double);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FPNXOffset := PNXOffset;
  FPNYOffset := PNYOffset;
  FPersistence := Persistence;
  FOctaves := Octaves;
  FXScale := AXScale;
  FYScale := AYScale;
  FZScale := AZScale;
  FZOffset := AZOffset;
end;

class function TTerrainSourcePerlinNoise.InterpolateCos(const A,
  B: TVectorFloat; const F: TVectorFloat): TVectorFloat;
var
  FC: TVectorFloat;
begin
  FC := Sqr(Cos(F * Pi * 0.5));
  Exit(FC * A + (1.0 - FC) * B);
end;

{$ifopt Q+}
{$define WasQ}
{$Q-}
{$endif}
class function TTerrainSourcePerlinNoise.Noise(const X, Y: Integer
  ): TVectorFloat;
var
  N: Integer;
begin
  N := x + y * 57;
  N := (N shl 13) xor N;
  Exit(1.0 - ( ( n * ( n * n * 15731 + 789221) + 1376312589) and $7fffffff) / 1073741824.0);
end;
{$ifdef WasQ}
{$undef WasQ}
{$Q+}
{$endif}

class function TTerrainSourcePerlinNoise.SmoothedNoise(const X, Y: Integer
  ): TVectorFloat;
begin
  Exit(
      (Noise(X-1, Y-1) + Noise(X+1, Y-1) + Noise(X-1, Y+1) + Noise(X+1, Y+1)) / 16.0 // corners
    + (Noise(X-1, Y) + Noise(X+1, Y) + Noise(X, Y-1) + Noise(X, Y+1)) / 8.0 // sides
    + Noise(X, Y) / 4.0 // center
  );
end;

class function TTerrainSourcePerlinNoise.InterpolatedNoise(const Pos: TVector2
  ): TVectorFloat; inline;
var
  IX, IY: Integer;
  FX, FY: Double;
  V: array [0..3] of Double;
  IV: array [0..1] of Double;
begin
  IX := Trunc(Pos.X);
  FX := Abs(Pos.X - IX);
  IY := Trunc(Pos.Y);
  FY := Abs(Pos.Y - IY);

  V[0] := SmoothedNoise(IX,     IY);
  V[1] := SmoothedNoise(IX + 1, IY);
  V[2] := SmoothedNoise(IX,     IY + 1);
  V[3] := SmoothedNoise(IX + 1, IY + 1);

  IV[0] := InterpolateCos(V[0], V[1], FX);
  IV[1] := InterpolateCos(V[2], V[3], FX);

  Exit(InterpolateCos(IV[0], IV[1], FY));
end;

class function TTerrainSourcePerlinNoise.PerlinNoise(const Pos: TVector2;
  const Persistence: Double; const Octaves: Cardinal): TVectorFloat;
var
  n: Integer;
  I: Integer;
  f, a: Double;
begin
  Assert(Octaves > 0);
  n := Octaves - 1;
  Result := 0.0;
  f := 1.0;
  a := 1.0;
  for I := 0 to n do
  begin
    Result += InterpolatedNoise(Pos * f) * a;
    f *= 2.0;
    a *= Persistence;
  end;
end;

function TTerrainSourcePerlinNoise.Height: Integer;
begin
  Result := FHeight;
end;

procedure TTerrainSourcePerlinNoise.GetData(const AX, AY, AW, AH: Integer;
  const TargetBuffer: PSingle);
var
  X, Y: Integer;
  AbsX, AbsY: Integer;
  XF, YF: Double;
begin
  YF := AY * FYScale + FPNYOffset;
  AbsY := 0;
  for Y := AY to (AY + AH) - 1 do
  begin
    AbsX := 0;
    XF := AX * FXScale + FPNXOffset;
    for X := AX to (AX + AW) - 1 do
    begin
      TargetBuffer[AbsX + AbsY * AW] := PerlinNoise(Vector2(XF, YF), FPersistence, FOctaves) * FZScale + FZOffset;
      XF += FXScale;
      Inc(AbsX);
    end;
    YF += FYScale;
    Inc(AbsY);
  end;
end;

function TTerrainSourcePerlinNoise.Width: Integer;
begin
  Result := FWidth;
end;

end.

