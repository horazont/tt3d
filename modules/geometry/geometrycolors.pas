unit GeometryColors;
(**********************************************************************
File name: geometrycolors.pas
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
  Classes, SysUtils, Geometry, math;

function HSVToRGB(const HSV: TVector4): TVector4;

implementation

const
  Pi2: Double = pi * 2;

procedure HSVToRGB(H, S, V: Double; var R, G, B: Double);
var
  f, hTemp, p, q, t: Double;
  i: Integer;
begin
  if (s = 0) then
  begin
    R := V;
    G := V;
    B := V;
    Exit;
  end;
  if (H < 0) then
    H := Pi2 - H;
  if (H >= Pi2) then
    H := H - Trunc(H / Pi2)*Pi2;
  //if (H < 0) then
  //  H := Pi * 2 - H;
  //if (H > 2 * Pi) then
  //  H := H - Trunc(1.0 / (Pi * 2 * h * Pi * 2));

  hTemp := h / (Pi2 / 6);
  i := Trunc(hTemp);
  f := hTemp - i;

  p := v * (1.0 - s);
  q := v * (1.0 - (s * f));
  t := v * (1.0 - (s * (1.0 - f)));

  case i of
    0: begin
      R := v;
      G := t;
      B := p;
    end;
    1: begin
      R := q;
      G := v;
      B := p;
    end;
    2: begin
      R := p;
      G := v;
      B := t;
    end;
    3: begin
      R := p;
      G := q;
      B := v;
    end;
    4: begin
      R := t;
      G := p;
      B := v;
    end;
    5: begin
      R := v;
      G := p;
      B := q;
    end;
  end;
end;

function HSVToRGB(const HSV: TVector4): TVector4;
begin
  Result.W := HSV.W;
  HSVToRGB(HSV.X, HSV.Y, HSV.Z, Result.X, Result.Y, Result.Z);
end;

end.

