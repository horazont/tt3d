unit GeometryColors;

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

