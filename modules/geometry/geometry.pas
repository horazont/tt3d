unit Geometry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

type
  TVectorFloat = Double;

  TVector2_Array = array [0..1] of TVectorFloat;
  TVector2 = record
  case Byte of
    0: (X, Y: TVectorFloat);
    1: (AsArray: TVector2_Array);
    2: (R, A: TVectorFloat);
    3: (S, T: TVectorFloat);
  end;
  PVector2 = ^TVector2;

  TVector3_Array = array [0..2] of TVectorFloat;
  TVector3 = record
  case Byte of
    0: (X, Y, Z: TVectorFloat);
    1: (AsArray: TVector3_Array);
    2: (R, G, B: TVectorFloat);
  end;
  PVector3 = ^TVector3;
  TTriangle3 = array [0..2] of TVector3;
  PTriangle3 = ^TTriangle3;

  TVector4_Array = array [0..3] of TVectorFloat;
  TVector4_Length = type TVector4_Array;
  TVector4 = record
  case Byte of
    0: (X, Y, Z, W: TVectorFloat);
    1: (AsArray: TVector4_Array);
    2: (R, G, B, A: TVectorFloat);
    3: (Vec3: TVector3);
  end;
  PVector4 = ^TVector4;
  TTriangle4 = array [0..2] of TVector4;
  PTriangle4 = ^TTriangle4;

  TCubicBezier1 = array [0..3] of TVectorFloat;
  TCubicBezier3 = array [0..3] of TVector3;
  TCubicBezier4 = array [0..3] of TVector4;

  TVector3f = array [0..2] of Single;
  PVector3f = ^TVector3f;
  TVector4f = array [0..3] of Single;
  PVector4f = ^TVector4f;

operator + (A, B: TVector3): TVector3; inline;
operator + (A, B: TVector4): TVector4; inline;

operator - (A, B: TVector3): TVector3; inline;
operator - (A, B: TVector4): TVector4; inline;
operator - (A: TVector3): TVector3; inline;
operator - (A: TVector4): TVector4; inline;

operator * (A, B: TVector3): Single; inline;
operator * (A, B: TVector4): Single; inline;

operator * (A: TVector3; B: TVectorFloat): TVector3; inline;
operator * (A: TVector4; B: TVectorFloat): TVector4; inline;
operator * (A: TVectorFloat; B: TVector3): TVector3; inline;
operator * (A: TVectorFloat; B: TVector4): TVector4; inline;

operator ** (A, B: TVector3): TVector3; inline;
operator ** (A: TCubicBezier1; B: TVectorFloat): TVectorFloat; inline;
operator ** (A: TCubicBezier3; B: TVectorFloat): TVector3; inline;
operator ** (A: TCubicBezier4; B: TVectorFloat): TVector4; inline;
// operator ** (A, B: TVector4f): TVector4f;

operator / (A: TVector3; B: TVectorFloat): TVector3; inline;
operator / (A: TVector4; B: TVectorFloat): TVector4; inline;

operator := (A: TVector3): TVector3f; inline;
operator := (A: TVector3f): TVector3; inline;
operator := (A: TVector4): TVector4f; inline;
operator := (A: TVector4f): TVector4; inline;

operator not (A: TVector3): TVector3; inline;
operator not (A: TVector4): TVector4; inline;

function Normalize(const Vec3: TVector3): TVector3; inline;
function Normalize(const Vec4: TVector4): TVector4; inline;
procedure NormalizeInPlace(var Vec3: TVector3); inline;
procedure NormalizeInPlace(var Vec4: TVector4); inline;

function VLength(Vec3: TVector3): TVectorFloat; inline;
function VLength(Vec4: TVector4): TVectorFloat; inline;

function BLength(Bezier: TCubicBezier3; Steps: Integer = 1000): TVectorFloat;
function BLength(Bezier: TCubicBezier4; Steps: Integer = 1000): TVectorFloat;

function CubicBezier1(const P1, P2, P3, P4: TVectorFloat): TCubicBezier1; inline;
function CubicBezier3(const P1, P2, P3, P4: TVector3): TCubicBezier3; inline;
function CubicBezier4(const P1, P2, P3, P4: TVector4): TCubicBezier4; inline;
function Vector3f(X: TVectorFloat; Y: TVectorFloat; Z: TVectorFloat): TVector3; inline;
function Vector4f(Vec3: TVector3; W: TVectorFloat = 0.0): TVector4; inline;
function Vector4f(X: TVectorFloat = 0.0; Y: TVectorFloat = 0.0; Z: TVectorFloat = 0.0; W: TVectorFloat = 0.0): TVector4; inline;

function FormatVector(Vec3: TVector3): String; inline;
function FormatVector(Vec4: TVector4): String; inline;

implementation

operator + (A, B: TVector3): TVector3;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
end;

operator + (A, B: TVector4): TVector4;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
  Result.W := A.W + B.W;
end;

operator - (A, B: TVector3): TVector3;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
end;

operator - (A, B: TVector4): TVector4;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
  Result.W := A.W - B.W;
end;

operator - (A: TVector3): TVector3;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
  Result.Z := -A.Z;
end;

operator - (A: TVector4): TVector4;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
  Result.Z := -A.Z;
  Result.W := -A.W;
end;

operator * (A, B: TVector3): Single;
begin
  Result := A.X * B.X +
            A.Y * B.Y +
            A.Z * B.Z;
end;

operator * (A, B: TVector4): Single;
begin
  Result := A.X * B.X +
            A.Y * B.Y +
            A.Z * B.Z +
            A.W * B.W;
end;

operator * (A: TVector3; B: TVectorFloat): TVector3;
begin
  Result.X := A.X * B;
  Result.Y := A.Y * B;
  Result.Z := A.Z * B;
end;

operator * (A: TVector4; B: TVectorFloat): TVector4;
begin
  Result.X := A.X * B;
  Result.Y := A.Y * B;
  Result.Z := A.Z * B;
  Result.W := A.W * B;
end;

operator * (A: TVectorFloat; B: TVector3): TVector3;
begin
  Result := B * A;
end;

operator * (A: TVectorFloat; B: TVector4): TVector4;
begin
  Result := B * A;
end;

operator ** (A, B: TVector3): TVector3;
begin
  Result.X := A.Y * B.Z - A.Z * B.Y;
  Result.Y := A.Z * B.X - A.X * B.Z;
  Result.Z := A.X * B.Y - A.Y * B.X;
end;

operator ** (A: TCubicBezier1; B: TVectorFloat): TVectorFloat; inline;
begin
  Result := (-A[0] + 3*A[1] - 3*A[2] + A[3]) * power(B, 3) +
            (3*A[0] - 6*A[1] + 3*A[2]) * power(B, 2) +
            (-3*A[0] + 3*A[1])*B +
            A[0];
end;

operator ** (A: TCubicBezier3; B: TVectorFloat): TVector3; inline;
begin
  Result := (-A[0] + 3*A[1] - 3*A[2] + A[3]) * power(B, 3) +
            (3*A[0] - 6*A[1] + 3*A[2]) * power(B, 2) +
            (-3*A[0] + 3*A[1])*B +
            A[0];
end;

operator ** (A: TCubicBezier4; B: TVectorFloat): TVector4; inline;
begin
  Result := (-A[0] + 3*A[1] - 3*A[2] + A[3]) * power(B, 3) +
            (3*A[0] - 6*A[1] + 3*A[2]) * power(B, 2) +
            (-3*A[0] + 3*A[1])*B +
            A[0];
end;

(*operator ** (A, B: TVector4): TVector4;
begin
  Result := Vector4f();
end;*)

operator / (A: TVector3; B: TVectorFloat): TVector3;
begin
  Result.X := A.X / B;
  Result.Y := A.Y / B;
  Result.Z := A.Z / B;
end;

operator / (A: TVector4; B: TVectorFloat): TVector4;
begin
  Result.X := A.X / B;
  Result.Y := A.Y / B;
  Result.Z := A.Z / B;
  Result.W := A.W / B;
end;

operator := (A: TVector3): TVector3f;
begin
  Result[0] := A.X;
  Result[1] := A.Y;
  Result[2] := A.Z;
end;

operator := (A: TVector3f): TVector3;
begin
  Result.X := A[0];
  Result.Y := A[1];
  Result.Z := A[2];
end;

operator := (A: TVector4): TVector4f;
begin
  Result[0] := A.X;
  Result[1] := A.Y;
  Result[2] := A.Z;
  Result[3] := A.W;
end;

operator := (A: TVector4f): TVector4;
begin
  Result.X := A[0];
  Result.Y := A[1];
  Result.Z := A[2];
  Result.W := A[3];
end;

operator not(A: TVector3): TVector3;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
  Result.Z := -A.Z;
end;

operator not(A: TVector4): TVector4;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
  Result.Z := -A.Z;
  Result.W := -A.W;
end;

function Normalize(const Vec3: TVector3): TVector3;
begin
  Result := Vec3;
  NormalizeInPlace(Result);
end;

function Normalize(const Vec4: TVector4): TVector4;
begin
  Result := Vec4;
  NormalizeInPlace(Result);
end;

procedure NormalizeInPlace(var Vec3: TVector3);
var
  Len: TVectorFloat;
begin
  Len := VLength(Vec3);
  Vec3.X := Vec3.X / Len;
  Vec3.Y := Vec3.Y / Len;
  Vec3.Z := Vec3.Z / Len;
end;

procedure NormalizeInPlace(var Vec4: TVector4);
var
  Len: TVectorFloat;
begin
  Len := VLength(Vec4);
  Vec4.X := Vec4.X / Len;
  Vec4.Y := Vec4.Y / Len;
  Vec4.Z := Vec4.Z / Len;
end;

function VLength(Vec3: TVector3): TVectorFloat;
begin
  Result := Sqrt(Sqr(Vec3.X) + Sqr(Vec3.Y) + Sqr(Vec3.Z));
end;

function VLength(Vec4: TVector4): TVectorFloat;
begin
  Result := Sqrt(Sqr(Vec4.X) + Sqr(Vec4.Y) + Sqr(Vec4.Z) + Sqr(Vec4.W));
end;

function BLength(Bezier: TCubicBezier3; Steps: Integer): TVectorFloat;
var
  I: Integer;
  Step: TVectorFloat;
  Current, Previous: TVector3;
begin
  Result := 0.0;
  Step := 1/Steps;
  Previous := Bezier ** 0.0;
  for I := 1 to Steps do
  begin
    Current := Bezier ** (I * Step);
    Result += VLength(Current - Previous);
    Previous := Current;
  end;
end;

function BLength(Bezier: TCubicBezier4; Steps: Integer): TVectorFloat;
var
  I: Integer;
  Step: TVectorFloat;
  Current, Previous: TVector4;
begin
  Result := 0.0;
  Step := 1/Steps;
  Previous := Bezier ** 0.0;
  for I := 1 to Steps do
  begin
    Current := Bezier ** (I * Step);
    Result += VLength(Current - Previous);
    Previous := Current;
  end;
end;

function CubicBezier1(const P1, P2, P3, P4: TVectorFloat): TCubicBezier1;
  inline;
begin
  Result[0] := P1;
  Result[1] := P2;
  Result[2] := P3;
  Result[3] := P4;
end;

function CubicBezier3(const P1, P2, P3, P4: TVector3): TCubicBezier3; inline;
begin
  Result[0] := P1;
  Result[1] := P2;
  Result[2] := P3;
  Result[3] := P4;
end;

function CubicBezier4(const P1, P2, P3, P4: TVector4): TCubicBezier4; inline;
begin
  Result[0] := P1;
  Result[1] := P2;
  Result[2] := P3;
  Result[3] := P4;
end;

function Vector3f(X: TVectorFloat; Y: TVectorFloat; Z: TVectorFloat): TVector3;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function Vector4f(Vec3: TVector3; W: TVectorFloat): TVector4;
begin
  Result.Vec3 := Vec3;
  Result.W := W;
end;

function Vector4f(X: TVectorFloat; Y: TVectorFloat; Z: TVectorFloat; W: TVectorFloat): TVector4;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
end;

function FormatVector(Vec3: TVector3): String;
begin
  Result := Format('vec3(%.3f, %.3f, %.3f)', [Vec3.X, Vec3.Y, Vec3.Z]);
end;

function FormatVector(Vec4: TVector4): String;
begin
  Result := Format('vec4(%.3f, %.3f, %.3f, %.3f)', [Vec4.X, Vec4.Y, Vec4.Z, Vec4.W]);
end;

end.

