unit Geometry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TVector3f_Array = array [0..2] of Single;
  TVector3f_Length = type TVector3f_Array;
  TVector3f = record
  case Byte of
    0: (X, Y, Z: Single);
    1: (AsArray: TVector3f_Array);
    2: (R, G, B: Single);
    3: (Length: TVector3f_Length);
  end;

  TVector4f_Array = array [0..3] of Single;
  TVector4f_Length = type TVector4f_Array;
  TVector4f = record
  case Byte of
    0: (X, Y, Z, W: Single);
    1: (AsArray: TVector4f_Array);
    2: (R, G, B, A: Single);
    3: (Vec3: TVector3f);
    4: (Length: TVector4f_Length);
  end;

operator + (A, B: TVector3f): TVector3f;
operator + (A, B: TVector4f): TVector4f;

operator - (A, B: TVector3f): TVector3f;
operator - (A, B: TVector4f): TVector4f;

operator * (A, B: TVector3f): Single;
operator * (A, B: TVector4f): Single;

operator * (A: TVector3f; B: Single): TVector3f;
operator * (A: TVector4f; B: Single): TVector4f;

operator ** (A, B: TVector3f): TVector3f;
// operator ** (A, B: TVector4f): TVector4f;

operator / (A: TVector3f; B: Single): TVector3f;
operator / (A: TVector4f; B: Single): TVector4f;

operator := (A: TVector3f_Length): Single;
operator := (A: TVector4f_Length): Single;

operator not (A: TVector3f): TVector3f;
operator not (A: TVector4f): TVector4f;

function Normalize(const Vec3: TVector3f): TVector3f;
function Normalize(const Vec4: TVector4f): TVector4f;
procedure NormalizeInPlace(var Vec3: TVector3f);
procedure NormalizeInPlace(var Vec4: TVector4f);

function VLength(Vec3: TVector3f): Single;
function VLength(Vec4: TVector4f): Single;

function Vector3f(X: Single; Y: Single; Z: Single): TVector3f;
function Vector4f(Vec3: TVector3f; W: Single = 0.0): TVector4f;
function Vector4f(X: Single = 0.0; Y: Single = 0.0; Z: Single = 0.0; W: Single = 0.0): TVector4f;

function FormatVector(Vec3: TVector3f): String;
function FormatVector(Vec4: TVector4f): String;

implementation

operator + (A, B: TVector3f): TVector3f;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
end;

operator + (A, B: TVector4f): TVector4f;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
  Result.W := A.W + B.W;
end;

operator - (A, B: TVector3f): TVector3f;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
end;

operator - (A, B: TVector4f): TVector4f;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
  Result.W := A.W - B.W;
end;

operator * (A, B: TVector3f): Single;
begin
  Result := A.X * B.X +
            A.Y * B.Y +
            A.Z * B.Z;
end;

operator * (A, B: TVector4f): Single;
begin
  Result := A.X * B.X +
            A.Y * B.Y +
            A.Z * B.Z +
            A.W * B.W;
end;

operator * (A: TVector3f; B: Single): TVector3f;
begin
  Result.X := A.X * B;
  Result.Y := A.Y * B;
  Result.Z := A.Z * B;
end;

operator * (A: TVector4f; B: Single): TVector4f;
begin
  Result.X := A.X * B;
  Result.Y := A.Y * B;
  Result.Z := A.Z * B;
  Result.W := A.W * B;
end;

operator ** (A, B: TVector3f): TVector3f;
begin
  Result.X := A.Y * B.Z - A.Z * B.Y;
  Result.Y := A.Z * B.X - A.X * B.Z;
  Result.Z := A.X * B.Y - A.Y * B.X;
end;

(*operator ** (A, B: TVector4f): TVector4f;
begin
  Result := Vector4f();
end;*)

operator / (A: TVector3f; B: Single): TVector3f;
begin
  Result.X := A.X / B;
  Result.Y := A.Y / B;
  Result.Z := A.Z / B;
end;

operator / (A: TVector4f; B: Single): TVector4f;
begin
  Result.X := A.X / B;
  Result.Y := A.Y / B;
  Result.Z := A.Z / B;
  Result.W := A.W / B;
end;

operator := (A: TVector3f_Length): Single;
begin
  Result := Sqrt(Sqr(A[0]) + Sqr(A[1]) + Sqr(A[2]));
end;

operator := (A: TVector4f_Length): Single;
begin
  Result := Sqrt(Sqr(A[0]) + Sqr(A[1]) + Sqr(A[2]) + Sqr(A[3]));
end;

operator not(A: TVector3f): TVector3f;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
  Result.Z := -A.Z;
end;

operator not(A: TVector4f): TVector4f;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
  Result.Z := -A.Z;
  Result.W := -A.W;
end;

function Normalize(const Vec3: TVector3f): TVector3f;
begin
  Result := Vec3;
  NormalizeInPlace(Result);
end;

function Normalize(const Vec4: TVector4f): TVector4f;
begin
  Result := Vec4;
  NormalizeInPlace(Result);
end;

procedure NormalizeInPlace(var Vec3: TVector3f);
var
  Len: Single;
begin
  Len := Vec3.Length;
  Vec3.X := Vec3.X / Len;
  Vec3.Y := Vec3.Y / Len;
  Vec3.Z := Vec3.Z / Len;
end;

procedure NormalizeInPlace(var Vec4: TVector4f);
var
  Len: Single;
begin
  Len := Vec4.Length;
  Vec4.X := Vec4.X / Len;
  Vec4.Y := Vec4.Y / Len;
  Vec4.Z := Vec4.Z / Len;
end;

function VLength(Vec3: TVector3f): Single;
begin
  Result := Vec3.Length;
end;

function VLength(Vec4: TVector4f): Single;
begin
  Result := Vec4.Length;
end;

function Vector3f(X: Single; Y: Single; Z: Single): TVector3f;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function Vector4f(Vec3: TVector3f; W: Single): TVector4f;
begin
  Result.Vec3 := Vec3;
  Result.W := W;
end;

function Vector4f(X: Single; Y: Single; Z: Single; W: Single): TVector4f;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
end;

function FormatVector(Vec3: TVector3f): String;
begin
  Result := Format('vec3(%.3f, %.3f, %.3f)', [Vec3.X, Vec3.Y, Vec3.Z]);
end;

function FormatVector(Vec4: TVector4f): String;
begin
  Result := Format('vec4(%.3f, %.3f, %.3f, %.3f)', [Vec4.X, Vec4.Y, Vec4.Z, Vec4.W]);
end;

end.

