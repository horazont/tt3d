unit Geometry;
(**********************************************************************
File name: geometry.pas
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
  Classes, SysUtils, math;

const
  Pi2 = Pi * 2;

type
  TVectorFloat = Double;

  TVector2_Array = array [0..1] of TVectorFloat;
  TVector2 = record
  case Byte of
    0: (X, Y: TVectorFloat);
    1: (AsArray: TVector2_Array);
    2: (R, A: TVectorFloat);
    3: (S, T: TVectorFloat);
    4: (Cos, Sin: TVectorFloat);
  end;
  PVector2 = ^TVector2;
  TBounds2 = record
    Min, Max: TVector2;
  end;

  TVector3_Array = array [0..2] of TVectorFloat;
  TVector3 = record
  case Byte of
    0: (X, Y, Z: TVectorFloat);
    1: (AsArray: TVector3_Array);
    2: (R, G, B: TVectorFloat);
    3: (Vec2: TVector2);
  end;
  PVector3 = ^TVector3;
  TTriangle3 = array [0..2] of TVector3;
  PTriangle3 = ^TTriangle3;

  TMatrix3 = array [0..8] of TVectorFloat;
  TMatrix4 = array [0..15] of TVectorFloat;
  TMatrix4f = array [0..15] of Single;

  TVector4_Array = array [0..3] of TVectorFloat;
  TVector4_Length = type TVector4_Array;
  TVector4 = record
  case Byte of
    0: (X, Y, Z, W: TVectorFloat);
    1: (AsArray: TVector4_Array);
    2: (R, G, B, A: TVectorFloat);
    3: (Vec3: TVector3);
    4: (Vec2: TVector2);
  end;
  PVector4 = ^TVector4;
  TTriangle4 = array [0..2] of TVector4;
  PTriangle4 = ^TTriangle4;

  TCubicBezier1 = array [0..3] of TVectorFloat;
  TCubicBezier2 = array [0..3] of TVector2;
  TCubicBezier3 = array [0..3] of TVector3;
  TCubicBezier4 = array [0..3] of TVector4;

  TVector2f = array [0..1] of Single;
  PVector2f = ^TVector2f;
  TVector3f = array [0..2] of Single;
  PVector3f = ^TVector3f;
  TVector4f = array [0..3] of Single;
  PVector4f = ^TVector4f;

  TVector2ub = packed array [0..1] of Byte;
  PVector2ub = ^TVector2ub;
  TVector3ub = packed array [0..2] of Byte;
  PVector3ub = ^TVector3ub;

  TVector2s = array [0..1] of SmallInt;
  PVector2s = ^TVector2s;

  TTriangle3f = array [0..2] of TVector3f;

operator + (A, B: TVector2): TVector2; inline;
operator + (A, B: TVector3): TVector3; inline;
operator + (A, B: TVector4): TVector4; inline;
operator + (A: TVector3; B: TVectorFloat): TVector3; inline;

operator - (A, B: TVector2): TVector2; inline;
operator - (A, B: TVector3): TVector3; inline;
operator - (A, B: TVector4): TVector4; inline;
operator - (A: TVector2): TVector2; inline;
operator - (A: TVector3): TVector3; inline;
operator - (A: TVector4): TVector4; inline;

operator * (A, B: TVector2): TVectorFloat; inline;
operator * (A, B: TVector3): TVectorFloat; inline;
operator * (A, B: TVector4): TVectorFloat; inline;

operator * (A: TVector2; B: TVectorFloat): TVector2; inline;
operator * (A: TVector3; B: TVectorFloat): TVector3; inline;
operator * (A: TVector4; B: TVectorFloat): TVector4; inline;
operator * (A: TVectorFloat; B: TVector2): TVector2; inline;
operator * (A: TVectorFloat; B: TVector3): TVector3; inline;
operator * (A: TVectorFloat; B: TVector4): TVector4; inline;

operator * (A: TMatrix3; B: TVector3): TVector3; inline;
operator * (A: TMatrix4; B: TVector4): TVector4; inline;
operator * (A, B: TMatrix4): TMatrix4; inline;
operator * (A, B: TMatrix4f): TMatrix4f; inline;

operator ** (A, B: TVector3): TVector3; inline;
operator ** (A: TCubicBezier1; B: TVectorFloat): TVectorFloat; inline;
operator ** (A: TCubicBezier2; B: TVectorFloat): TVector2; inline;
operator ** (A: TCubicBezier3; B: TVectorFloat): TVector3; inline;
operator ** (A: TCubicBezier4; B: TVectorFloat): TVector4; inline;
// operator ** (A, B: TVector4f): TVector4f;

operator / (A: TVector2; B: TVectorFloat): TVector2; inline;
operator / (A: TVector3; B: TVectorFloat): TVector3; inline;
operator / (A: TVector4; B: TVectorFloat): TVector4; inline;

operator := (A: TVector2): TVector2f; inline;
operator := (A: TVector2f): TVector2; inline;
operator := (A: TVector3): TVector3f; inline;
operator := (A: TVector3f): TVector3; inline;
operator := (A: TVector4): TVector4f; inline;
operator := (A: TVector4f): TVector4; inline;
operator := (A: TMatrix4): TMatrix4f; inline;

operator := (A: TVector2): TVector2ub; inline;
operator := (A: TVector2ub): TVector2; inline;
operator := (A: TVector3): TVector3ub; inline;
operator := (A: TVector3ub): TVector3; inline;

operator = (A, B: TVector2): Boolean; inline;
operator = (A, B: TVector3): Boolean; inline;
operator = (A, B: TVector4): Boolean; inline;

function Normalize(const Vec2: TVector2): TVector2; inline;
function Normalize(const Vec3: TVector3): TVector3; inline;
function Normalize(const Vec3f: TVector3f): TVector3f; inline;
function Normalize(const Vec4: TVector4): TVector4; inline;
function NormalizeInPlace(var Vec2: TVector2): TVectorFloat; inline;
function NormalizeInPlace(var Vec3: TVector3): TVectorFloat; inline;
function NormalizeInPlace(var Vec3f: TVector3f): TVectorFloat; inline;
function NormalizeInPlace(var Vec4: TVector4): TVectorFloat; inline;

function VLength(Vec2: TVector2): TVectorFloat; inline;
function VLength(Vec3: TVector3): TVectorFloat; inline;
function VLength(Vec3f: TVector3f): TVectorFloat; inline;
function VLength(Vec4: TVector4): TVectorFloat; inline;

function BLength(Bezier: TCubicBezier1): TVectorFloat;
function BLength(Bezier: TCubicBezier3): TVectorFloat;
function BLengthNumeric(Bezier: TCubicBezier3; Steps: Integer = 1000): TVectorFloat;
function BLengthEx(Bezier: TCubicBezier3; Accuracy: TVectorFloat): TVectorFloat;
function BLengthAutoAccuracy(Bezier: TCubicBezier3): TVectorFloat; inline;
function BLengthAuto(Bezier: TCubicBezier3): TVectorFloat; inline;
function BLength(Bezier: TCubicBezier4; Steps: Integer = 1000): TVectorFloat;

function CubicBezier1(const P1, P2, P3, P4: TVectorFloat): TCubicBezier1; inline;
function CubicBezier3(const P1, P2, P3, P4: TVector3): TCubicBezier3; inline;
function CubicBezier4(const P1, P2, P3, P4: TVector4): TCubicBezier4; inline;
function Vector2(X: TVectorFloat; Y: TVectorFloat): TVector2; inline;
function Vector3(X: TVectorFloat; Y: TVectorFloat; Z: TVectorFloat): TVector3; inline;
function Vector3(Vec2: TVector2; Z: TVectorFloat): TVector3; inline;
function Vector3ub(Vec3: TVector3): TVector3ub; inline;
function Vector3f(Vec3: TVector3): TVector3f; inline;
function Vector3(Vec3: TVector3f): TVector3; inline;
function Vector3f(X, Y, Z: Single): TVector3f; inline;
function Vector4(Vec2: TVector2; Z, W: TVectorFloat): TVector4; inline;
function Vector4(Vec3: TVector3; W: TVectorFloat): TVector4; inline;
function Vector4(X, Y, Z, W: TVectorFloat): TVector4; inline;
function Vector4(Vec4: TVector4f): TVector4; inline;
function Vector4f(Vec4: TVector4): TVector4f; inline;
function Vector4f(X, Y, Z: Single; W: Single = 1.0): TVector4f; inline;
function Vector4f(Vec3f: TVector3f; W: Single = 1.0): TVector4f; inline;
function Matrix4f(C11, C12, C13, C14, C21, C22, C23, C24, C31, C32, C33, C34, C41, C42, C43, C44: Single): TMatrix4f; inline;

function RotationMatrix(Axis: TVector3; Angle: TVectorFloat): TMatrix3;
function RotationMatrixX(Angle: TVectorFloat): TMatrix4;
function RotationMatrixY(Angle: TVectorFloat): TMatrix4;
function RotationMatrixZ(Angle: TVectorFloat): TMatrix4;
function TranslationMatrix(const V: TVector3): TMatrix4; inline;
function ScaleMatrix(const V: TVector3): TMatrix4; inline;

function Intersection(const AOrigin, ADirection, BOrigin, BDirection: TVector2): TVector2;
function Intersection(const ADirection, BDirection, BOffset: TVector2): TVector2;
function IntersectionPlane(const AOrigin, ADirection, BNormal: TVector3): TVector3;

function VecToAngle(Vec2: TVector2): TVectorFloat; inline;
function AngleToVec(a: TVectorFloat): TVector2; inline;

function FormatVector(Vec2: TVector2): String; inline;
function FormatVector(Vec3: TVector3): String; inline;
function FormatVector(Vec3: TVector3f): String; inline;
function FormatVector(Vec4: TVector4): String; inline;
function FormatVector(Vec4: TVector4f): String; inline;
function FormatMatrix(Mat4: TMatrix4): String; inline;
function FormatMatrix(Mat4: TMatrix4f): String; inline;

const
  V_EX : TVector3 = (X: 1.0; Y: 0.0; Z: 0.0);
  V_EY : TVector3 = (X: 0.0; Y: 1.0; Z: 0.0);
  V_EZ : TVector3 = (X: 0.0; Y: 0.0; Z: 1.0);

  V_EUP : TVector3 = (X: 0.0; Y: 0.0; Z: 1.0);

const
  IdentityMatrix3 : TMatrix3 = (1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);
  IdentityMatrix4f : TMatrix4f = (1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0);
  IdentityMatrix4 : TMatrix4 = (1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0);

implementation

operator + (A, B: TVector2): TVector2; inline;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

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

operator + (A: TVector3; B: TVectorFloat): TVector3;
begin
  Result.X := A.X + B;
  Result.Y := A.Y + B;
  Result.Z := A.Z + B;
end;

operator - (A, B: TVector2): TVector2; inline;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
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

operator - (A: TVector2): TVector2; inline;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
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

operator * (A, B: TVector2): TVectorFloat; inline;
begin
  Result := A.X * B.X +
            A.Y * B.Y;
end;

operator * (A, B: TVector3): TVectorFloat;
begin
  Result := A.X * B.X +
            A.Y * B.Y +
            A.Z * B.Z;
end;

operator * (A, B: TVector4): TVectorFloat;
begin
  Result := A.X * B.X +
            A.Y * B.Y +
            A.Z * B.Z +
            A.W * B.W;
end;

operator * (A: TVector2; B: TVectorFloat): TVector2; inline;
begin
  Result.X := A.X * B;
  Result.Y := A.Y * B;
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

operator * (A: TVectorFloat; B: TVector2): TVector2; inline;
begin
  Result := B * A;
end;

operator * (A: TVectorFloat; B: TVector3): TVector3;
begin
  Result := B * A;
end;

operator * (A: TVectorFloat; B: TVector4): TVector4;
begin
  Result := B * A;
end;

operator * (A: TMatrix3; B: TVector3): TVector3; inline;
begin
  Result.X := A[0] * B.X + A[3] * B.Y + A[6] * B.Z;
  Result.Y := A[1] * B.X + A[4] * B.Y + A[7] * B.Z;
  Result.Z := A[2] * B.X + A[5] * B.Z + A[8] * B.Z;
end;

operator * (A: TMatrix4; B: TVector4): TVector4; inline;
begin
  Result.X := A[0] * B.X + A[4] * B.Y + A[8] * B.Z + A[12] * B.W;
  Result.Y := A[1] * B.X + A[5] * B.Y + A[9] * B.Z + A[13] * B.W;
  Result.Z := A[2] * B.X + A[6] * B.Z + A[10] * B.Z + A[14] * B.W;
  Result.W := A[3] * B.X + A[7] * B.Z + A[11] * B.Z + A[15] * B.W;
end;

operator * (A, B: TMatrix4): TMatrix4;
begin
  Result[0] := A[0] * B[0] + A[4] * B[1] + A[8] * B[2] + A[12] * B[3];
  Result[1] := A[1] * B[0] + A[5] * B[1] + A[9] * B[2] + A[13] * B[3];
  Result[2] := A[2] * B[0] + A[6] * B[1] + A[10] * B[2] + A[14] * B[3];
  Result[3] := A[3] * B[0] + A[7] * B[1] + A[11] * B[2] + A[15] * B[3];

  Result[4] := A[0] * B[4] + A[4] * B[5] + A[8] * B[6] + A[12] * B[7];
  Result[5] := A[1] * B[4] + A[5] * B[5] + A[9] * B[6] + A[13] * B[7];
  Result[6] := A[2] * B[4] + A[6] * B[5] + A[10] * B[6] + A[14] * B[7];
  Result[7] := A[3] * B[4] + A[7] * B[5] + A[11] * B[6] + A[15] * B[7];

  Result[8] := A[0] * B[8] + A[4] * B[9] + A[8] * B[10] + A[12] * B[11];
  Result[9] := A[1] * B[8] + A[5] * B[9] + A[9] * B[10] + A[13] * B[11];
  Result[10] := A[2] * B[8] + A[6] * B[9] + A[10] * B[10] + A[14] * B[11];
  Result[11] := A[3] * B[8] + A[7] * B[9] + A[11] * B[10] + A[15] * B[11];

  Result[12] := A[0] * B[12] + A[4] * B[13] + A[8] * B[14] + A[12] * B[15];
  Result[13] := A[1] * B[12] + A[5] * B[13] + A[9] * B[14] + A[13] * B[15];
  Result[14] := A[2] * B[12] + A[6] * B[13] + A[10] * B[14] + A[14] * B[15];
  Result[15] := A[3] * B[12] + A[7] * B[13] + A[11] * B[14] + A[15] * B[15];
end;

operator * (A, B: TMatrix4f): TMatrix4f;
begin
  Result[0] := A[0] * B[0] + A[4] * B[1] + A[8] * B[2] + A[12] * B[3];
  Result[1] := A[1] * B[0] + A[5] * B[1] + A[9] * B[2] + A[13] * B[3];
  Result[2] := A[2] * B[0] + A[6] * B[1] + A[10] * B[2] + A[14] * B[3];
  Result[3] := A[3] * B[0] + A[7] * B[1] + A[11] * B[2] + A[15] * B[3];

  Result[4] := A[0] * B[4] + A[4] * B[5] + A[8] * B[6] + A[12] * B[7];
  Result[5] := A[1] * B[4] + A[5] * B[5] + A[9] * B[6] + A[13] * B[7];
  Result[6] := A[2] * B[4] + A[6] * B[5] + A[10] * B[6] + A[14] * B[7];
  Result[7] := A[3] * B[4] + A[7] * B[5] + A[11] * B[6] + A[15] * B[7];

  Result[8] := A[0] * B[8] + A[4] * B[9] + A[8] * B[10] + A[12] * B[11];
  Result[9] := A[1] * B[8] + A[5] * B[9] + A[9] * B[10] + A[13] * B[11];
  Result[10] := A[2] * B[8] + A[6] * B[9] + A[10] * B[10] + A[14] * B[11];
  Result[11] := A[3] * B[8] + A[7] * B[9] + A[11] * B[10] + A[15] * B[11];

  Result[12] := A[0] * B[12] + A[4] * B[13] + A[8] * B[14] + A[12] * B[15];
  Result[13] := A[1] * B[12] + A[5] * B[13] + A[9] * B[14] + A[13] * B[15];
  Result[14] := A[2] * B[12] + A[6] * B[13] + A[10] * B[14] + A[14] * B[15];
  Result[15] := A[3] * B[12] + A[7] * B[13] + A[11] * B[14] + A[15] * B[15];
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

operator ** (A: TCubicBezier2; B: TVectorFloat): TVector2; inline;
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
  Result := Vector4();
end;*)

operator / (A: TVector2; B: TVectorFloat): TVector2; inline;
begin
  Result.X := A.X / B;
  Result.Y := A.Y / B;
end;

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

operator := (A: TVector2): TVector2f; inline;
begin
  Result[0] := A.X;
  Result[1] := A.Y;
end;

operator := (A: TVector2f): TVector2; inline;
begin
  Result.X := A[0];
  Result.Y := A[1];
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

operator := (A: TVector2): TVector2ub;
begin
  Result[0] := Min(Max(Round((A.X / 2. + 0.5) * 255), 0), 255);
  Result[1] := Min(Max(Round((A.Y / 2. + 0.5) * 255), 0), 255);
end;

operator := (A: TVector2ub): TVector2;
begin
  Result.X := (A[0] / 255) * 2 - 1.0;
  Result.Y := (A[1] / 255) * 2 - 1.0;
end;

operator := (A: TVector3): TVector3ub;
begin
  Result[0] := Min(Max(Round((A.X / 2. + 0.5) * 255), 0), 255);
  Result[1] := Min(Max(Round((A.Y / 2. + 0.5) * 255), 0), 255);
  Result[2] := Min(Max(Round((A.Z / 2. + 0.5) * 255), 0), 255);
end;

operator := (A: TVector3ub): TVector3;
begin
  Result.X := (A[0] / 255) * 2 - 1.0;
  Result.Y := (A[1] / 255) * 2 - 1.0;
  Result.Z := (A[2] / 255) * 2 - 1.0;
end;

operator:=(A: TMatrix4): TMatrix4f;
begin
  Result[0] := A[0];
  Result[1] := A[1];
  Result[2] := A[2];
  Result[3] := A[3];
  Result[4] := A[4];
  Result[5] := A[5];
  Result[6] := A[6];
  Result[7] := A[7];
  Result[8] := A[8];
  Result[9] := A[9];
  Result[10] := A[10];
  Result[11] := A[11];
  Result[12] := A[12];
  Result[13] := A[13];
  Result[14] := A[14];
  Result[15] := A[15];
end;

operator = (A, B: TVector2): Boolean; inline;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

operator = (A, B: TVector3): Boolean; inline;
begin
  Result := (A.Vec2 = B.Vec2) and (A.Z = B.Z);
end;

operator = (A, B: TVector4): Boolean; inline;
begin
  Result := (A.Vec3 = B.Vec3) and (A.W = B.W);
end;

function Normalize(const Vec2: TVector2): TVector2; inline;
begin
  Result := Vec2;
  NormalizeInPlace(Result);
end;

function Normalize(const Vec3: TVector3): TVector3;
begin
  Result := Vec3;
  NormalizeInPlace(Result);
end;

function Normalize(const Vec3f: TVector3f): TVector3f;
begin
  Result := Vec3f;
  NormalizeInPlace(Result);
end;

function Normalize(const Vec4: TVector4): TVector4;
begin
  Result := Vec4;
  NormalizeInPlace(Result);
end;

function NormalizeInPlace(var Vec2: TVector2): TVectorFloat; inline;
begin
  Result := VLength(Vec2);
  Vec2.X := Vec2.X / Result;
  Vec2.Y := Vec2.Y / Result;
end;

function NormalizeInPlace(var Vec3: TVector3): TVectorFloat;
begin
  Result := VLength(Vec3);
  Vec3.X := Vec3.X / Result;
  Vec3.Y := Vec3.Y / Result;
  Vec3.Z := Vec3.Z / Result;
end;

function NormalizeInPlace(var Vec3f: TVector3f): TVectorFloat;
begin
  Result := VLength(Vec3f);
  Vec3f[0] := Vec3f[0] / Result;
  Vec3f[1] := Vec3f[1] / Result;
  Vec3f[2] := Vec3f[2] / Result;
end;

function NormalizeInPlace(var Vec4: TVector4): TVectorFloat;
begin
  Result := VLength(Vec4);
  Vec4.X := Vec4.X / Result;
  Vec4.Y := Vec4.Y / Result;
  Vec4.Z := Vec4.Z / Result;
  Vec4.W := Vec4.W / Result;
end;

function VLength(Vec2: TVector2): TVectorFloat; inline;
begin
  Result := Sqrt(Sqr(Vec2.X) + Sqr(Vec2.Y));
end;

function VLength(Vec3: TVector3): TVectorFloat;
begin
  Result := Sqrt(Sqr(Vec3.X) + Sqr(Vec3.Y) + Sqr(Vec3.Z));
end;

function VLength(Vec3f: TVector3f): TVectorFloat;
begin
  Result := Sqrt(Sqr(Vec3f[0]) + Sqr(Vec3f[1]) + Sqr(Vec3f[2]));
end;

function VLength(Vec4: TVector4): TVectorFloat;
begin
  Result := Sqrt(Sqr(Vec4.X) + Sqr(Vec4.Y) + Sqr(Vec4.Z) + Sqr(Vec4.W));
end;

function BLength(Bezier: TCubicBezier1): TVectorFloat;
begin
  Result := Abs(-Bezier[0]+3*Bezier[1]-3*Bezier[2]+Bezier[3])+
      Abs(3*Bezier[0]-6*Bezier[1]+3*Bezier[2])+
      Abs(-3*Bezier[0]+3*Bezier[1]);
end;

function BLength(Bezier: TCubicBezier3): TVectorFloat;
begin
  Result := VLength(-Bezier[0]+3*Bezier[1]-3*Bezier[2]+Bezier[3])+
      VLength(3*Bezier[0]-6*Bezier[1]+3*Bezier[2])+
      VLength(-3*Bezier[0]+3*Bezier[1]);
end;

function BLengthNumeric(Bezier: TCubicBezier3; Steps: Integer): TVectorFloat;
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

function BLengthEx(Bezier: TCubicBezier3; Accuracy: TVectorFloat): TVectorFloat;
var
  t, tprev, tstep, l: TVectorFloat;
  Current, Prev: TVector3;
begin
  Result := 0.0;
  tprev := 0.0;
  tstep := Accuracy;
  if tstep >= 1.0 then
    tstep := 0.9;
  t := tstep;
  Prev := Bezier ** 0.0;
  while t < 1.0 do
  begin
    Current := Bezier ** t;
    l := VLength(Current - Prev);
    if l > Accuracy then
    begin
      tstep /= 2;
      t := tprev + tstep;
//      WriteLn('decreasing step size');
      Continue;
    end;
    Result += l;
    Prev := Current;
    if l*1.5 < Accuracy then
    begin
      tstep *= 2;
//      WriteLn('increasing step size');
    end;
    tprev := t;
    t += tstep;
  end;
end;

function BLengthAutoAccuracy(Bezier: TCubicBezier3): TVectorFloat; inline;
begin
  Result := Power(10,
      Trunc(
        log10(
          VLength(Bezier[0] - Bezier[1]) +
          VLength(Bezier[1] - Bezier[2]) +
          VLength(Bezier[2] - Bezier[3])
         )
      )-3);
end;

function BLengthAuto(Bezier: TCubicBezier3): TVectorFloat;
begin
  Result := BLengthEx(Bezier,
      BLengthAutoAccuracy(Bezier)
    );
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

function Vector2(X: TVectorFloat; Y: TVectorFloat): TVector2; inline;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Vector3(X: TVectorFloat; Y: TVectorFloat; Z: TVectorFloat): TVector3;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function Vector3(Vec2: TVector2; Z: TVectorFloat): TVector3; inline;
begin
  Result.Vec2 := Vec2;
  Result.Z := Z;
end;

function Vector3ub(Vec3: TVector3): TVector3ub; inline;
begin
  Result := Vec3;
end;

function Vector3f(Vec3: TVector3): TVector3f; inline;
begin
  Result[0] := Vec3.X;
  Result[1] := Vec3.Y;
  Result[2] := Vec3.Z;
end;

function Vector3(Vec3: TVector3f): TVector3;
begin
  Result.X := Vec3[0];
  Result.Y := Vec3[1];
  Result.Z := Vec3[2];
end;

function Vector3f(X, Y, Z: Single): TVector3f; inline;
begin
  Result[0] := X;
  Result[1] := Y;
  Result[2] := Z;
end;

function Vector4(Vec2: TVector2; Z, W: TVectorFloat): TVector4; inline;
begin
  Result.Vec2 := Vec2;
  Result.Z := Z;
  Result.W := W;
end;

function Vector4(Vec3: TVector3; W: TVectorFloat): TVector4;
begin
  Result.Vec3 := Vec3;
  Result.W := W;
end;

function Vector4(X, Y, Z, W: TVectorFloat): TVector4; inline;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
end;

function Vector4(Vec4: TVector4f): TVector4;
begin
  Result.X := Vec4[0];
  Result.Y := Vec4[1];
  Result.Z := Vec4[2];
  Result.W := Vec4[3];
end;

function Vector4f(Vec4: TVector4): TVector4f; inline;
begin
  Result[0] := Vec4.X;
  Result[1] := Vec4.Y;
  Result[2] := Vec4.Z;
  Result[3] := Vec4.W;
end;

function Vector4f(X, Y, Z: Single; W: Single): TVector4f; inline;
begin
  Result[0] := X;
  Result[1] := Y;
  Result[2] := Z;
  Result[3] := W;
end;

function Vector4f(Vec3f: TVector3f; W: Single): TVector4f; inline;
begin
  Result[0] := Vec3f[0];
  Result[1] := Vec3f[1];
  Result[2] := Vec3f[2];
  Result[3] := W;
end;

function Matrix4f(C11, C12, C13, C14, C21, C22, C23, C24, C31, C32, C33, C34,
  C41, C42, C43, C44: Single): TMatrix4f;
begin
  Result[0] := C11;
  Result[1] := C12;
  Result[2] := C13;
  Result[3] := C14;

  Result[4] := C21;
  Result[5] := C22;
  Result[6] := C23;
  Result[7] := C24;

  Result[8] := C31;
  Result[9] := C32;
  Result[10] := C33;
  Result[11] := C34;

  Result[12] := C41;
  Result[13] := C42;
  Result[14] := C43;
  Result[15] := C44;
end;

function Matrix4f(Components: TMatrix4f): TMatrix4f;
begin
  Result := Components;
end;

function RotationMatrix(Axis: TVector3; Angle: TVectorFloat): TMatrix3;
var
  RawSin, RawCos: float;
  Sine, Cosine, OneMinusCosine: TVectorFloat;
  Len: TVectorFloat;
begin
  sincos(Angle, RawSin, RawCos);
  Sine := RawSin;
  Cosine := RawCos;
  OneMinusCosine := 1-RawCos;


  Len := NormalizeInPlace(Axis);

  if Len = 0.0 then
    Exit(IdentityMatrix3)
  else
  begin
    Result[0] := (OneMinusCosine * Sqr(Axis.X)) + Cosine;
    Result[1] := (OneMinusCosine * Axis.X * Axis.Y) - (Axis.Z * Sine);
    Result[2] := (OneMinusCosine * Axis.Z * Axis.X) + (Axis.Y * Sine);

    Result[3] := (OneMinusCosine * Axis.X * Axis.Y) + (Axis.Z * Sine);
    Result[4] := (OneMinusCosine * Sqr(Axis.Y)) + Cosine;
    Result[5] := (OneMinusCosine * Axis.Y * Axis.Z) - (Axis.X * Sine);

    Result[6] := (OneMinusCosine * Axis.Z * Axis.X) - (Axis.Y * Sine);
    Result[7] := (OneMinusCosine * Axis.Y * Axis.Z) + (Axis.X * Sine);
    Result[8] := (OneMinusCosine * Sqr(Axis.Z)) + Cosine;
  end;
end;
  
function RotationMatrixX(Angle: TVectorFloat): TMatrix4;
var
  sr, cr: float;
  S, C: TVectorFloat;
begin
  sincos(Angle, sr, cr);
  S := Sin(Angle);
  C := Cos(Angle);
  Result := IdentityMatrix4;
  Result[5] := C;
  Result[6] := S;
  Result[9] := -S;
  Result[10] := C;
end;

function RotationMatrixY(Angle: TVectorFloat): TMatrix4;
var
  sr, cr: float;
  S, C: TVectorFloat;
begin
  sincos(Angle, sr, cr);
  S := sr;
  C := cr;
  Result := IdentityMatrix4;
  Result[0] := C;
  Result[2] := -S;
  Result[8] := S;
  Result[10] := C;
end;

function RotationMatrixZ(Angle: TVectorFloat): TMatrix4;
var
  sr, cr: float;
  S, C: TVectorFloat;
begin
  sincos(Angle, sr, cr);
  S := sr;
  C := cr;
  Result := IdentityMatrix4;
  Result[0] := C;
  Result[1] := S;
  Result[4] := -S;
  Result[5] := C;
end;

function TranslationMatrix(const V: TVector3): TMatrix4;
begin
  Result := IdentityMatrix4;
  Result[12] := V.X;
  Result[13] := V.Y;
  Result[14] := V.Z;
end;

function ScaleMatrix(const V: TVector3): TMatrix4;
begin
  Result := IdentityMatrix4;
  Result[0] := V.X;
  Result[5] := V.Y;
  Result[10] := V.Z;
end;

function Intersection(const AOrigin, ADirection, BOrigin, BDirection: TVector2
  ): TVector2;
begin
  if AOrigin = BOrigin then
    Exit(AOrigin);
  Exit(Intersection(ADirection, BDirection, BOrigin - AOrigin));
end;

function Intersection(const ADirection, BDirection, BOffset: TVector2
  ): TVector2;
var
  s: TVectorFloat;
begin
  {$ifopt R+}
  {$define WasR}
  {$R-}
  {$endif}
  {$ifopt Q+}
  {$define WasQ}
  {$Q-}
  {$endif}
  if (ADirection = BDirection) or (ADirection = -BDirection) then
    Exit(Vector2(NaN, NaN));
  {$ifdef WasQ}
  {$undef WasQ}
  {$Q-}
  {$endif}
  {$ifdef WasR}
  {$undef WasR}
  {$R+}
  {$endif}
  s := -(ADirection.X * BOffset.Y - ADirection.Y * BOffset.Y) / (ADirection.X * BDirection.Y - ADirection.Y * BDirection.X);
  Exit(BOffset + BDirection * s);
end;

function IntersectionPlane(const AOrigin, ADirection, BNormal: TVector3
  ): TVector3;
var
  t: TVectorFloat;
begin
  {$ifopt R+}
  {$define WasR}
  {$R-}
  {$endif}
  {$ifopt Q+}
  {$define WasQ}
  {$Q-}
  {$endif}
  if (ADirection * BNormal = 0) then
    Exit(Vector3(NaN, NaN, NaN));
  {$ifdef WasQ}
  {$undef WasQ}
  {$Q-}
  {$endif}
  {$ifdef WasR}
  {$undef WasR}
  {$R+}
  {$endif}
  t := (AOrigin * BNormal) / (ADirection * BNormal);
  Exit(AOrigin + ADirection * t);
end;

function VecToAngle(Vec2: TVector2): TVectorFloat; inline;
begin
  if Vec2.Y > 0.0 then
  begin
    Exit(arccos(Vec2.X));
  end
  else
  begin
    (*Result := arccos(-Vec2.X) + Pi;
    if SameValue(Result, 2*Pi, 0) then
      Exit(0.0);*)
    Exit(arccos(-Vec2.X) + Pi);
  end;
end;

function AngleToVec(a: TVectorFloat): TVector2; inline;
begin
  Result.X := cos(a);
  Result.Y := sin(a);
end;

function FormatVector(Vec2: TVector2): String; inline;
begin
  Result := Format('vec2(%.3f, %.3f)', [Vec2.X, Vec2.Y]);
end;

function FormatVector(Vec3: TVector3): String;
begin
  Result := Format('vec3(%.3f, %.3f, %.3f)', [Vec3.X, Vec3.Y, Vec3.Z]);
end;

function FormatVector(Vec3: TVector3f): String; inline;
begin
  Result := Format('vec3(%.3f, %.3f, %.3f)', [Vec3[0], Vec3[1], Vec3[2]]);
end;

function FormatVector(Vec4: TVector4): String;
begin
  Result := Format('vec4(%.3f, %.3f, %.3f, %.3f)', [Vec4.X, Vec4.Y, Vec4.Z, Vec4.W]);
end;

function FormatVector(Vec4: TVector4f): String; inline;
begin
  Result := Format('vec4(%.3f, %.3f, %.3f, %.3f)', [Vec4[0], Vec4[1], Vec4[2], Vec4[3]]);
end;

function FormatMatrix(Mat4: TMatrix4): String;
begin
  Result := Format('mat4(%6.3f, %6.3f, %6.3f, %6.3f)'+LineEnding+'    (%6.3f, %6.3f, %6.3f, %6.3f)'+LineEnding+'    (%6.3f, %6.3f, %6.3f, %6.3f)'+LineEnding+'    (%6.3f, %6.3f, %6.3f, %6.3f)', [Mat4[0], Mat4[4], Mat4[8], Mat4[12], Mat4[1], Mat4[5], Mat4[9], Mat4[13], Mat4[2], Mat4[6], Mat4[10], Mat4[14], Mat4[3], Mat4[7], Mat4[11], Mat4[15]]);
end;

function FormatMatrix(Mat4: TMatrix4f): String;
begin
  Result := Format('mat4(%6.3f, %6.3f, %6.3f, %6.3f)'+LineEnding+'    (%6.3f, %6.3f, %6.3f, %6.3f)'+LineEnding+'    (%6.3f, %6.3f, %6.3f, %6.3f)'+LineEnding+'    (%6.3f, %6.3f, %6.3f, %6.3f)', [Mat4[0], Mat4[4], Mat4[8], Mat4[12], Mat4[1], Mat4[5], Mat4[9], Mat4[13], Mat4[2], Mat4[6], Mat4[10], Mat4[14], Mat4[3], Mat4[7], Mat4[11], Mat4[15]]);
end;

end.

