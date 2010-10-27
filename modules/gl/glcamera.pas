unit GLCamera;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dglOpenGL, Geometry, math;

const
  ACCEL_EPSILON = 0.001;
  VEL_EPSILON = 0.001;

type

  { TGLViewport }

  TGLViewport = class (TObject)
  public
    constructor Create;
  private
    FBottom: Integer;
    FLeft: Integer;
    FOnChange: TNotifyEvent;
    FRight: Integer;
    FTop: Integer;
    procedure SetBottom(const AValue: Integer);
    procedure SetLeft(const AValue: Integer);
    procedure SetRight(const AValue: Integer);
    procedure SetTop(const AValue: Integer);
  protected
    procedure DoChange;
  public
    procedure Assign(Source: TGLViewport);
    procedure SetAll(const ATop, ALeft, ABottom, ARight: Integer);
  public
    property Top: Integer read FTop write SetTop;
    property Left: Integer read FLeft write SetLeft;
    property Right: Integer read FRight write SetRight;
    property Bottom: Integer read FBottom write SetBottom;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TGLCamera }

  TGLCamera = class (TObject)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FModelViewInvalidated: Boolean;
    FProjectionInvalidated: Boolean;
    FViewport: TGLViewport;
    FOnMoved: TNotifyEvent;
  protected
    FModelView: TMatrix4f;
    FProjection: TMatrix4f;
  protected
    procedure DeinvalidateModelView;
    procedure DeinvalidateProjection;
    procedure DoMoved;
    procedure InvalidateModelView;
    procedure InvalidateProjection;
    procedure RecalculateModelView; virtual; abstract;
    procedure RecalculateProjection; virtual; abstract;
    procedure ViewportChanged(Sender: TObject); virtual;
  public
    procedure Load;
    procedure Mult;
    procedure Update(const TimeInterval: Double); virtual;
    procedure Validate;
  public
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
    property Viewport: TGLViewport read FViewport;
  end;

  { TGLCameraPerspective }

  TGLCameraPerspective = class (TGLCamera)
  private
    FFOV: TVectorFloat;
    FNearZ, FFarZ: TVectorFloat;
    procedure SetFarZ(const AValue: TVectorFloat);
    procedure SetFOV(const AValue: TVectorFloat);
    procedure SetNearZ(const AValue: TVectorFloat);
  protected
    procedure RecalculateProjection; override;
    procedure ViewportChanged(Sender: TObject); override;
  public
    property FOV: TVectorFloat read FFOV write SetFOV;
    property NearZ: TVectorFloat read FNearZ write SetNearZ;
    property FarZ: TVectorFloat read FFarZ write SetFarZ;
  end;

  { TGLCameraFree }

  TGLCameraFree = class (TGLCameraPerspective)
  public
    constructor Create;
  private
    FModelViewHiRes: TMatrix4;
    FInvModelViewHiRes: TMatrix4;
    FPos: TVector3;
    FRotation: TVector2;
    FZoom: TVectorFloat;
    FRight, FUp, FFront: TVector3;
    FFlatRight, FFlatFront: TVector2;
    FFlatRightFrontInvalidated: Boolean;
    FTransformedPos: TVector3;
    FTransformedPosInvalidated: Boolean;
    function GetFlatFront: TVector2;
    function GetFlatRight: TVector2;
    function GetTransformedPos: TVector3;
    procedure SetPos(const AValue: TVector3);
    procedure SetRotation(const AValue: TVector2);
    procedure SetZoom(const AValue: TVectorFloat);
  protected
    procedure DeinvalidateFlatRightFront;
    procedure DeinvalidateTransformedPos;
    procedure InvalidateFlatRightFront;
    procedure InvalidateTransformedPos;
    procedure RecalculateFlatRightFront; virtual;
    procedure RecalculateModelView; override;
    procedure RecalculateTransformedPos;
  public
    property Pos: TVector3 read FPos write SetPos;
    property Rotation: TVector2 read FRotation write SetRotation;
    property Zoom: TVectorFloat read FZoom write SetZoom;
    property Right: TVector3 read FRight;
    property Up: TVector3 read FUp;
    property Front: TVector3 read FFront;
    property FlatRight: TVector2 read GetFlatRight;
    property FlatFront: TVector2 read GetFlatFront;
    property TransformedPos: TVector3 read GetTransformedPos;
  end;

  { TGLCameraFreeSmooth }

  TGLCameraFreeSmooth = class (TGLCameraFree)
  public
    constructor Create;
  private
    FAcceleration: TVector2;
    FRotationAcceleration: TVector2;
    FRotationVelocity: TVector2;
    FVelocity: TVector2;
    FZoomAcceleration: TVectorFloat;
    FZoomVelocity: TVectorFloat;

    FMoving: Boolean;
    FMoveTarget: TVector2;
    FRotating: Boolean;
    FRotateTarget: TVector2;

    procedure SetAcceleration(const AValue: TVector2);
    procedure SetRotationAcceleration(const AValue: TVector2);
    procedure SetRotationVelocity(const AValue: TVector2);
    procedure SetVelocity(const AValue: TVector2);
    procedure SetZoomAcceleration(const AValue: TVectorFloat);
    procedure SetZoomVelocity(const AValue: TVectorFloat);
  public
    procedure Accelerate(const ABy: TVector2);
    procedure AccelerateRotation(const ABy: TVector2);
    procedure AccelerateZoom(const ABy: TVectorFloat);
    procedure IssueMoveTo(const ATarget: TVector2);
    procedure IssueRotateTo(const ATarget: TVector2);
    procedure StopMovement(ResetAcceleration: Boolean);
    procedure StopRotation(ResetAcceleration: Boolean);
    procedure StopZoom(ResetAcceleration: Boolean);
    procedure Update(const TimeInterval: Double); override;
  public
    property Acceleration: TVector2 read FAcceleration write SetAcceleration;
    property RotationAcceleration: TVector2 read FRotationAcceleration write SetRotationAcceleration;
    property RotationVelocity: TVector2 read FRotationVelocity write SetRotationVelocity;
    property Velocity: TVector2 read FVelocity write SetVelocity;
    property ZoomAcceleration: TVectorFloat read FZoomAcceleration write SetZoomAcceleration;
    property ZoomVelocity: TVectorFloat read FZoomVelocity write SetZoomVelocity;
  end;

implementation

{ TGLViewport }

constructor TGLViewport.Create;
begin
  FOnChange := nil;
end;

procedure TGLViewport.SetBottom(const AValue: Integer);
begin
  if FBottom=AValue then exit;
  FBottom:=AValue;
  DoChange;
end;

procedure TGLViewport.SetLeft(const AValue: Integer);
begin
  if FLeft=AValue then exit;
  FLeft:=AValue;
  DoChange;
end;

procedure TGLViewport.SetRight(const AValue: Integer);
begin
  if FRight=AValue then exit;
  FRight:=AValue;
  DoChange;
end;

procedure TGLViewport.SetTop(const AValue: Integer);
begin
  if FTop=AValue then exit;
  FTop:=AValue;
  DoChange;
end;

procedure TGLViewport.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TGLViewport.Assign(Source: TGLViewport);
begin
  SetAll(Source.Top, Source.Left, Source.Bottom, Source.Right);
end;

procedure TGLViewport.SetAll(const ATop, ALeft, ABottom, ARight: Integer);
begin
  if (FTop = ATop) and (FLeft = ALeft) and (FRight = ARight) and (FBottom = ABottom) then
    Exit;
  FTop := ATop;
  FLeft := ALeft;
  FRight := ARight;
  FBottom := ABottom;
  DoChange;
end;

{ TGLCamera }

constructor TGLCamera.Create;
begin
  FViewport := TGLViewport.Create;
  FModelView := IdentityMatrix4f;
  FProjection := IdentityMatrix4f;
  FViewport.OnChange := @ViewportChanged;
  InvalidateProjection;
  InvalidateModelView;
end;

destructor TGLCamera.Destroy;
begin
  FViewport.Free;
  inherited Destroy;
end;

procedure TGLCamera.DeinvalidateModelView;
begin
  FModelViewInvalidated := False;
end;

procedure TGLCamera.DeinvalidateProjection;
begin
  FProjectionInvalidated := False;
end;

procedure TGLCamera.DoMoved;
begin
  if Assigned(FOnMoved) then
    FOnMoved(Self);
end;

procedure TGLCamera.InvalidateModelView;
begin
  FModelViewInvalidated := True;
end;

procedure TGLCamera.InvalidateProjection;
begin
  FProjectionInvalidated := True;
end;

procedure TGLCamera.ViewportChanged(Sender: TObject);
begin

end;

procedure TGLCamera.Load;
begin
  if FProjectionInvalidated then
    RecalculateProjection;
  if FModelViewInvalidated then
    RecalculateModelView;
  glMatrixMode(GL_PROJECTION);
  glLoadMatrixf(@FProjection[0]);
  glMatrixMode(GL_MODELVIEW);
  glLoadMatrixf(@FModelView[0]);
end;

procedure TGLCamera.Mult;
begin
  if FProjectionInvalidated then
    RecalculateProjection;
  if FModelViewInvalidated then
    RecalculateModelView;
  glMatrixMode(GL_PROJECTION);
  glMultMatrixf(@FProjection[0]);
  glMatrixMode(GL_MODELVIEW);
  glMultMatrixf(@FModelView[0]);
end;

procedure TGLCamera.Update(const TimeInterval: Double);
begin

end;

procedure TGLCamera.Validate;
begin
  if FProjectionInvalidated then
    RecalculateProjection;
  if FModelViewInvalidated then
    RecalculateModelView;
end;

{ TGLCameraPerspective }

procedure TGLCameraPerspective.SetFarZ(const AValue: TVectorFloat);
begin
  if FFarZ = AValue then
    Exit;
  FFarZ := AValue;
  InvalidateProjection;
end;

procedure TGLCameraPerspective.SetFOV(const AValue: TVectorFloat);
begin
  if FFOV = AValue then
    Exit;
  FFOV := AValue;
  InvalidateProjection;
end;

procedure TGLCameraPerspective.SetNearZ(const AValue: TVectorFloat);
begin
  if FNearZ = AValue then
    Exit;
  FNearZ := AValue;
  InvalidateProjection;
end;

procedure TGLCameraPerspective.RecalculateProjection;
var
  f: Single;
  aspect: Single;
begin
  aspect := (FViewport.Right - FViewport.Left) / (FViewport.Bottom - FViewport.Top);
  f := cotan(DegToRad(FFOV) / 2.0);
  FProjection := IdentityMatrix4f;
  FProjection[0] := f / aspect;
  FProjection[5] := f;
  FProjection[10] := (FFarZ - FNearZ) / (FNearZ - FFarZ);
  FProjection[11] := -1;
  FProjection[15] := 0.0;
  FProjection[14] := (2*FFarZ*FNearZ) / (FNearZ - FFarZ);
  DeinvalidateProjection;
end;

procedure TGLCameraPerspective.ViewportChanged(Sender: TObject);
begin
  InvalidateProjection;
end;

{ TGLCameraFree }

constructor TGLCameraFree.Create;
begin
  inherited;
  FPos := Vector3(0.0, 0.0, 0.0);
  FRotation := Vector2(-Pi/4, 0.0);
  FZoom := -5.0;
  FFlatRightFrontInvalidated := True;
  FTransformedPosInvalidated:= True;
end;

procedure TGLCameraFree.SetPos(const AValue: TVector3);
begin
  if FPos=AValue then exit;
  FPos:=AValue;
  InvalidateModelView;
end;

function TGLCameraFree.GetFlatFront: TVector2;
begin
  if FFlatRightFrontInvalidated then
    RecalculateFlatRightFront;
  Exit(FFlatFront);
end;

function TGLCameraFree.GetFlatRight: TVector2;
begin
  if FFlatRightFrontInvalidated then
    RecalculateFlatRightFront;
  Exit(FFlatRight);
end;

function TGLCameraFree.GetTransformedPos: TVector3;
begin
  if FTransformedPosInvalidated then
    RecalculateTransformedPos;
  Exit(FTransformedPos);
end;

procedure TGLCameraFree.SetRotation(const AValue: TVector2);
begin
  if FRotation=AValue then exit;
  FRotation:=AValue;
  InvalidateModelView;
end;

procedure TGLCameraFree.SetZoom(const AValue: TVectorFloat);
begin
  if FZoom=AValue then exit;
  FZoom:=AValue;
  InvalidateModelView;
end;

procedure TGLCameraFree.DeinvalidateFlatRightFront;
begin
  FFlatRightFrontInvalidated := False;
end;

procedure TGLCameraFree.DeinvalidateTransformedPos;
begin
  FTransformedPosInvalidated := False;
end;

procedure TGLCameraFree.InvalidateFlatRightFront;
begin
  FFlatRightFrontInvalidated := True;
end;

procedure TGLCameraFree.InvalidateTransformedPos;
begin
  FTransformedPosInvalidated := True;
end;

procedure TGLCameraFree.RecalculateFlatRightFront;
begin
  FFlatRight := AngleToVec(-FRotation.Y);
  FFlatFront := AngleToVec(-(FRotation.Y + Pi / 2.0));

  DeinvalidateFlatRightFront;
end;

procedure TGLCameraFree.RecalculateModelView;
var
  Mat: TMatrix4;
begin
  Mat := TranslationMatrix(Vector3(0, 0, FZoom)) * RotationMatrixX(FRotation.X) * RotationMatrixZ(FRotation.Y) * TranslationMatrix(-FPos);
  FModelView := Mat;
  FModelViewHiRes := Mat;
  FRight := Normalize(Vector3(Mat[0], Mat[4], Mat[8]));
  FUp := Normalize(Vector3(Mat[1], Mat[5], Mat[9]));
  FFront := Normalize(Vector3(Mat[2], Mat[6], Mat[10]));
  DeinvalidateModelView;
end;

procedure TGLCameraFree.RecalculateTransformedPos;
var
  Mat: TMatrix4;
begin
  if FModelViewInvalidated then
    RecalculateModelView;
  Mat := TranslationMatrix(FPos) * RotationMatrixZ(-FRotation.Y) * RotationMatrixX(-FRotation.X) * TranslationMatrix(Vector3(0, 0, -FZoom));
  FTransformedPos := (Mat * Vector4(0.0, 0.0, 0.0, 1.0)).Vec3;
end;

{ TGLCameraFreeSmooth }

constructor TGLCameraFreeSmooth.Create;
begin
  inherited;
  FAcceleration := Vector2(0.0, 0.0);
  FVelocity := Vector2(0.0, 0.0);
  FZoomAcceleration := 0.0;
  FZoomVelocity := 0.0;
  FRotationAcceleration := Vector2(0.0, 0.0);
  FRotationVelocity := Vector2(0.0, 0.0);
  FMoving := False;
  FRotating := False;
end;

procedure TGLCameraFreeSmooth.SetAcceleration(const AValue: TVector2);
begin
  if FAcceleration=AValue then exit;
  FAcceleration:=AValue;
end;

procedure TGLCameraFreeSmooth.SetRotationAcceleration(const AValue: TVector2);
begin
  if FRotationAcceleration=AValue then exit;
  FRotationAcceleration:=AValue;
end;

procedure TGLCameraFreeSmooth.SetRotationVelocity(const AValue: TVector2);
begin
  if FRotationVelocity=AValue then exit;
  FRotationVelocity:=AValue;
end;

procedure TGLCameraFreeSmooth.SetVelocity(const AValue: TVector2);
begin
  if FVelocity=AValue then exit;
  FVelocity:=AValue;
end;

procedure TGLCameraFreeSmooth.SetZoomAcceleration(const AValue: TVectorFloat);
begin
  if FZoomAcceleration=AValue then exit;
  FZoomAcceleration:=AValue;
end;

procedure TGLCameraFreeSmooth.SetZoomVelocity(const AValue: TVectorFloat);
begin
  if FZoomVelocity=AValue then exit;
  FZoomVelocity:=AValue;
end;

procedure TGLCameraFreeSmooth.Accelerate(const ABy: TVector2);
begin
  FAcceleration += ABy;
end;

procedure TGLCameraFreeSmooth.AccelerateRotation(const ABy: TVector2);
begin
  FRotationAcceleration += ABy;
end;

procedure TGLCameraFreeSmooth.AccelerateZoom(const ABy: TVectorFloat);
begin
  FZoomAcceleration += ABy;
end;

procedure TGLCameraFreeSmooth.IssueMoveTo(const ATarget: TVector2);
begin
  FMoving := True;
  FMoveTarget := ATarget;
end;

procedure TGLCameraFreeSmooth.IssueRotateTo(const ATarget: TVector2);
begin
  FRotating := True;
  FRotateTarget := ATarget;
end;

procedure TGLCameraFreeSmooth.StopMovement(ResetAcceleration: Boolean
  );
begin
  FMoving := False;
  if ResetAcceleration then
  begin
    FAcceleration := Vector2(0.0, 0.0);
  end;
end;

procedure TGLCameraFreeSmooth.StopRotation(ResetAcceleration: Boolean
  );
begin
  FRotating := False;
  if ResetAcceleration then
  begin
    FRotationAcceleration := Vector2(0.0, 0.0);
  end;
end;

procedure TGLCameraFreeSmooth.StopZoom(ResetAcceleration: Boolean);
begin
  if ResetAcceleration then
  begin
    FZoomAcceleration := 0.0;
  end;
end;

procedure TGLCameraFreeSmooth.Update(const TimeInterval: Double);
var
  TISqr: TVectorFloat;
  D: TVector2;
  L: TVectorFloat;
  Changed: Boolean;
begin
  if FMoving then
  begin
    D := FMoveTarget - FPos.Vec2;
    L := VLength(D);
    if L <= 0.05 then
    begin
      FAcceleration := D * 50.0;
      FMoving := False;
    end
    else
      FAcceleration := D * 50.0;
  end;

  if FRotating then
  begin
    D := FRotateTarget - FRotation;
    if Abs(D.Y) > Pi then
      D.Y := -D.Y;
    L := VLength(D);
    if L <= 0.05 then
    begin
      FRotating := False;
      FRotationVelocity := D * 7.5;
    end
    else
    begin
      FRotationVelocity := D * 7.5;
    end;
  end;

  TISqr := Sqr(TimeInterval);
  if not (FVelocity = Vector2(0.0, 0.0)) or not (FAcceleration = Vector2(0.0, 0.0)) then
  begin
//    WriteLn(Format('%.16f %.16f', [FVelocity.X, FVelocity.Y]));
//    WriteLn(Format('%.16f %.16f', [FAcceleration.X, FAcceleration.Y]));
    FPos.Vec2 += 0.5 * FAcceleration * TISqr + FVelocity * TimeInterval;
    FVelocity += FAcceleration * TimeInterval;

    FAcceleration /= 180 * TimeInterval;
    if Abs(FAcceleration.X) <= ACCEL_EPSILON then
      FAcceleration.X := 0.0;
    if Abs(FAcceleration.Y) <= ACCEL_EPSILON then
      FAcceleration.Y := 0.0;

    FVelocity /= 110 * TimeInterval;
    if Abs(FVelocity.X) <= VEL_EPSILON then
      FVelocity.X := 0.0;
    if Abs(FVelocity.Y) <= VEL_EPSILON then
      FVelocity.Y := 0.0;

    InvalidateModelView;
    DoMoved;
  end;

  if (FZoomVelocity <> 0.0) or (FZoomAcceleration <> 0.0) then
  begin
    FZoom += 0.5 * FZoomAcceleration * TISqr + FZoomVelocity * TimeInterval;
    FZoomVelocity += FZoomAcceleration * TimeInterval;

    FZoomAcceleration /= 180 * TimeInterval;
    if Abs(FZoomAcceleration) <= ACCEL_EPSILON then
      FZoomAcceleration := 0.0;

    FZoomVelocity /= 110 * TimeInterval;
    if Abs(FZoomVelocity) <= VEL_EPSILON then
      FZoomVelocity := 0.0;

    InvalidateModelView;
    DoMoved;
  end;

  if (FRotationVelocity <> Vector2(0.0, 0.0)) or (FRotationAcceleration <> Vector2(0.0, 0.0)) then
  begin
    FRotation += 0.5 * FRotationAcceleration * TISqr + FRotationVelocity * TimeInterval;
    FRotationVelocity += FRotationAcceleration * TimeInterval;

    FRotationAcceleration /= 180 * TimeInterval;
    if Abs(FRotationAcceleration.X) <= ACCEL_EPSILON then
      FRotationAcceleration.X := 0.0;
    if Abs(FRotationAcceleration.Y) <= ACCEL_EPSILON then
      FRotationAcceleration.Y := 0.0;

    FRotationVelocity /= 110 * TimeInterval;
    if Abs(FRotationVelocity.X) <= VEL_EPSILON then
      FRotationVelocity.X := 0.0;
    if Abs(FRotationVelocity.Y) <= VEL_EPSILON then
      FRotationVelocity.Y := 0.0;

    InvalidateModelView;
    InvalidateFlatRightFront;
  end;

  if FRotation.X < -(8*Pi/18) then
  begin
    StopRotation(True);
    FRotation.X := -(8*Pi/18);
    InvalidateModelView;
    InvalidateFlatRightFront;
  end;

  if FRotation.X > -(Pi/18) then
  begin
    StopRotation(True);
    FRotation.X := -(Pi/18);
    InvalidateModelView;
    InvalidateFlatRightFront;
  end;

  if FRotation.Y > Pi2 then
    FRotation.Y -= Pi2;
  if FRotation.Y < 0.0 then
    FRotation.Y += Pi2;

  if FZoom >= -1.3 then
  begin
    FZoom := -1.3;
    StopZoom(True);
    InvalidateModelView;
  end;
end;

end.

