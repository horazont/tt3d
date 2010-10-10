unit GLMaterials;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GLGeometry;

type

  { TGLMaterials }

  TGLMaterials = class (TObject)
  public
    constructor Create(const MaterialCount: Integer);
    destructor Destroy; override;
  private
    FMaterials: array of TGLMaterial;
  protected
    function GetMaterial(AIndex: Integer): TGLMaterial;
    procedure LinkMaterial(const AMaterial: TGLMaterial); virtual;
    procedure MaterialDeleting(Sender: TObject); virtual;
    procedure SetMaterial(AIndex: Integer; AValue: TGLMaterial); virtual;
    procedure UnlinkMaterial(const AMaterial: TGLMaterial); virtual;
  end;

implementation

{ TGLMaterials }

constructor TGLMaterials.Create(const MaterialCount: Integer);
begin
  SetLength(FMaterials, MaterialCount);
end;

destructor TGLMaterials.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FMaterials) do
    if FMaterials[I] <> nil then
    begin
      UnlinkMaterial(FMaterials[I]);
      FMaterials[I].Free;
    end;
  inherited Destroy;
end;

function TGLMaterials.GetMaterial(AIndex: Integer): TGLMaterial;
begin
  Result := FMaterials[AIndex];
end;

procedure TGLMaterials.LinkMaterial(const AMaterial: TGLMaterial);
begin
  AMaterial.OnDestruction.RegisterHandler(@MaterialDeleting);
end;

procedure TGLMaterials.MaterialDeleting(Sender: TObject);
var
  I: Integer;
  Mat: TGLMaterial;
begin
  Mat := Sender as TGLMaterial;
  for I := 0 to High(FMaterials) do
    if FMaterials[I] = Mat then
    begin
      UnlinkMaterial(Mat);
      FMaterials[I] := nil;
      Exit;
    end;
end;

procedure TGLMaterials.SetMaterial(AIndex: Integer; AValue: TGLMaterial);
begin
  if FMaterials[AIndex] <> nil then
    UnlinkMaterial(FMaterials[AIndex]);
  FMaterials[AIndex] := AValue;
  if AValue <> nil then
    LinkMaterial(AValue);
end;

procedure TGLMaterials.UnlinkMaterial(const AMaterial: TGLMaterial);
begin
  AMaterial.OnDestruction.UnRegisterHandler(@MaterialDeleting);
end;

end.

