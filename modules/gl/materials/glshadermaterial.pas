unit GLShaderMaterial;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GLGeometry, Geometry, GLShader;

type

  { TGLShaderMaterial }

  TGLShaderMaterial = class (TGLMaterial)
  public
    constructor Create(const AGeometryBuffer: TGLGeometryBuffer;
      const AFormat: TGLGeometryFormatClass); override;
    destructor Destroy; override;
  private
    FShader: TGLShader;
  public
    property Shader: TGLShader read FShader;
  public
    procedure BindForRendering(const UseStream: Boolean); override;
    procedure UnbindForRendering; override;
  end;

implementation

{ TGLShaderMaterial }

constructor TGLShaderMaterial.Create(const AGeometryBuffer: TGLGeometryBuffer;
  const AFormat: TGLGeometryFormatClass);
begin
  inherited;
  FShader := TGLShader.Create;
end;

destructor TGLShaderMaterial.Destroy;
begin
  FShader.Free;
  inherited Destroy;
end;

procedure TGLShaderMaterial.BindForRendering(const UseStream: Boolean);
begin
  inherited BindForRendering(UseStream);
  FShader.Bind;
end;

procedure TGLShaderMaterial.UnbindForRendering;
begin
  FShader.Unbind;
  inherited UnbindForRendering;
end;

end.

