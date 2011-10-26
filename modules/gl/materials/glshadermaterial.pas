unit GLShaderMaterial;
(**********************************************************************
File name: glshadermaterial.pas
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

