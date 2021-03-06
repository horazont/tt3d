unit GLBase;
(**********************************************************************
File name: glbase.pas
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
  Classes, SysUtils, dglOpenGL, DynamicException, GTBase;

type
  EGLException = class (EDynamicException);

  { TGLObject }

  TGLObject = class (TGTBaseObject)
  public
    constructor Create; override;
  protected
    FGLID: TGLuint;
  public
    procedure Bind; virtual; abstract;
    procedure Unbind; virtual; abstract;
  end;

procedure RaiseLastGLError;

implementation

procedure RaiseLastGLError;
var
  Error: TGLenum;
begin
  Error := glGetError();
  if Error <> GL_NO_ERROR then
    raise Exception.CreateFmt('OpenGL error: %s', [gluErrorString(Error)]);
end;

{ TGLObject }

constructor TGLObject.Create;
begin
  inherited Create;
  FGLID := 0;
end;

end.

