unit GLBase;

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

