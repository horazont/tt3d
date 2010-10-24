unit GLShader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dglOpenGL, ioLog;

type

  { TGLShader }

  TGLShader = class (TObject)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FProgramObject: TGLUint;
  protected
    function CompileShader(Kind: TGLenum; const Source: String): GLHandle;
  public
    procedure Bind;
    procedure LoadShader(const Vertex, Fragment: String);
    procedure LoadShader(const Vertex, Fragment: TStream);
    procedure Unbind;
  end;

implementation

{ TGLShader }

constructor TGLShader.Create;
begin
  FProgramObject := -1;
end;

destructor TGLShader.Destroy;
begin
  if FProgramObject <> 0 then
    glDeleteProgram(FProgramObject);
  inherited Destroy;
end;

function TGLShader.CompileShader(Kind: TGLenum; const Source: String
  ): GLHandle;
var
  Src: PChar;
  Len: GLint;
  Error: String;
begin
  Src := @Source[1];
  Len := Length(Source);
  Result := glCreateShader(Kind);
  glShaderSource(Result, 1, @Src, @Len);
  glCompileShader(Result);

  glGetShaderiv(Result, GL_INFO_LOG_LENGTH, @Len);
  if Len > 1 then
  begin
    SetLength(Error, Len);
    glGetShaderInfoLog(Result, Len, Len, @Error[1]);
    SetLength(Error, Len);
    GeneralLog.Log(lmetWarning, 'Shader compilation returned warnings/errors:'+LineEnding+'%s', 'TGLShader.CompileShader', [Error]);
  end;
end;

procedure TGLShader.Bind;
begin
  glUseProgram(FProgramObject);
end;

procedure TGLShader.LoadShader(const Vertex, Fragment: String);
var
  VertexObj, FragmentObj: TGLUint;
  Error: String;
begin
  if FProgramObject <> -1 then
    glDeleteProgram(FProgramObject);
  FProgramObject := glCreateProgram();
  try
    VertexObj := CompileShader(GL_VERTEX_SHADER, Vertex);
    FragmentObj := CompileShader(GL_FRAGMENT_SHADER, Fragment);

    glAttachShader(FProgramObject, VertexObj);
    glAttachShader(FProgramObject, FragmentObj);

    glLinkProgram(FProgramObject);
    if glGetError() <> GL_NO_ERROR then
    begin
      Error := StrPas(gluErrorString(glGetError()));
      glDeleteShader(VertexObj);
      glDeleteShader(FragmentObj);
      // raise Exception.CreateFmt('Linker error: %s', [Error]);
      glDeleteProgram(FProgramObject);
      FProgramObject := -1;
      Exit;
    end;
    glDeleteShader(VertexObj);
    glDeleteShader(FragmentObj);
  except
    glDeleteProgram(FProgramObject);
    raise;
  end;
end;

procedure TGLShader.LoadShader(const Vertex, Fragment: TStream);
var
  VS, FS: String;
begin
  SetLength(VS, Vertex.Size);
  Vertex.Read(VS[1], Length(VS));
  SetLength(FS, Fragment.Size);
  Vertex.Read(FS[1], Length(FS));
  LoadShader(VS, FS);
end;

procedure TGLShader.Unbind;
begin
  glUseProgram(0);
end;

end.

