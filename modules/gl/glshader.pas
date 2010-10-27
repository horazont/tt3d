unit GLShader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dglOpenGL, ioLog, GLGeometry;

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
    property ProgramObject: TGLuint read FProgramObject;
  public
    procedure Bind;
    procedure Clear;
    procedure LoadShader(const Vertex, Fragment: String);
    procedure LoadShader(const Vertex, Fragment: TStream);
    procedure Unbind;
  end;

implementation

{ TGLShader }

constructor TGLShader.Create;
begin
  FProgramObject := 0;
end;

destructor TGLShader.Destroy;
begin
  Clear;
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
  if FProgramObject = 0 then
    Exit;
  glUseProgram(FProgramObject);
  RaiseLastGLError;
end;

procedure TGLShader.Clear;
begin
  if FProgramObject = 0 then
    glDeleteProgram(FProgramObject);
  FProgramObject := 0;
end;

procedure TGLShader.LoadShader(const Vertex, Fragment: String);
var
  VertexObj, FragmentObj: TGLUint;
  Error: String;
begin
  Clear;
  RaiseLastGLError;
  FProgramObject := glCreateProgram();
  RaiseLastGLError;
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
      GeneralLog.Log(lmetError, 'Shader linking failed.', 'TglShader.LoadShader');
      glDeleteProgram(FProgramObject);
      FProgramObject := -1;
      Exit;
    end;
    glDeleteShader(VertexObj);
    RaiseLastGLError;
    glDeleteShader(FragmentObj);
    RaiseLastGLError;
  except
    glDeleteProgram(FProgramObject);
    RaiseLastGLError;
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
  Fragment.Read(FS[1], Length(FS));
  LoadShader(VS, FS);
end;

procedure TGLShader.Unbind;
begin
  glUseProgram(0);
end;

end.

