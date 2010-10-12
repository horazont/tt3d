unit GLHelpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dglOpenGL;

procedure SetupOrthoDefault(const Left, Right, Top, Bottom: Integer);
procedure SetupPerspective(const Left, Right, Top, Bottom: Integer;
  const NearClip, FarClip: Double; const FOV: Double);

implementation

procedure SetupOrthoDefault(const Left, Right, Top, Bottom: Integer);
begin
  glViewport(Left, Top, Right - Left, Bottom - Top);

  glMatrixMode( GL_PROJECTION );
    glLoadIdentity;
    (*case ProjectionType of
      ptPerspective: gluPerspective( FFOV, (Right - Left) / (Bottom - Top), 0.1, 100.0 );
      ptOrthogonal: *)
    glOrtho(Left, Right, Bottom, Top, -10.0, 10.0);
    //end;
  glMatrixMode( GL_MODELVIEW );

  glLoadIdentity;
end;

procedure SetupPerspective(const Left, Right, Top, Bottom: Integer;
  const NearClip, FarClip: Double; const FOV: Double);
begin
  glViewport(Left, Top, Right - Left, Bottom - Top);

  glMatrixMode( GL_PROJECTION );
    glLoadIdentity;
    gluPerspective( FOV, (Right - Left) / (Bottom - Top), NearClip, FarClip );
  glMatrixMode( GL_MODELVIEW );

  glLoadIdentity;
end;

end.

