unit GLHelpers;
(**********************************************************************
File name: glhelpers.pas
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

