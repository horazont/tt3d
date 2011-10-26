unit MessageLoop;
(**********************************************************************
File name: messageloop.pas
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
  Classes, SysUtils, sdl;

type
  TMouseButton = Word;
  TMouseButtons = Word;

  TOnKeyDown = procedure (Sender: TObject; AKey: Word; AChar: Cardinal) of object;
  TOnKeyPress = TOnKeyDown;
  TOnKeyUp = TOnKeyDown;

  TOnMouseDown = procedure (Sender: TObject; X, Y: Integer; ChangedButton: TMouseButton) of object;
  TOnMouseUp = TOnMouseDown;

  TOnMouseMove = procedure (Sender: TObject; X, Y: Integer; RelX, RelY: Integer) of object;

  TOnLoop = procedure (Sender: TObject; TimeInterval: Double) of object;
  TOnIdle = procedure (Sender: TObject) of object;
  TOnQuit = procedure (Sender: TObject) of object;

  { TMessageLoop }

  TSDLApplication = class (TObject)
  public
    constructor Create; virtual;
    destructor Destroy; override;
  private
    FOnIdle: TOnIdle;
    FOnKeyDown: TOnKeyDown;
    FOnKeyPress: TOnKeyPress;
    FOnKeyUp: TOnKeyUp;
    FOnLoop: TOnLoop;
    FOnMouseDown: TOnMouseDown;
    FOnMouseMove: TOnMouseMove;
    FOnMouseUp: TOnMouseUp;
    FOnQuit: TOnQuit;
    FSleep: Cardinal;
    FTermiated: Boolean;
  protected
    procedure DoIdle; virtual;
    procedure DoMouseDown(X, Y: Integer; ChangedButton: TMouseButton); virtual;
    procedure DoMouseMove(X, Y: Integer; RelX, RelY: Integer); virtual;
    procedure DoMouseUp(X, Y: Integer; ChangedButton: TMouseButton); virtual;
    procedure DoLoop(TimeInterval: Double); virtual;
    procedure DoKeyDown(AKey: Word; AChar: Cardinal); virtual;
    procedure DoKeyPress(AKey: Word; AChar: Cardinal); virtual;
    procedure DoKeyUp(AKey: Word; AChar: Cardinal); virtual;
    procedure DoQuit; virtual;
  public
    procedure EnterLoop;
    procedure Terminate;
    function Terminated: Boolean;
    procedure ProcessMessages;
  published
    property OnIdle: TOnIdle read FOnIdle write FOnIdle;
    property OnMouseDown: TOnMouseDown read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TOnMouseMove read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TOnMouseUp read FOnMouseUp write FOnMouseUp;
    property OnLoop: TOnLoop read FOnLoop write FOnLoop;
    property OnKeyDown: TOnKeyDown read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TOnKeyPress read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TOnKeyUp read FOnKeyUp write FOnKeyUp;
    property OnQuit: TOnQuit read FOnQuit write FOnQuit;
    property Sleep: Cardinal read FSleep write FSleep;
  end;

implementation

{ TSDLApplication }

constructor TSDLApplication.Create;
begin
  FTermiated := False;
  FSleep := 10;
end;

destructor TSDLApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TSDLApplication.DoIdle;
begin
  if Assigned(FOnIdle) then
    FOnIdle(Self);
end;

procedure TSDLApplication.DoMouseDown(X, Y: Integer; ChangedButton: TMouseButton);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, X, Y, ChangedButton);
end;

procedure TSDLApplication.DoMouseMove(X, Y: Integer; RelX, RelY: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, X, Y, RelX, RelY);
end;

procedure TSDLApplication.DoMouseUp(X, Y: Integer; ChangedButton: TMouseButton);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, X, Y, ChangedButton);
end;

procedure TSDLApplication.DoLoop(TimeInterval: Double);
begin
  if Assigned(FOnLoop) then
    FOnLoop(Self, TimeInterval);
end;

procedure TSDLApplication.DoKeyDown(AKey: Word; AChar: Cardinal);
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self, AKey, AChar);
end;

procedure TSDLApplication.DoKeyPress(AKey: Word; AChar: Cardinal);
begin
  if Assigned(FOnKeyPress) then
    FOnKeyPress(Self, AKey, AChar);
end;

procedure TSDLApplication.DoKeyUp(AKey: Word; AChar: Cardinal);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Self, AKey, AChar);
end;

procedure TSDLApplication.DoQuit;
begin
  if Assigned(FOnQuit) then
    FOnQuit(Self);
end;

procedure TSDLApplication.EnterLoop;
var
  Last, This: Cardinal;
begin
  Last := SDL_GetTicks;
  while not Terminated do
  begin
    ProcessMessages;
    This := SDL_GetTicks;
    if Last - This < FSleep then
    begin
      DoIdle;
      SysUtils.Sleep(FSleep - (Last - SDL_GetTicks));
    end
    else
    begin
      DoLoop((Last - This) / 1000.0);
      Last := This;
      SysUtils.Sleep(FSleep - (Last - SDL_GetTicks));
    end;
  end;
end;

procedure TSDLApplication.Terminate;
begin
  FTermiated := True;
end;

function TSDLApplication.Terminated: Boolean;
begin
  Result := FTermiated;
end;

procedure TSDLApplication.ProcessMessages;
var
  Ev: TSDL_Event;
begin
  while SDL_PollEvent(@Ev) <> 0 do
  begin
    case Ev.type_ of
      SDL_KEYDOWN: DoKeyDown(Ev.key.keysym.sym, Ev.key.keysym.unicode);
      SDL_KEYUP: DoKeyUp(Ev.key.keysym.sym, Ev.key.keysym.unicode);
      SDL_MOUSEBUTTONDOWN: DoMouseDown(Ev.button.x, Ev.button.y, Ev.button.which);
      SDL_MOUSEBUTTONUP: DoMouseUp(Ev.button.x, Ev.button.y, Ev.button.which);
      SDL_MOUSEMOTION: DoMouseMove(Ev.motion.x, Ev.motion.y, Ev.motion.xrel, Ev.motion.yrel);
      SDL_QUITEV: DoQuit;
    end;
  end;
end;

end.

