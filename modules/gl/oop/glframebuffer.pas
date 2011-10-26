unit GLFramebuffer;
(**********************************************************************
File name: glframebuffer.pas
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
  Classes, SysUtils, dglOpenGL, GLBase, GTBase, GLCamera;

const
  MAX_FRAMEBUFFER_COLOR_ATTACHMENTS = 8;

type
  EGLFramebufferError = class (EGLException);

  { TGLAttachment }

  TGLAttachment = class (TGTMultiRefObject)
  public
    constructor Create(Format: TGLenum; AWidth, AHeight: Integer); virtual;
    constructor Create; override;
  private
    FAutofree: Boolean;
    FID: TGLuint;
    FWidth, FHeight: Integer;
  protected
    procedure Attach(Attachment: TGLenum); virtual; abstract;
    procedure DoRefChange; override;
    procedure SetGLObject(AID: TGLuint);
  public
    function GetGLObject: TGLuint;
  public
    property Height: Integer read FHeight;
    property Width: Integer read FWidth;
  end;

  { TGLAttachmentRawTexture }

  TGLAttachmentRawTexture = class (TGLAttachment)
  public
    constructor Create(Format: TGLenum; AWidth, AHeight: Integer); override;
    destructor Destroy; override;
  protected
    procedure Attach(Attachment: TGLenum); override;
  end;

  { TGLAttachmentRenderBuffer }

  TGLAttachmentRenderBuffer = class (TGLAttachment)
  public
    constructor Create(Format: TGLenum; AWidth, AHeight: Integer); override;
    destructor Destroy; override;
  protected
    procedure Attach(Attachment: TGLenum); override;
  end;

  { TGLFramebuffer }

  TGLFramebuffer = class (TGLObject)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FChanged: Boolean;
    FDepthAttachment,
    FStencilAttachment: TGLAttachment;
    FColorAttachments: array [0..MAX_FRAMEBUFFER_COLOR_ATTACHMENTS-1] of TGLAttachment;
    FWidth, FHeight: Integer;
    function GetColorAttachment(Index: Integer): TGLAttachment;
  protected
    procedure RaiseFramebufferError(Message: String);
    procedure HandleAttachmentDeleting(Sender: TObject);
    procedure LinkAttachment(const AAttachment: TGLAttachment);
    procedure UnlinkAttachment(const AAttachment: TGLAttachment);
    procedure SetColorAttachment(AIndex: Integer; AAttachment: TGLAttachment);
    procedure SetDepthAttachment(AAttachment: TGLAttachment);
    procedure SetStencilAttachment(AAttachment: TGLAttachment);
    procedure RemoveAttachment(AAttachment: TGLAttachment);
  public
    property ColorAttachment[Index: Integer]: TGLAttachment read GetColorAttachment write SetColorAttachment;
    property DepthAttachment: TGLAttachment read FDepthAttachment write SetDepthAttachment;
    property StencilAttachment: TGLAttachment read FStencilAttachment write SetStencilAttachment;
  public
    procedure Bind; override;
    procedure Unbind; override;
    procedure SetupActualViewport;
    procedure SetupViewport(const AViewport: TGLViewport);
    procedure Validate;
  end;

implementation

{ TGLAttachment }

constructor TGLAttachment.Create(Format: TGLenum; AWidth, AHeight: Integer);
begin
  Create;
  FWidth := AWidth;
  FHeight := AHeight;
end;

constructor TGLAttachment.Create;
begin
  inherited Create;
  FAutofree := True;
  FID := 0;
end;

procedure TGLAttachment.DoRefChange;
begin
  inherited;
  if FAutofree and (ReferenceCount = 0) then
    Free;
end;

procedure TGLAttachment.SetGLObject(AID: TGLuint);
begin
  FID := AID;
end;

function TGLAttachment.GetGLObject: TGLuint;
begin
  Result := FID;
end;

{ TGLAttachmentRawTexture }

constructor TGLAttachmentRawTexture.Create(Format: TGLenum; AWidth,
  AHeight: Integer);
var
  TID: TGLuint;
begin
  inherited;
  glGenTextures(1, @TID);
  glBindTexture(GL_TEXTURE_2D, TID);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexImage2D(GL_TEXTURE_2D, 0, Format, AWidth, AHeight, 0, GL_RGB, GL_UNSIGNED_BYTE, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glBindTexture(GL_TEXTURE_2D, 0);
  SetGLObject(TID);
  RaiseLastGLError;
end;

destructor TGLAttachmentRawTexture.Destroy;
begin
  glDeleteTextures(1, @FID);
  inherited Destroy;
end;

procedure TGLAttachmentRawTexture.Attach(Attachment: TGLenum);
begin
  glFramebufferTexture2D(GL_FRAMEBUFFER, Attachment, GL_TEXTURE_2D, GetGLObject, 0);
  RaiseLastGLError;
end;

{ TGLAttachmentRenderBuffer }

constructor TGLAttachmentRenderBuffer.Create(Format: TGLenum; AWidth,
  AHeight: Integer);
var
  RID: TGLuint;
begin
  inherited;
  glGenRenderbuffers(1, @RID);
  glBindRenderbuffer(GL_RENDERBUFFER, RID);
  glRenderbufferStorage(GL_RENDERBUFFER, Format, AWidth, AHeight);
  glBindRenderbuffer(GL_RENDERBUFFER, 0);
  SetGLObject(RID);
  RaiseLastGLError;
end;

destructor TGLAttachmentRenderBuffer.Destroy;
begin
  glDeleteRenderbuffers(1, @FID);
  inherited Destroy;
end;

procedure TGLAttachmentRenderBuffer.Attach(Attachment: TGLenum);
begin
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, Attachment, GL_RENDERBUFFER, GetGLObject);
  RaiseLastGLError;
end;

{ TGLFramebuffer }

constructor TGLFramebuffer.Create;
begin
  inherited;
  FChanged := False;
  glGenFramebuffers(1, @FGLID);
end;

destructor TGLFramebuffer.Destroy;
var
  I: Integer;
begin
  SetDepthAttachment(nil);
  SetStencilAttachment(nil);
  for I := 0 to MAX_FRAMEBUFFER_COLOR_ATTACHMENTS - 1 do
    SetColorAttachment(I, nil);
  glDeleteFramebuffers(1, @FGLID);
  inherited Destroy;
end;

function TGLFramebuffer.GetColorAttachment(Index: Integer): TGLAttachment;
begin

end;

procedure TGLFramebuffer.RaiseFramebufferError(Message: String);
begin
  raise EGLFramebufferError.Create(Message);
end;

procedure TGLFramebuffer.HandleAttachmentDeleting(Sender: TObject);
begin
  RemoveAttachment(Sender as TGLAttachment);
end;

procedure TGLFramebuffer.LinkAttachment(const AAttachment: TGLAttachment);
begin
  AAttachment.AddReference(Self);
  AAttachment.OnDestruction.RegisterHandler(@HandleAttachmentDeleting);
end;

procedure TGLFramebuffer.UnlinkAttachment(const AAttachment: TGLAttachment);
begin
  AAttachment.OnDestruction.UnRegisterHandler(@HandleAttachmentDeleting);
  AAttachment.RemoveReference(Self);
end;

procedure TGLFramebuffer.SetColorAttachment(AIndex: Integer;
  AAttachment: TGLAttachment);
begin
  if FColorAttachments[AIndex] <> nil then
    UnlinkAttachment(FColorAttachments[AIndex]);
  FColorAttachments[AIndex] := AAttachment;
  if AAttachment <> nil then
  begin
    FChanged := True;
    LinkAttachment(AAttachment);
  end;
end;

procedure TGLFramebuffer.SetDepthAttachment(AAttachment: TGLAttachment);
begin
  if FDepthAttachment <> nil then
    UnlinkAttachment(FDepthAttachment);
  FDepthAttachment := AAttachment;
  if FDepthAttachment <> nil then
  begin
    FChanged := True;
    LinkAttachment(FDepthAttachment);
  end;
end;

procedure TGLFramebuffer.SetStencilAttachment(AAttachment: TGLAttachment);
begin
  if FStencilAttachment <> nil then
    UnlinkAttachment(FStencilAttachment);
  FStencilAttachment := AAttachment;
  if FStencilAttachment <> nil then
  begin
    FChanged := True;
    LinkAttachment(FStencilAttachment);
  end;
end;

procedure TGLFramebuffer.RemoveAttachment(AAttachment: TGLAttachment);
var
  I: Integer;
begin
  UnlinkAttachment(AAttachment);
  if FDepthAttachment = AAttachment then
    SetDepthAttachment(nil);
  if FStencilAttachment = AAttachment then
    SetStencilAttachment(nil);
  for I := 0 to High(FColorAttachments) do
    if FColorAttachments[I] = AAttachment then
      SetColorAttachment(I, nil);
end;

procedure TGLFramebuffer.Bind;
begin
  Validate;
  glBindFramebuffer(GL_FRAMEBUFFER, FGLID);
end;

procedure TGLFramebuffer.Unbind;
begin
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
end;

procedure TGLFramebuffer.SetupActualViewport;
begin
  glViewport(0, 0, FWidth, FHeight);
end;

procedure TGLFramebuffer.SetupViewport(const AViewport: TGLViewport);
begin
  Validate;
  AViewport.SetAll(0, 0, FHeight-1, FWidth-1);
end;

procedure TGLFramebuffer.Validate;
var
  I: Integer;
  errno: TGLenum;
begin
  if not FChanged then
    Exit;

  glBindFramebuffer(GL_FRAMEBUFFER, FGLID);
  if FDepthAttachment <> nil then
  begin
    FDepthAttachment.Attach(GL_DEPTH_ATTACHMENT);
    FWidth := FDepthAttachment.Width;
    FHeight := FDepthAttachment.Height;
  end;
  if FStencilAttachment <> nil then
  begin
    FStencilAttachment.Attach(GL_STENCIL_ATTACHMENT);
    FWidth := FStencilAttachment.Width;
    FHeight := FStencilAttachment.Height;
  end;
  for I := 0 to High(FColorAttachments) do
    if FColorAttachments[I] <> nil then
    begin
      FColorAttachments[I].Attach(GL_COLOR_ATTACHMENT0 + I);
      FWidth := FColorAttachments[I].Width;
      FHeight := FColorAttachments[I].Height;
    end;

  errno := glCheckFramebufferStatus(GL_FRAMEBUFFER);
  case errno of
    GL_FRAMEBUFFER_COMPLETE_EXT:
      Exit;
    GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT:
      RaiseFramebufferError('Incomplete attachment.');
    GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT:
      RaiseFramebufferError('Missing attachment');
    GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT:
      RaiseFramebufferError('Incomplete dimensions');
    GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT:
      RaiseFramebufferError('Incomplete formats');
    GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT:
      RaiseFramebufferError('Incomplete draw buffer');
    GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT:
      RaiseFramebufferError('Incomplete read buffer');
    GL_FRAMEBUFFER_UNSUPPORTED_EXT:
      RaiseFramebufferError('Framebufferobjects unsupported');
  else
    RaiseFramebufferError(Format('Unknovn framebuffer state: %d',  [errno]));
  end;
  Unbind;

  FChanged := False;
end;

end.

