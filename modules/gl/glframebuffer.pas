unit GLFramebuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dglOpenGL;

type

  { TGLAttachment }

  TGLAttachment = class (TObject)
  private
    FID: TGLuint;
  protected
    procedure SetGLObject(AID: TGLuint);
  public
    function GetGLObject: TGLuint;
  end;

  { TGLAttachmentRawTexture }

  TGLAttachmentRawTexture = class (TGLAttachment)
  public
    constructor Create(Format: TGLenum; Width, Height: Integer);
  private
    FTextureID: TGLuint;
  public
    function GetTextureID: TGLuint;
  end;

  TGLAttachmentRenderBuffer = class (TGLAttachment)

  end;

implementation

{

    glGenTextures(1, @FFFTSBufferTex);
    glBindTexture(GL_TEXTURE_2D, FFFTSBufferTex);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32F, FFT_WIDTH, 1, 0, GL_RGBA, GL_FLOAT, nil);
    RaiseGLError;
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glBindTexture(GL_TEXTURE_2D, 0);

    glGenRenderbuffers(1, @Depth);
    glBindRenderbuffer(GL_RENDERBUFFER, Depth);
    glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT, FFT_WIDTH, 1);
    RaiseGLError;
    glBindRenderbuffer(GL_RENDERBUFFER, 0);

    glGenFramebuffers(1, @FFFTSFramebuffer);
    glBindFramebuffer(GL_FRAMEBUFFER, FFFTSFramebuffer);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, Depth);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, FFFTSBufferTex, 0);
    RaiseGLError;
    CheckFBO;
    glBindFramebuffer(GL_FRAMEBUFFER, 0);  }

{ TGLAttachment }

procedure TGLAttachment.SetGLObject(AID: TGLuint);
begin
  FID := AID;
end;

function TGLAttachment.GetGLObject: TGLuint;
begin
  Result := FID;
end;

{ TGLAttachmentRawTexture }

constructor TGLAttachmentRawTexture.Create(Format: TGLenum; Width,
  Height: Integer);
begin
  glGenTextures(1, @FTextureID);
  glBindTexture(GL_TEXTURE_2D, FTextureID);
end;

function TGLAttachmentRawTexture.GetTextureID: TGLuint;
begin

end;

end.

