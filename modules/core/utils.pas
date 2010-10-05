unit utils;

{$mode objfpc}{$H+}

{.$define CardinalTime}
{$define UTF8}
// {$define LinuxSurfaceDebug}

interface

uses
  Classes, SysUtils, dglOpenGL, SDL, ioLog, math;
  
const
  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;
  
  STRATEGY_GAME_VERSION_MAJOR: Byte = 0;
  STRATEGY_GAME_VERSION_MINOR: Byte = 0;
  
  GAME_WAIT_FOR_THREAD_DELAY: Integer = 250;
  
  Pi: Extended = 3.1415926535;
  
type
  TgglTexture2D = record
    TexID: TGLUInt;
    SCoord, TCoord: Single;
    OrigW, OrigH: Integer;
  end;
  TgglSplitParamList = array of String;

function gglMakePowerOfTwo(const Input: Integer): Integer; inline;

function Divide(const S:String; const Index: Integer; Divider: Char = '='; AEndOfExpression: String = '[end of expression]'):String;
procedure Explode(const S: String; const List: TStrings; Divider: Char = ' ');

function gglSplitParams(Indicators: array of String;
  Defaults: array of String; LastIndicator: String = '';
  StartIndex: Integer = 1): TgglSplitParamList;
  
function ReadStreamLn(AStream: TStream): String;

function StreamReadWordToN(AStream: TStream; var Value: Word): Integer; inline;
function StreamReadDWordToN(AStream: TStream; var Value: DWord): Integer; inline;
function StreamReadQWordToN(AStream: TStream; var Value: QWord): Integer; inline;
function StreamReadSmallIntToN(AStream: TStream; var Value: SmallInt): Integer; inline;
function StreamReadLongIntToN(AStream: TStream; var Value: LongInt): Integer; inline;
function StreamReadInt64ToN(AStream: TStream; var Value: Int64): Integer; inline;

function StreamWriteWordToLE(AStream: TStream; const Value: Word): Integer; inline;
function StreamWriteDWordToLE(AStream: TStream; const Value: DWord): Integer; inline;
function StreamWriteQWordToLE(AStream: TStream; const Value: QWord): Integer; inline;
function StreamWriteSmallIntToLE(AStream: TStream; const Value: SmallInt): Integer; inline;
function StreamWriteLongIntToLE(AStream: TStream; const Value: LongInt): Integer; inline;
function StreamWriteInt64ToLE(AStream: TStream; const Value: Int64): Integer; inline;

function gglSDLSurfaceToGLTexture(const Src: PSDL_Surface; DoConversion: Boolean = True; InitialTextureID: TGLUInt = 0): TgglTexture2D;
procedure gglCopySurfaceContent(Src, Dst: PSDL_Surface);

implementation

function gglMakePowerOfTwo(Const Input: Integer): Integer;
var
  value: Integer;
begin
  value := 1;
  while (value < input) do
  begin
    value := value shl 1;
  end;
  result := value;
end;

function Divide(const S:String; const Index: Integer; Divider: Char = '='; AEndOfExpression: String = '[end of expression]'):String;
var I, EI:Integer; B:String; IsFound: Boolean;
begin
  EI := 1;
  I := -1;
  Result := '';
  B := S+Divider;
  IsFound := False;
  while Pos(Divider, B) > 0 do begin
    Inc(I);
    EI := Pos(Divider, B);
    if I = Index then
    begin
      IsFound := True;
      Break;
    end;
    Delete(B, 1, EI);
  end;
  if IsFound then
    Result := LeftStr(B, EI-1)
  else
    Result := AEndOfExpression;
end;

procedure Explode(const S: String; const List: TStrings; Divider: Char = ' ');
var
  LastIndex: Integer;
  I: Integer;
  C: Char;
  Buf: String;
begin
  I := 1;
  Buf := '';
  LastIndex := 1;
  while I <= Length(S) do
  begin
    C := S[I];
    case C of
      ' ':
      begin
        List.Add(Buf);
        Buf := '';
      end;
    else
      Buf += C;
    end;
    Inc(I);
  end;
end;

function gglSplitParams(Indicators: array of String;
  Defaults: array of String; LastIndicator: String = '';
  StartIndex: Integer = 1): TgglSplitParamList;
var
  I: Integer;
  Count: Integer;
  NextIndex: Integer;
  ParamIndex: Integer;
  Param: String;
begin
  Count := Length(Indicators);
  SetLength(Result, Length(Indicators));

  if Length(Defaults) = Count then
  begin
    for I := 0 to Count - 1 do
      Result[I] := Defaults[I];
  end
  else
  begin
    for I := 0 to Count - 1 do
      Result[I] := '';
  end;

  NextIndex := -1;
  ParamIndex := StartIndex;
  Param := ParamStr(ParamIndex);
  while (Param <> '') and (Param <> LastIndicator) do
  begin
    if NextIndex > -1 then
    begin
      Result[NextIndex] := Param;
      NextIndex := -1;
    end
    else
    begin
      for I := 0 to Count - 1 do
      begin
        if Indicators[I] = Param then
          NextIndex := I;
      end;
    end;

    Inc(ParamIndex);
    Param := ParamStr(ParamIndex);
  end;
end;

function ReadStreamLn(AStream: TStream): String;
var
  I: Integer;
  StartPos: Int64;
  LastChar: Char;
  CurrentChar: Char;
begin
  StartPos := AStream.Position;
  I := 0;
  repeat
    AStream.Read(CurrentChar, 1);
    Inc(I);
  until (CurrentChar = #10) or (CurrentChar = #13) or (AStream.Position = AStream.Size);
  if AStream.Position < AStream.Size then
    Dec(I);

  AStream.Position := StartPos;
  if (I > 0) then
  begin
    SetLength(Result, I);
    AStream.Read(Result[1], I);
  end
  else
    Result := '';

  AStream.Read(CurrentChar, 1);
  if CurrentChar = #13 then
  begin
    AStream.Read(CurrentChar, 1);
    if CurrentChar <> #10 then
      AStream.Position := AStream.Position - 1;
  end;
end;

function StreamReadWordToN(AStream: TStream; var Value: Word): Integer;
begin
  {$IFDEF BIGENDIAN}
  Result := AStream.Read(Value, SizeOf(Word));
  Value := LEToN(Value);
  {$ELSE}
  Result := AStream.Read(Value, SizeOf(Word));
  {$ENDIF}
end;

function StreamReadDWordToN(AStream: TStream; var Value: DWord): Integer;
begin
  {$IFDEF BIGENDIAN}
  Result := AStream.Read(Value, SizeOf(DWord));
  Value := LEToN(Value);
  {$ELSE}
  Result := AStream.Read(Value, SizeOf(DWord));
  {$ENDIF}
end;

function StreamReadQWordToN(AStream: TStream; var Value: QWord): Integer;
begin
  {$IFDEF BIGENDIAN}
  Result := AStream.Read(Value, SizeOf(QWord));
  Value := LEToN(Value);
  {$ELSE}
  Result := AStream.Read(Value, SizeOf(QWord));
  {$ENDIF}
end;

function StreamReadSmallIntToN(AStream: TStream; var Value: SmallInt): Integer;
begin
  {$IFDEF BIGENDIAN}
  Result := AStream.Read(Value, SizeOf(SmallInt));
  Value := LEToN(Value);
  {$ELSE}
  Result := AStream.Read(Value, SizeOf(SmallInt));
  {$ENDIF}
end;

function StreamReadLongIntToN(AStream: TStream; var Value: LongInt): Integer;
begin
  {$IFDEF BIGENDIAN}
  Result := AStream.Read(Value, SizeOf(LongInt));
  Value := LEToN(Value);
  {$ELSE}
  Result := AStream.Read(Value, SizeOf(LongInt));
  {$ENDIF}
end;

function StreamReadInt64ToN(AStream: TStream; var Value: Int64): Integer;
begin
  {$IFDEF BIGENDIAN}
  Result := AStream.Read(Value, SizeOf(Int64));
  Value := LEToN(Value);
  {$ELSE}
  Result := AStream.Read(Value, SizeOf(Int64));
  {$ENDIF}
end;

function StreamWriteWordToLE(AStream: TStream; const Value: Word): Integer;
{$IFDEF BIGENDIAN}
var
  Buf: Word;
begin
  Buf := NToLE(Value);
  Result := AStream.Write(Value, SizeOf(Word));
end;
{$ELSE}
begin
  Result := AStream.Write(Value, SizeOf(Word));
end;
{$ENDIF}

function StreamWriteDWordToLE(AStream: TStream; const Value: DWord): Integer;
{$IFDEF BIGENDIAN}
var
  Buf: DWord;
begin
  Buf := NToLE(Value);
  Result := AStream.Write(Value, SizeOf(DWord));
end;
{$ELSE}
begin
  Result := AStream.Write(Value, SizeOf(DWord));
end;
{$ENDIF}

function StreamWriteQWordToLE(AStream: TStream; const Value: QWord): Integer;
{$IFDEF BIGENDIAN}
var
  Buf: QWord;
begin
  Buf := NToLE(Value);
  Result := AStream.Write(Value, SizeOf(QWord));
end;
{$ELSE}
begin
  Result := AStream.Write(Value, SizeOf(QWord));
end;
{$ENDIF}

function StreamWriteSmallIntToLE(AStream: TStream;
  const Value: SmallInt): Integer;
{$IFDEF BIGENDIAN}
var
  Buf: SmallInt;
begin
  Buf := NToLE(Value);
  Result := AStream.Write(Value, SizeOf(SmallInt));
end;
{$ELSE}
begin
  Result := AStream.Write(Value, SizeOf(SmallInt));
end;
{$ENDIF}

function StreamWriteLongIntToLE(AStream: TStream; const Value: LongInt): Integer;
{$IFDEF BIGENDIAN}
var
  Buf: LongInt;
begin
  Buf := NToLE(Value);
  Result := AStream.Write(Value, SizeOf(LongInt));
end;
{$ELSE}
begin
  Result := AStream.Write(Value, SizeOf(LongInt));
end;
{$ENDIF}

function StreamWriteInt64ToLE(AStream: TStream; const Value: Int64): Integer;
{$IFDEF BIGENDIAN}
var
  Buf: Int64;
begin
  Buf := NToLE(Value);
  Result := AStream.Write(Value, SizeOf(Int64));
end;
{$ELSE}
begin
  Result := AStream.Write(Value, SizeOf(Int64));
end;
{$ENDIF}

function gglSDLSurfaceToGLTexture(const Src: PSDL_Surface; DoConversion: Boolean = True; InitialTextureID: TGLUInt = 0): TgglTexture2D;
var
  SavedFlags: UInt32;
  SavedAlpha: UInt8;
  FinalSurface: PSDL_Surface;
  NewW, NewH: Integer;
  Area: TSDL_Rect;

  MyBuffer: Pointer;
begin
  if (DoConversion) then
  begin
    NewW := gglMakePowerOfTwo(Src^.w);
    NewH := gglMakePowerOfTwo(Src^.h);
    
    {$ifdef LinuxSurfaceDebug}SDL_SaveBMP(Src, 'non-converted.bmp');{$endif}

    FinalSurface := SDL_CreateRGBSurface(
      SDL_SWSurface,
      NewW,
      NewH,
      32,
      $000000FF,
      $0000FF00,
      $00FF0000,
      $FF000000
    );

    if FinalSurface = nil then
    begin
      GeneralLog.Log(lmetError, 'Couldn''t create conversion surface. Texture won''t be converted.', 'SDLSurfaceToGLTexture');
      Exit;
    end;

    Area.x := 0;
    Area.y := 0;
    Area.w := Src^.w;
    Area.h := Src^.h;

    SavedFlags := Src^.flags and (SDL_SRCALPHA or SDL_RLEACCELOK);
    SavedAlpha := Src^.format^.alpha;

    if ((SavedFlags and SDL_SRCALPHA) = SDL_SRCALPHA) then
      SDL_SetAlpha(Src, 0, 0);

    SDL_BlitSurface(Src, @Area, FinalSurface, @Area);

    if ((SavedFlags and SDL_SRCALPHA) = SDL_SRCALPHA) then
      SDL_SetAlpha(Src, SavedFlags, SavedAlpha);

    Result.SCoord := Src^.w / NewW;
    Result.TCoord := Src^.h / NewH;
    Result.OrigW := Src^.w;
    Result.OrigH := Src^.h;
  end
  else
  begin
    FinalSurface := Src;
  end;

    {$ifdef LinuxSurfaceDebug}SDL_SaveBMP(Src, 'converted.bmp');{$endif}

  //GetMem(MyBuffer, FinalSurface^.pitch * FinalSurface^.h);
  try
    //Move(FinalSurface^.pixels^, MyBuffer^, FinalSurface^.pitch * FinalSurface^.h);
  
    if (InitialTextureID = 0) then
    begin
      glGenTextures(1, @Result.TexID);
      if Result.TexID = 0 then
      begin
        if DoConversion then
          SDL_FreeSurface(FinalSurface);
        Exit;
      end;
    end
    else
      Result.TexID := InitialTextureID;
    glBindTexture(GL_TEXTURE_2D, Result.TexID);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, FinalSurface^.w, FinalSurface^.h, 0, GL_RGBA, GL_UNSIGNED_BYTE, FinalSurface^.pixels);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    {$ifdef LinuxSurfaceDebug}SDL_SaveBMP(FinalSurface, 'after-add.bmp');{$endif}
    if DoConversion then
      SDL_FreeSurface(FinalSurface);
  finally
    //FreeMem(MyBuffer);
  end;
  

  // GeneralLog.Log(lmetInfo, 'Converted texture.', 'SDLSurfaceToGLTexture');
end;

procedure gglCopySurfaceContent(Src, Dst: PSDL_Surface);
var
  Area: TSDL_Rect;
  SavedFlags: UInt32;
  SavedAlpha: UInt8;
begin
  Area.x := 0;
  Area.y := 0;
  Area.w := Min(Src^.w, Dst^.w);
  Area.h := Min(Src^.h, Dst^.h);
  


  SavedFlags := Src^.flags and (SDL_SRCALPHA or SDL_RLEACCELOK);
  SavedAlpha := Src^.format^.alpha;

  if ((SavedFlags and SDL_SRCALPHA) = SDL_SRCALPHA) then
    SDL_SetAlpha(Src, 0, 0);

  SDL_BlitSurface(Src, @Area, Dst, @Area);

  if ((SavedFlags and SDL_SRCALPHA) = SDL_SRCALPHA) then
    SDL_SetAlpha(Src, SavedFlags, SavedAlpha);
    

end;

end.

