unit ioLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dglOpenGL, SDL;
  
const
  LOG_THIS_IS_FATAL = 'This is a fatal error. Application will shut down.';
  
type
  TlmLogMode = (lmlmAppend, lmlmOverwrite);
  TlmEntryType = (lmetDebug, lmetInfo, lmetglInfo, lmetHint, lmetWarning, lmetError, lmetFatal);
    // Debug: Only for debugging purposes
    // Info: Just an information, really not important
    // glInfo: Information about OpenGL properties, not important
    // Hint: Might be a problem, but not very important
    // Warning: Chance for a problem is high, but program will run stable
    // Error: Is a problem, maybe stability problems will occur
    // Fatal: Program is unable to continue (like as initialization error), will terminate
    
  { TlmLogFile }

  TlmLogFile = class (TObject)
    constructor Create(const FileName: String; Mode: TlmLogMode = lmlmAppend; HTMLFile: String = '');
    destructor Destroy; override;
  private
    FFileName: String;
    FFileMutex: PSDL_Mutex;
    FFileStream: TFileStream;
    FHTMLStream: TFileStream;
    FLogLevelMutex: PSDL_Mutex;
    FLogLevel: TlmEntryType;
    FMirrorLogToConsoleMutex: PSDL_Mutex;
    FMirrorLogToConsole: Boolean;
    function GetMirrorLogToConsole: Boolean;
    procedure SetMirrorLogToConsole(AValue: Boolean);
    function GetLogLevel: TlmEntryType;
    procedure SetLogLevel(AValue: TlmEntryType);
    procedure nmLog(const EntryType: TlmEntryType; const Text, SourceMethod: String; const HTML: String = '');
  public
    property MirrorLogToConsole: Boolean read GetMirrorLogToConsole write SetMirrorLogToConsole;
    property LogLevel: TlmEntryType read GetLogLevel write SetLogLevel;
  
    procedure Log(const EntryType: TlmEntryType; const Text, SourceMethod: String);
    procedure Log(const EntryType: TlmEntryType; const Text, SourceMethod: String; TextParams: array of const);
    procedure Log(const EntryType: TlmEntryType; const E: Exception; const SourceMethod: String; const ExtraMsg: String = '');
    procedure LogFmt(const EntryType: TlmEntryType; const Text, SourceMethod: String; TextParams: array of const);
    procedure LogException(const EntryType: TlmEntryType; const E: Exception; const SourceMethod: String; const ExtraMsg: String = '');
    procedure OutputExtensions;
    
    procedure ClearLog;
  end;
  
function EntryTypeToStr(EntryType: TlmEntryType): String;

var
  GeneralLog: TlmLogFile;

implementation

uses
  utils;

const
  CSS = 'th { border: 1px solid #000000; text-align: left; vertical-align: top; background-color: #9F9F9F; }'#13#10'td { vertical-align: top; }'#13#10'tr { border: 1px solid #000000; }'#13#10'table { border-collapse: collapse; }';

function EntryTypeToStr(EntryType: TlmEntryType): String;
begin
  case EntryType of
    lmetInfo: Result := 'Info';
    lmetglInfo: Result := 'glInfo';
    lmetDebug: Result := 'Debug';
    lmetHint: Result := 'Hint';
    lmetWarning: Result := 'Warning';
    lmetError: Result := 'Error';
    lmetFatal: Result := 'Fatal';
  else
    Result := 'Unknown';
  end;
end;

{ TlmLog }

constructor TlmLogFile.Create(const FileName: String; Mode: TlmLogMode; HTMLFile: String = '');
var
  HTML: String;
begin
  inherited Create;
  case Mode of
    lmlmAppend:
    begin
      if FileExists(FileName) then
      begin
        FFileStream := TFileStream.Create(FileName, fmOpenWrite);
        FFileStream.Position := FFileStream.Size;
      end
      else
        FFileStream := TFileStream.Create(FileName, fmCreate);
    end;
  else
    FFileStream := TFileStream.Create(FileName, fmCreate);
  end;
  if HTMLFile <> '' then
  begin
    FHTMLStream := TFileStream.Create(HTMLFile + '.css', fmCreate);
    try
      FHTMLStream.Write(CSS[1], Length(CSS));
    finally
      FHTMLStream.Free;
    end;
  
    FHTMLStream := TFileStream.Create(HTMLFile, fmCreate);
    HTML := '<html><head><title>Log file</title><link rel="stylesheet" type="text/css" href="file:///'+StringReplace(HTMLFile, '\', '/', [rfReplaceAll])+'.css'+'" /></head><body><table width="100%"><tr><th width="75px">Type</th><th width="150px">Method</th><th>Message</th></tr>';
    FHTMLStream.Write(HTML[1], Length(HTML));
  end
  else
    FHTMLStream := nil;
  FFileName := FileName;
  FFileMutex := SDL_CreateMutex;
  FLogLevel := lmetInfo;
  FLogLevelMutex := SDL_CreateMutex;
end;

destructor TlmLogFile.Destroy;
var
  HTML: String;
begin
  SDL_DestroyMutex(FLogLevelMutex);
  SDL_DestroyMutex(FFileMutex);
  FFileStream.Free;
  if FHTMLStream <> nil then
  begin
    HTML := '</table></body>';
    FHTMLStream.Write(HTML[1], Length(HTML));
    FHTMLStream.Free;
  end;
  inherited Destroy;
end;

function TlmLogFile.GetMirrorLogToConsole: Boolean;
begin
  SDL_LockMutex(FMirrorLogToConsoleMutex);
  try
    Result := FMirrorLogToConsole and System.IsConsole;
  finally
    SDL_UnlockMutex(FMirrorLogToConsoleMutex);
  end;
end;

procedure TlmLogFile.SetMirrorLogToConsole(AValue: Boolean);
begin
  SDL_LockMutex(FMirrorLogToConsoleMutex);
  try
    FMirrorLogToConsole := AValue;
  finally
    SDL_UnlockMutex(FMirrorLogToConsoleMutex);
  end;
end;

function TlmLogFile.GetLogLevel: TlmEntryType;
begin
  SDL_LockMutex(FLogLevelMutex);
  try
    Result := FLogLevel;
  finally
    SDL_UnlockMutex(FLogLevelMutex);
  end;
end;

procedure TlmLogFile.SetLogLevel(AValue: TlmEntryType);
begin
  SDL_LockMutex(FLogLevelMutex);
  try
    FLogLevel := AValue;
  finally
    SDL_UnlockMutex(FLogLevelMutex);
  end;
end;

procedure TlmLogFile.nmLog(const EntryType: TlmEntryType; const Text, SourceMethod: String; const HTML: String = '');
var
  MethodPart: String;
  S: String;
  Color: String;
  Formatting: String;
begin
  if EntryType < FLogLevel then
    Exit;
  if SourceMethod <> '' then
    MethodPart := '@' + SourceMethod
  else
    MethodPart := '';
  S := '[' + EntryTypeToStr(EntryType) + MethodPart + ']' + ': ' + Text + #13 + #10;
  FFileStream.Write(S[1], Length(S));
  if FMirrorLogToConsole then
    Write(S);
  if FHTMLStream <> nil then
  begin
    S := '';
    Color := '#000000';
    Formatting := '';
    case EntryType of
      lmetFatal, lmetError: Color := '#FF0000';
      lmetWarning: Color := '#FF7F00';
      lmetDebug, lmetInfo, lmetglInfo, lmetHint: Color := '#000000';
    end;
    case EntryType of
      lmetFatal: Formatting := 'b';
      lmetDebug, lmetglInfo: Formatting := 'i';
    end;
    
    if Length(MethodPart) = 0 then
      MethodPart := '<i>Global</i>'
    else
      Delete(MethodPart, 1, 1);

    S += '<tr><td><font color="'+Color+'">';
    if Formatting <> '' then
      S += '<' + Formatting + '>';
    S += EntryTypeToStr(EntryType);
    if Formatting <> '' then
      S += '</' + Formatting + '>';
    if HTML <> '' then
      S += '</font></td><td>'+MethodPart+'</td><td>'+HTML+'</td></tr>'
    else
      S += '</font></td><td>'+MethodPart+'</td><td>'+Text+'</td></tr>';
    
    FHTMLStream.Write(S[1], Length(S));
  end;
end;

procedure TlmLogFile.Log(const EntryType: TlmEntryType; const Text, SourceMethod: String);
begin
  SDL_LockMutex(FLogLevelMutex);
  SDL_LockMutex(FFileMutex);
  try
    nmLog(EntryType, Text, SourceMethod);
  finally
    SDL_UnlockMutex(FFileMutex);
    SDL_UnlockMutex(FLogLevelMutex);
  end;
end;

procedure TlmLogFile.Log(const EntryType: TlmEntryType; const Text,
  SourceMethod: String; TextParams: array of const);
begin
  Log(EntryType, Format(Text, TextParams), SourceMethod);
end;

procedure TlmLogFile.Log(const EntryType: TlmEntryType; const E: Exception;
  const SourceMethod: String; const ExtraMsg: String);
begin
  LogException(EntryType, E, SourceMethod, ExtraMsg);
end;

procedure TlmLogFile.LogFmt(const EntryType: TlmEntryType; const Text, SourceMethod: String; TextParams: array of const);
begin
  Log(EntryType, Format(Text, TextParams), SourceMethod);
end;

function Sort(List: TStringList; I1, I2: Integer): Integer;
var
  Item1, Item2: String;
  NS1, NS2: String;
  Ext1, Ext2: String;
begin
  Item1 := List[I1];
  Item2 := List[I2];
  NS1 := Divide(Item1, 0, '_');
  NS2 := Divide(Item2, 0, '_');
  Ext1 := Divide(Item1, 1, '_');
  Ext2 := Divide(Item2, 1, '_');
  
  if (NS1 = 'GL') and (NS2 = 'WGL') then
  begin
    Result := -1;
    Exit;
  end
  else if (NS1 = 'WGL') and (NS2 = 'GL') then
  begin
    Result := 1;
    Exit;
  end
  else if (Ext1 = 'ARB') then
  begin
    if (Ext2 = 'ARB') then
      Result := CompareStr(Item1, Item2)
    else
      Result := -1;
  end
  else if (Ext1 = 'EXT') then
  begin
    if (Ext2 = 'ARB') then
      Result := 1
    else if (Ext2 = 'EXT') then
      Result := CompareStr(Item1, Item2)
    else
      Result := -1;
  end
  else
  begin
    if (Ext2 = 'ARB') or (Ext2 = 'EXT') then
      Result := 1
    else
      Result := CompareStr(Item1, Item2);
  end;
end;

procedure TlmLogFile.LogException(const EntryType: TlmEntryType;
  const E: Exception; const SourceMethod: String; const ExtraMsg: String = '');
var
  AExtraMsg: String;
  TextMsg, HTMLMsg: String;
  LastMsgChar: Char;
begin
  if E = nil then
    Exit;
  SDL_LockMutex(FLogLevelMutex);
  SDL_LockMutex(FFileMutex);
  try
    AExtraMsg := ExtraMsg;
    if Length(AExtraMsg) > 0 then
    begin
      if AExtraMsg[1] <> ' ' then
        AExtraMsg := ' '+AExtraMsg;
      LastMsgChar := AExtraMsg[Length(AExtraMsg)];
      if LastMsgChar = '.' then
        AExtraMsg[Length(AExtraMsg)] := ':'
      else if LastMsgChar <> ':' then
        AExtraMsg := AExtraMsg + ':';
    end;
    TextMsg := 'Exception occured'+AExtraMsg+' Exception class = '+E.ClassName+'; Message = '+E.Message;
    HTMLMsg := '<b>Exception occured'+AExtraMsg+'</b><br/>Exception class: '+E.ClassName+'<br/>Message: '+E.Message;
    
    nmLog(EntryType, TextMsg, SourceMethod, HTMLMsg);
  finally
    SDL_UnlockMutex(FLogLevelMutex);
    SDL_UnlockMutex(FFileMutex);
  end;
end;

procedure TlmLogFile.OutputExtensions;
var
  Extensions: String;
  List: TStringList;
  I: Integer;
  LastClass, LastType, StyleExt: String;
  CurrentExt, CurrentClass, CurrentType, CurrentName: String;
  FinalHTML: String;
  ARBs, EXTs, ATIs, NVs, Others: Integer;
begin
  SDL_LockMutex(FLogLevelMutex);
  SDL_LockMutex(FFileMutex);
  try
    Extensions := StrPas(glGetString(GL_EXTENSIONS));
    List := TStringList.Create;
    try
      Explode(Extensions, List, ' ');
      List.CustomSort(@Sort);
      FinalHTML := '<table width="100%" style="border-top: none; margin-top: -2px; margin-bottom: -2px"><tr><th width="25px">Class</th><th width="25px">Level</th><th>Name</th></tr>';
      LastClass := '';
      LastType := '';
      ARBs := 0;
      EXTs := 0;
      NVs := 0;
      ATIs := 0;
      Others := 0;
      for I := 0 to List.Count - 1 do
      begin
        CurrentExt := List[I];
        CurrentClass := Divide(CurrentExt, 0, '_');
        CurrentType := Divide(CurrentExt, 1, '_');
        CurrentName := CurrentExt;
        Delete(CurrentName, 1, Length(CurrentClass) + Length(CurrentType) + 2);
        
        if CurrentType = 'ARB' then
          Inc(ARBs)
        else if CurrentType = 'EXT' then
          Inc(EXTs)
        else if CurrentType = 'NV' then
          Inc(NVs)
        else if CurrentType = 'ATI' then
          Inc(ATIs)
        else
          Inc(Others);

        if (LastType = 'EXT') and ((CurrentType <> 'EXT') and (CurrentType <> 'ARB')) then
          StyleExt := ' style="border-top: 3px solid #000000"'
        else if ((LastClass <> CurrentClass) or (LastType <> CurrentType)) and (LastClass <> '') and (LastType <> '') then
          StyleExt := ' style="border-top: 2px solid #000000"'
        else
          StyleExt := '';
        FinalHTML += '<tr'+StyleExt+'><td>'+CurrentClass+'</td><td>'+CurrentType+'</td><td>'+CurrentName+'</td></tr>';
        
        LastClass := CurrentClass;
        LastType := CurrentType;
      end;
      FinalHTML += '</table><br/>'+Format('<b>ARBs: </b>%d&nbsp;&nbsp;&nbsp;<b>EXTs: </b>%d&nbsp;&nbsp;&nbsp;<b>NVs: </b>%d&nbsp;&nbsp;&nbsp;<b>ATIs: </b>%d&nbsp;&nbsp;&nbsp;<b>Others: </b>%d', [ARBs, EXTs, NVs, ATIs, Others]);
      nmLog(lmetglInfo, Extensions + #13#10 + Format('ARBs: %d; EXTs: %d; NVs: %d; ATIs: %d; Others: %d', [ARBs, EXTs, NVs, ATIs, Others]), 'Extensions', FinalHTML);
    finally
      List.Free;
    end;
  finally
    SDL_UnlockMutex(FFileMutex);
    SDL_UnlockMutex(FLogLevelMutex);
  end;
end;

procedure TlmLogFile.ClearLog;
begin
  SDL_LockMutex(FFileMutex);
  try
    FFileStream.Free;
    FFileStream.Create(FFileName, fmCreate);
  finally
    SDL_UnlockMutex(FFileMutex);
  end;
end;

finalization
WriteLn('Freeing general log');
FreeAndNil(GeneralLog);
WriteLn('Done');

end.

