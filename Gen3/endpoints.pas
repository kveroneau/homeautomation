unit endpoints;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, fphttpapp, netcard, cutypes,
  fpjson, jsonparser, cmdparse, forecast, logger, kutils, DateUtils;

type

  { TCUEndpoints }

  TCUEndpoints = class(TFPWebModule)
    procedure allbrightRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure bedroomRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure blacklightRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure configRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure CUShutdownRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure entranceRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure healthRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure hoffRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure homeRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure livingRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure logRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure relaxRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure sayRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure studyRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    FCard: TNetCard;
    FBlock, FCUBlock: TMemoryStream;
    FBlockID, FCUBlkID, FWeatherID: integer;
    FLightNames: TStringList;
    FWeatherData: PWeatherData;
    procedure PullState;
    procedure PushState;
    procedure PushSettings;
    procedure ProcessSync(card, blkid: integer);
    procedure IdleChecker(Sender: TObject);
    procedure SetLight(const light_name: string; const lit: Boolean);
    procedure SetLight(const light_name: string; const lit: Boolean; const hue, bri, sat: integer);
    function LightOn(const light_name: string): Boolean;
    procedure SetLight(lgt: PLight; const lit: Boolean; const hue, bri, sat: integer);
    procedure EnsureLight(const light_name: string);
  public

  end;

var
  CUEndpoints: TCUEndpoints;

implementation

{$R *.lfm}

{ TCUEndpoints }

procedure TCUEndpoints.allbrightRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  i: integer;
begin
  for i:=1 to LightSettings^.count do
    SetLight(@Lights[i], True, 34494, 254, 232);
  PushState;
  AResponse.Content:='OK';
  Handled:=True;
end;

procedure TCUEndpoints.bedroomRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  hr: word;
begin
  if LightOn('Bedroom') then
    SetLight('Bedroom', False)
  else
  begin
    EnsureLight('Bedroom');
    hr:=HourOf(Now);
    if (hr > 20) or (hr < 6) then
      SetLight('Bedroom', True, 8088, 100, 254)
    else
      SetLight('Bedroom', True, 34494, 254, 232);
  end;
  PushState;
  AResponse.Content:='OK';
  Handled:=True;
end;

procedure TCUEndpoints.blacklightRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  i: integer;
begin
  for i:=1 to LightSettings^.count do
    if Lights[i].lit then
      SetLight(@Lights[i], True, 47417, 233, 241);
  PushState;
  AResponse.Content:='OK';
  Handled:=True;
end;

procedure TCUEndpoints.configRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  json: TJSONObject;
begin
  LogInfo('Config Request from '+ARequest.RemoteAddr);
  AResponse.ContentType:='application/json';
  json:=TJSONObject.Create;
  try
    json.Add('dark', afDark in CUSettings^.flags);
    json.Add('home', afHome in CUSettings^.flags);
    json.Add('sleeping', afSleeping in CUSettings^.flags);
    AResponse.Content:=json.AsJSON;
  finally
    json.Free;
  end;
  Handled:=True;
end;

procedure TCUEndpoints.CUShutdownRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  LogInfo('CUShutdown Request from '+ARequest.RemoteAddr);
  {$IFDEF DEBUG}
  if Assigned(LightSettings) then
  begin
    LightSettings^.running:=False;
    PushState;
  end;
  {$ENDIF}
  Application.Terminate;
  AResponse.Content:='OK';
  Handled:=True;
end;

procedure TCUEndpoints.DataModuleCreate(Sender: TObject);
var
  info: PBlockInfo;
begin
  LogInfo('CUEndpoints DataModule Creating...');
  FLightNames:=Nil;
  FWeatherData:=Nil;
  FBlock:=Nil;
  FCUBlock:=Nil;
  LogInfo('Connecting to Memory Card Server...');
  FCard:=TNetCard.Create(CmdParams^.server, CmdParams^.port);
  FCard.Authenticate(CmdParams^.key);
  FCard.SelectCard(CmdParams^.card);
  New(info);
  LogInfo('Searching for Memory Card Blocks...');
  FBlockID:=FCard.FindType(LIGHT_TYPNO, info);
  FCUBlkID:=FCard.FindType(CU_TYPNO, info);
  FWeatherID:=FCard.FindType(WEATHER_TYPNO, info);
  Dispose(info);
  if (FBlockID = 0) or (FCUBlkID = 0) or (FWeatherID = 0) then
  begin
    LogError('Error locating required memory blocks!');
    FCard.Free;
    FCard:=Nil;
    Exit;
  end;
  PullState;
  FCard.OnSync:=@ProcessSync;
  FCard.Subscribe(FBlockID);
  FCard.Subscribe(FCUBlkID);
  FCard.Subscribe(FWeatherID);
  Application.AcceptIdleTimeout:=1000;
  Application.OnAcceptIdle:=@IdleChecker;
  LogInfo('CUEndpoints DataModule Created.');
end;

procedure TCUEndpoints.DataModuleDestroy(Sender: TObject);
begin
  if Assigned(FBlock) then
    FBlock.Free;
  if Assigned(FCUBlock) then
    FCUBlock.Free;
  if Assigned(FCard) then
    FCard.Free;
  if Assigned(LightSettings) then
    Dispose(LightSettings);
  if Assigned(CUSettings) then
    Dispose(CUSettings);
  if Assigned(FLightNames) then
    FLightNames.Free;
  if Assigned(FWeatherData) then
    Dispose(FWeatherData);
  LogInfo('CUEndpoints DataModule Destroyed.');
end;

procedure TCUEndpoints.entranceRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  if not (afHome in CUSettings^.flags) then
  begin
    AResponse.Content:='NO';
    Handled:=True;
    Exit;
  end;
  CUSettings^.say:='Kevin, here is the current forcast. '+FWeatherData^.forecast[0].outlook;
  PushSettings;
  FCard.CheckEvents(Self, Handled);
  homeRequest(Sender, ARequest, AResponse, Handled);
end;

procedure TCUEndpoints.healthRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  json, avail, forecast, outlook, config, memstat: TJSONObject;
begin
  LogInfo('Health Request from '+ARequest.RemoteAddr);
  AResponse.ContentType:='application/json';
  json:=TJSONObject.Create;
  try
    avail:=TJSONObject.Create;
    with avail do
    begin
      { TODO : Should be corrected sourced from a checker }
      Add('hue lights', True);
    end;
    json.Add('available', avail);
    json.Add('lights', TJSONArray.Create(['Study']));
    forecast:=TJSONObject.Create;
    outlook:=TJSONObject.Create;
    outlook.Add('outlook', FWeatherData^.forecast[0].outlook);
    outlook.Add('temperature', FWeatherData^.forecast[0].temperature);
    outlook.Add('title', FWeatherData^.forecast[0].title);
    forecast.Add('futurecast', TJSONArray.Create([outlook]));
    forecast.Add('conditions', FWeatherData^.conditions);
    forecast.Add('temperature', FWeatherData^.temperature);
    forecast.Add('feels_like', FWeatherData^.temperature);
    json.Add('weather', forecast);
    config:=TJSONObject.Create;
    config.Add('paused', afPaused in CUSettings^.flags);
    config.Add('sleeping', afSleeping in CUSettings^.flags);
    config.Add('dark', afDark in CUSettings^.flags);
    config.Add('home', afHome in CUSettings^.flags);
    config.Add('wakeme', afWakeMe in CUSettings^.flags);
    config.Add('rooms', afRooms in CUSettings^.flags);
    json.Add('config', config);
    memstat:=TJSONObject.Create;
    memstat.Add('endpoints', VmRSS);
    memstat.Add('lights', LightSettings^.vmrss);
    memstat.Add('automation', CUSettings^.vmrss);
    json.Add('vmrss', memstat);
    AResponse.Content:=json.AsJSON;
  finally
    json.Free;
  end;
  Handled:=True;
end;

procedure TCUEndpoints.hoffRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  i: integer;
begin
  for i:=1 to LightSettings^.count do
    if Lights[i].lit then
      Lights[i].lit:=False;
  PushState;
  AResponse.Content:='OK';
  Handled:=True;
end;

procedure TCUEndpoints.homeRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  SetLight('Study', True, 34494, 254, 232);
  if afDark in CUSettings^.flags then
  begin
    SetLight('Living room', True, 34494, 254, 232);
    SetLight('LightStrips', True, 34494, 254, 232);
    SetLight('Brazier', True, 34494, 254, 232);
    SetLight('Bedroom', True, 34494, 254, 232);
  end;
  PushState;
  AResponse.Content:='OK';
  Handled:=True;
end;

procedure TCUEndpoints.livingRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  hue, hr: word;
begin
  if not (afDark in CUSettings^.flags) then
  begin
    AResponse.Content:='NO';
    Handled:=True;
    Exit;
  end;
  hr:=HourOf(Now);
  if (hr > 19) or (hr < 6) then
    hue:=8088
  else
    hue:=34494;
  EnsureLight('Living room');
  SetLight('Living room', True, hue, 254, 232);
  SetLight('LightStrips', True, hue, 254, 232);
  SetLight('Brazier', True, hue, 254, 232);
  PushState;
  AResponse.Content:='OK';
  Handled:=True;
end;

procedure TCUEndpoints.logRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  CUSettings^.log:=ARequest.QueryFields.Values['entry'];
  PushSettings;
  AResponse.Content:='OK';
  Handled:=True;
end;

procedure TCUEndpoints.relaxRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  i: integer;
begin
  for i:=1 to LightSettings^.count do
    if Lights[i].lit then
      SetLight(@Lights[i], True, 13088, 144, 213);
  PushState;
  AResponse.Content:='OK';
  Handled:=True;
end;

procedure TCUEndpoints.sayRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  CUSettings^.say:=ARequest.Content;
  PushSettings;
  AResponse.Content:='OK';
  Handled:=True;
end;

procedure TCUEndpoints.studyRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  if LightOn('Study') then
    SetLight('Study', False)
  else
  begin
    EnsureLight('Study');
    SetLight('Study', True, 34494, 254, 232);
  end;
  PushState;
  AResponse.Content:='OK';
  Handled:=True;
end;

procedure TCUEndpoints.PullState;
var
  i: integer;
  blk: TMemoryStream;
begin
  LogInfo('Pulling State information from Memory Card...');
  if Assigned(FBlock) then
    FBlock.Free;
  FBlock:=FCard.ReadBlock(FBlockID);
  if not Assigned(LightSettings) then
    New(LightSettings);
  FBlock.Read(LightSettings^, SizeOf(LightSettings^));
  SetLength(LightNames, LightSettings^.count+1);
  FBlock.Read(LightNames[0], 21*(LightSettings^.count+1));
  SetLength(Lights, LightSettings^.count+1);
  FBlock.Read(Lights[0], SizeOf(TLight)*(LightSettings^.count+1));
  if not Assigned(FLightNames) then
  begin
    FLightNames:=TStringList.Create;
    for i:=0 to LightSettings^.count do
      FLightNames.Add(LightNames[i]);
  end;
  if Assigned(FCUBlock) then
    FCUBlock.Free;
  FCUBlock:=FCard.ReadBlock(FCUBlkID);
  if not Assigned(CUSettings) then
    New(CUSettings);
  FCUBlock.Read(CUSettings^, SizeOf(CUSettings^));
  blk:=FCard.ReadBlock(FWeatherID);
  try
    if not Assigned(FWeatherData) then
      New(FWeatherData);
    blk.Read(FWeatherData^, SizeOf(FWeatherData^));
  finally
    blk.Free;
  end;
end;

procedure TCUEndpoints.PushState;
begin
  LogInfo('Pushing State information to Memory Card...');
  if not Assigned(FBlock) then
    Exit;
  FBlock.Position:=0;
  FBlock.Write(LightSettings^, SizeOf(LightSettings^));
  FBlock.Write(LightNames[0], 21*(LightSettings^.count+1));
  FBlock.Write(Lights[0], SizeOf(TLight)*(LightSettings^.count+1));
  FCard.WriteBlock(FBlockID, FBlock);
end;

procedure TCUEndpoints.PushSettings;
begin
  LogInfo('Pushing Control Unit State to Memory Card...');
  if not Assigned(FCUBlock) then
    Exit;
  FCUBlock.Position:=0;
  FCUBlock.Write(CUSettings^, SizeOf(CUSettings^));
  FCard.WriteBlock(FCUBlkID, FCUBlock);
end;

procedure TCUEndpoints.ProcessSync(card, blkid: integer);
begin
  PullState;
end;

procedure TCUEndpoints.IdleChecker(Sender: TObject);
var
  done: Boolean;
begin
  {$IFDEF DEBUG}LogInfo('Idle checker...');{$ENDIF}
  FCard.CheckEvents(Sender, done);
end;

procedure TCUEndpoints.SetLight(const light_name: string; const lit: Boolean);
var
  lgt: integer;
begin
  lgt:=FLightNames.IndexOf(light_name);
  Lights[lgt].lit:=lit;
end;

procedure TCUEndpoints.SetLight(const light_name: string; const lit: Boolean;
  const hue, bri, sat: integer);
var
  lgt: integer;
begin
  lgt:=FLightNames.IndexOf(light_name);
  SetLight(@Lights[lgt], lit, hue, bri, sat);
end;

function TCUEndpoints.LightOn(const light_name: string): Boolean;
begin
  Result:=Lights[FLightNames.IndexOf(light_name)].lit;
end;

procedure TCUEndpoints.SetLight(lgt: PLight; const lit: Boolean;
  const hue, bri, sat: integer);
begin
  lgt^.lit:=lit;
  lgt^.hue:=hue;
  lgt^.bri:=bri;
  lgt^.sat:=sat;
end;

procedure TCUEndpoints.EnsureLight(const light_name: string);
var
  i, lgt: Integer;
begin
  lgt:=FLightNames.IndexOf(light_name);
  for i:=Low(Lights) to High(Lights) do
    if i = lgt then
      Lights[i].lit:=True
    else if not (afRooms in CUSettings^.flags) then
      Lights[i].lit:=False;
end;

initialization
  RegisterHTTPModule('cu', TCUEndpoints);
end.

