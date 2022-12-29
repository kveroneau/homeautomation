unit endpoints;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, fphttpapp, netcard, cutypes,
  fpjson, jsonparser;

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
    procedure relaxRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure studyRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    FCard: TNetCard;
    FBlock: TMemoryStream;
    FBlockID: integer;
    FLightNames: TStringList;
    procedure PullState;
    procedure PushState;
    procedure ProcessSync(card, blkid: integer);
    procedure IdleChecker(Sender: TObject);
    procedure SetLight(const light_name: string; const lit: Boolean);
    procedure SetLight(const light_name: string; const lit: Boolean; const hue, bri, sat: integer);
    function LightOn(const light_name: string): Boolean;
    procedure SetLight(lgt: PLight; const lit: Boolean; const hue, bri, sat: integer);
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
begin
  if LightOn('Bedroom') then
    SetLight('Bedroom', False)
  else
    SetLight('Bedroom', True);
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
  AResponse.ContentType:='application/json';
  {with AResponse.Contents do
  begin
    Add('{"dark":true,"home":true,"sleeping":false}');
  end;}
  json:=TJSONObject.Create;
  json.Add('dark', True);
  json.Add('home', True);
  json.Add('sleeping', False);
  AResponse.Content:=json.AsJSON;
  Handled:=True;
end;

procedure TCUEndpoints.CUShutdownRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  if Assigned(LightSettings) then
  begin
    LightSettings^.running:=False;
    PushState;
  end;
  Application.Terminate;
  AResponse.Content:='OK';
  Handled:=True;
end;

procedure TCUEndpoints.DataModuleCreate(Sender: TObject);
var
  info: PBlockInfo;
begin
  FLightNames:=Nil;
  FCard:=TNetCard.Create('cherry.home.lan', 3845);
  FCard.Authenticate('HomeCU');
  FCard.SelectCard(2);
  New(info);
  FBlockID:=FCard.FindType(LIGHT_TYPNO, info);
  Dispose(info);
  if FBlockID = 0 then
  begin
    FCard.Free;
    FCard:=Nil;
    FBlock:=Nil;
    Exit;
  end;
  FBlock:=Nil;
  PullState;
  FCard.OnSync:=@ProcessSync;
  FCard.Subscribe(FBlockID);
  Application.AcceptIdleTimeout:=1000;
  Application.OnAcceptIdle:=@IdleChecker;
end;

procedure TCUEndpoints.DataModuleDestroy(Sender: TObject);
begin
  if Assigned(FBlock) then
    FBlock.Free;
  if Assigned(FCard) then
    FCard.Free;
  if Assigned(LightSettings) then
    Dispose(LightSettings);
  if Assigned(FLightNames) then
    FLightNames.Free;
end;

procedure TCUEndpoints.entranceRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  homeRequest(Sender, ARequest, AResponse, Handled);
end;

procedure TCUEndpoints.healthRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  AResponse.ContentType:='application/json';
  with AResponse.Contents do
  begin
    Add('{"available":');
    Add('{"upbox":true,"automation hud":true,"Veroneau dot net":true,');
    Add('"home proxy":true,"Canon printer":false,"bedroom hud":true,');
    Add('"living room hud":true,"Hacker''s Edge website":true,');
    Add('"hue lights":true},"lights": ["Study", "LightStrips"], "weather":');
    Add('{"futurecast": [{"outlook":"Outlook", "temperature":"7", "title": "Tonight"}],');
    Add('"conditions":"Sunny","temperature":"21.7","feels_like":"21.7"},');
    Add('"config":{');
    Add('"paused":false,"sleeping":false,"dark":true,"home":true,');
    Add('"wakeme":true,"rooms":false}}');
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
  SetLight('Living room', True, 34494, 254, 232);
  SetLight('LightStrips', True, 34494, 254, 232);
  SetLight('Brazier', True, 34494, 254, 232);
  SetLight('Bedroom', True, 34494, 254, 232);
  PushState;
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

procedure TCUEndpoints.studyRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  if LightOn('Study') then
    SetLight('Study', False)
  else
    SetLight('Study', True, 34494, 254, 232);
  PushState;
  AResponse.Content:='OK';
  Handled:=True;
end;

procedure TCUEndpoints.PullState;
var
  i: integer;
begin
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
end;

procedure TCUEndpoints.PushState;
begin
  if not Assigned(FBlock) then
    Exit;
  FBlock.Position:=0;
  FBlock.Write(LightSettings^, SizeOf(LightSettings^));
  FBlock.Write(LightNames[0], 21*(LightSettings^.count+1));
  FBlock.Write(Lights[0], SizeOf(TLight)*(LightSettings^.count+1));
  FCard.WriteBlock(FBlockID, FBlock);
end;

procedure TCUEndpoints.ProcessSync(card, blkid: integer);
begin
  PullState;
end;

procedure TCUEndpoints.IdleChecker(Sender: TObject);
var
  done: Boolean;
begin
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

initialization
  RegisterHTTPModule('cu', TCUEndpoints);
end.

