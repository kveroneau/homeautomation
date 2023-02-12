unit cuclient;

{$mode objfpc}{$H+}
{$DEFINE DEBUG}

interface

uses
  Classes, SysUtils, cutypes, netcard, forecast;

type

  EMissingData = class(Exception);

  TSayEvent = procedure(const message: String) of Object;
  TLogEvent = procedure(const message: String) of Object;

  { THomeCU }

  THomeCU = class(TComponent)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect;
    procedure CheckEvents;
    procedure Strips;
    procedure AllOff;
    procedure LivingRoom;
    procedure Brightness(value: word);
    procedure Hue(hue: word);
    procedure AllBright;
    procedure Relax;
    procedure Bathroom;
    procedure Bedroom;
    procedure Blacklight;
    procedure Study;
    procedure Entrance;
    procedure Say(const msg: string);
    procedure Log(const msg: string);
    procedure StopCU;
  private
    FCard: TNetCard;
    FBlock: TMemoryStream;
    FLightBlk, FCUBlk: integer;
    FRunning: Boolean;
    FOnSync: TNotifyEvent;
    FOnLog: TLogEvent;
    FOnSay: TSayEvent;
    FLastLog, FLastSay: string;
    FLoadAll: Boolean;
    FWeather: PWeatherData;
    procedure ProcessSync(card, blkid: integer);
    procedure WriteState;
    procedure DoSync;
    procedure DoLog;
    procedure DoSay;
    function GetHomeMode: Boolean;
    procedure SetHomeMode(value: Boolean);
    function GetSleepMode: Boolean;
    procedure SetSleepMode(value: Boolean);
    function GetDarkMode: Boolean;
    function GetWakeMe: Boolean;
    procedure SetWakeMe(value: Boolean);
    function GetPaused: Boolean;
    procedure SetPaused(value: Boolean);
    function GetRooms: Boolean;
    procedure SetRooms(value: Boolean);
    function GetSunrise: TDateTime;
    function GetSunset: TDateTime;
    procedure LoadLightData;
    procedure WriteLightData;
    procedure ToggleLight(const lgt: integer);
    procedure LoadForecastData;
  public
    property OnSync: TNotifyEvent read FOnSync write FOnSync;
    property OnLog: TLogEvent read FOnLog write FOnLog;
    property OnSay: TSayEvent read FOnSay write FOnSay;
    property Running: Boolean read FRunning write FRunning;
    property HomeMode: Boolean read GetHomeMode write SetHomeMode;
    property SleepMode: Boolean read GetSleepMode write SetSleepMode;
    property DarkMode: Boolean read GetDarkMode;
    property WakeMe: Boolean read GetWakeMe write SetWakeMe;
    property Paused: Boolean read GetPaused write SetPaused;
    property Rooms: Boolean read GetRooms write SetRooms;
    property Sunrise: TDateTime read GetSunrise;
    property Sunset: TDateTime read GetSunset;
    property LoadAll: Boolean write FLoadAll;
    property Weather: PWeatherData read FWeather;
    property Card: TNetCard read FCard;
  end;

implementation

const
  MC_SERVER = 'localhost';
  MC_PORT = 3845;
  MC_KEY = 'TestKey123';

{ THomeCU }

constructor THomeCU.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCard:=Nil;
  FBlock:=Nil;
  FOnSync:=Nil;
  FLoadAll:=False;
  FWeather:=Nil;
end;

destructor THomeCU.Destroy;
begin
  if Assigned(FBlock) then
    FBlock.Free;
  if Assigned(FCard) then
    FCard.Free;
  if CUSettings <> Nil then
    Dispose(CUSettings);
  if LightSettings <> Nil then
  begin
    Dispose(LightSettings);
    SetLength(Lights, 0);
  end;
  if FWeather <> Nil then
    Dispose(FWeather);
  inherited Destroy;
end;

procedure THomeCU.Connect;
var
  info: PBlockInfo;
begin
  FCard:=TNetCard.Create(MC_SERVER, MC_PORT);
  FCard.Authenticate(MC_KEY);
  FCard.SelectCard(0);

  New(info);
  FLightBlk:=FCard.FindType(LIGHT_TYPNO, info);
  FCUBlk:=FCard.FindType(CU_TYPNO, info);
  Dispose(info);
  if (FLightBlk = 0) or (FCUBlk = 0) then
    raise EMissingData.Create('Control Unit blocks missing from memory card.');

  FBlock:=FCard.ReadBlock(FCUBlk);
  New(CUSettings);
  FBlock.Read(CUSettings^, SizeOf(CUSettings^));
  FLastLog:=CUSettings^.log;
  FLastSay:=CUSettings^.say;
  FCard.OnSync:=@ProcessSync;
  FCard.Subscribe(FCUBlk);

  if FLoadAll then
  begin
    LoadLightData;
    LoadForecastData;
  end;

  DoSync;

  FRunning:=True;
end;

procedure THomeCU.CheckEvents;
var
  done: Boolean;
begin
  FCard.CheckEvents(Self, done);
end;

procedure THomeCU.Strips;
begin
  ToggleLight(6);
end;

procedure THomeCU.AllOff;
var
  i: integer;
begin
  LoadLightData;
  for i:=1 to LightSettings^.count do
    Lights[i].lit:=False;
  WriteLightData;
end;

procedure THomeCU.LivingRoom;
begin
  LoadLightData;
  if Lights[4].lit then
  begin
    Lights[4].lit:=False;
    Lights[8].lit:=False;
    Lights[10].lit:=False;
  end
  else
  begin
    Lights[4].lit:=True;
    Lights[8].lit:=True;
    Lights[10].lit:=True;
  end;
  WriteLightData;
end;

procedure THomeCU.Brightness(value: word);
var
  i: integer;
begin
  LoadLightData;
  for i:=1 to LightSettings^.count do
    if Lights[i].lit then
      Lights[i].bri:=value;
  WriteLightData;
end;

procedure THomeCU.Hue(hue: word);
var
  i: integer;
begin
  LoadLightData;
  for i:=1 to LightSettings^.count do
    if Lights[i].lit then
      Lights[i].hue:=hue;
  WriteLightData;
end;

procedure THomeCU.AllBright;
var
  i: integer;
begin
  LoadLightData;
  for i:=1 to LightSettings^.count do
    with Lights[i] do
    begin
      lit:=True;
      bri:=254;
      hue:=34494;
      sat:=232;
    end;
  WriteLightData;
end;

procedure THomeCU.Relax;
var
  i: integer;
begin
  LoadLightData;
  for i:=1 to LightSettings^.count do
    if Lights[i].lit then
      Lights[i].hue:=8088;
  WriteLightData;
end;

procedure THomeCU.Bathroom;
begin
  ToggleLight(5);
end;

procedure THomeCU.Bedroom;
begin
  ToggleLight(1);
end;

procedure THomeCU.Blacklight;
var
  i: integer;
begin
  LoadLightData;
  for i:=1 to LightSettings^.count do
    if Lights[i].lit then
    begin
      Lights[i].hue:=47417;
      Lights[i].sat:=241;
      Lights[i].bri:=233;
    end;
  WriteLightData;
end;

procedure THomeCU.Study;
begin
  ToggleLight(2);
end;

procedure THomeCU.Entrance;
begin

end;

procedure THomeCU.Say(const msg: string);
begin
  FLastSay:=msg;
  CUSettings^.say:=msg;
  WriteState;
end;

procedure THomeCU.Log(const msg: string);
begin
  FLastLog:=msg;
  CUSettings^.log:=msg;
  WriteState;
end;

procedure THomeCU.StopCU;
begin
  CUSettings^.running:=False;
  WriteState;
end;

procedure THomeCU.ProcessSync(card, blkid: integer);
begin
  if blkid <> FCUBlk then
    Exit;
  if Assigned(FBlock) then
    FBlock.Free;
  FBlock:=FCard.ReadBlock(FCUBlk);
  FBlock.Read(CUSettings^, SizeOf(CUSettings^));
  if CUSettings^.log <> FLastLog then
  begin
    FLastLog:=CUSettings^.log;
    DoLog;
  end;
  if CUSettings^.say <> FLastSay then
  begin
    FLastSay:=CUSettings^.say;
    DoSay;
  end;
  DoSync;
end;

procedure THomeCU.WriteState;
begin
  if Assigned(FBlock) then
    FBlock.Free;
  try
    FBlock:=FCard.ReadBlock(FCUBlk);
  except
    On EAuthError do FBlock:=Nil;
  end;
  if not Assigned(FBlock) then
    FBlock:=FCard.ReadBlock(FCUBlk);
  FBlock.Write(CUSettings^, SizeOf(CUSettings^));
  FCard.WriteBlock(FCUBlk, FBlock);
end;

procedure THomeCU.DoSync;
begin
  if Assigned(FOnSync) then
    FOnSync(Self);
end;

procedure THomeCU.DoLog;
begin
  if Assigned(FOnLog) then
    FOnLog(CUSettings^.log);
end;

procedure THomeCU.DoSay;
begin
  if Assigned(FOnSay) then
    FOnSay(CUSettings^.say);
end;

function THomeCU.GetHomeMode: Boolean;
begin
  if afHome in CUSettings^.flags then
    Result:=True
  else
    Result:=False;
end;

procedure THomeCU.SetHomeMode(value: Boolean);
begin
  if value then
    CUSettings^.flags:=CUSettings^.flags+[afHome]
  else
    CUSettings^.flags:=CUSettings^.flags-[afHome];
  WriteState;
end;

function THomeCU.GetSleepMode: Boolean;
begin
  if afSleeping in CUSettings^.flags then
    Result:=True
  else
    Result:=False;
end;

procedure THomeCU.SetSleepMode(value: Boolean);
begin
  if value then
    CUSettings^.flags:=CUSettings^.flags+[afSleeping]
  else
    CUSettings^.flags:=CUSettings^.flags-[afSleeping];
  WriteState;
end;

function THomeCU.GetDarkMode: Boolean;
begin
  if afDark in CUSettings^.flags then
    Result:=True
  else
    Result:=False;
end;

function THomeCU.GetWakeMe: Boolean;
begin
  if afWakeMe in CUSettings^.flags then
    Result:=True
  else
    Result:=False;
end;

procedure THomeCU.SetWakeMe(value: Boolean);
begin
  if value then
    CUSettings^.flags:=CUSettings^.flags+[afWakeMe]
  else
    CUSettings^.flags:=CUSettings^.flags-[afWakeMe];
  WriteState;
end;

function THomeCU.GetPaused: Boolean;
begin
  if afPaused in CUSettings^.flags then
    Result:=True
  else
    Result:=False;
end;

procedure THomeCU.SetPaused(value: Boolean);
begin
  if value then
    CUSettings^.flags:=CUSettings^.flags+[afPaused]
  else
    CUSettings^.flags:=CUSettings^.flags-[afPaused];
  WriteState;
end;

function THomeCU.GetRooms: Boolean;
begin
  if afRooms in CUSettings^.flags then
    Result:=True
  else
    Result:=False;
end;

procedure THomeCU.SetRooms(value: Boolean);
begin
  if value then
    CUSettings^.flags:=CUSettings^.flags+[afRooms]
  else
    CUSettings^.flags:=CUSettings^.flags-[afRooms];
  WriteState;
end;

function THomeCU.GetSunrise: TDateTime;
begin
  Result:=CUSettings^.sunrise;
end;

function THomeCU.GetSunset: TDateTime;
begin
  Result:=CUSettings^.sunset;
end;

procedure THomeCU.LoadLightData;
var
  blk: TMemoryStream;
begin
  if LightSettings = Nil then
    New(LightSettings);
  blk:=FCard.ReadBlock(FLightBlk);
  try
    blk.Read(LightSettings^, SizeOf(LightSettings^));
    SetLength(LightNames, LightSettings^.count+1);
    blk.Read(LightNames[0], 21*(LightSettings^.count+1));
    SetLength(Lights, LightSettings^.count+1);
    blk.Read(Lights[0], SizeOf(TLight)*(LightSettings^.count+1));
  finally
    blk.Free;
  end;
end;

procedure THomeCU.WriteLightData;
var
  blk: TMemoryStream;
begin
  if LightSettings = Nil then
    LoadLightData;
  blk:=FCard.ReadBlock(FLightBlk);
  try
    blk.Write(LightSettings^, SizeOf(LightSettings^));
    blk.Write(LightNames[0], 21*(LightSettings^.count+1));
    blk.Write(Lights[0], SizeOf(TLight)*(LightSettings^.count+1));
    FCard.WriteBlock(FLightBlk, blk);
  finally
    blk.Free;
  end;
end;

procedure THomeCU.ToggleLight(const lgt: integer);
begin
  LoadLightData;
  if Lights[lgt].lit then
    Lights[lgt].lit:=False
  else
    Lights[lgt].lit:=True;
  WriteLightData;
end;

procedure THomeCU.LoadForecastData;
var
  blk: TMemoryStream;
  blkid: integer;
  info: TBlockInfo;
begin
  if FWeather = Nil then
    New(FWeather);
  blkid:=FCard.FindType(WEATHER_TYPNO, @info);
  blk:=FCard.ReadBlock(blkid);
  try
    blk.Read(FWeather^, SizeOf(FWeather^));
  finally
    blk.Free;
  end;
end;

end.

