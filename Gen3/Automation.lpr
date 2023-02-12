program Automation;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, netcard, cutypes, cmdparse, suncalc,
  DateUtils, Math, opensslsockets, ecweather, forecast, logger, kutils,
  urllib, jsonparser, fpjson{$IFDEF UNIX}, signals{$ENDIF}
  { you can add units after this };

type

  { TAutomation }

  TAutomation = class(TCustomApplication)
  private
    FCard: TNetCard;
    FBlockID, FLightBlk: integer;
    FBlock: TMemoryStream;
    FSyncLock: Boolean;
    procedure CUGet(const uri: string);
    procedure PullFlags;
    procedure InitBlock(info: PBlockInfo);
    procedure ReadState;
    procedure WriteState;
    procedure ProcessSync(card, blkid: integer);
    procedure AppLoop;
    procedure Say(msg: string);
    procedure Log(msg: string);
    procedure UpdateSun;
    procedure UpdateWeather;
    procedure LoadLightData;
    procedure WriteLightData;
    procedure DayMode;
    procedure DarkMode;
    procedure BrightMode;
    procedure DoWakeMe;
    {$IFDEF UNIX}
    procedure HandleSignal;
    {$ENDIF UNIX}
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

const
  WAKEUP_MSG = 'Wake up Kevin, it is time to keep your schedule.';

{ TAutomation }

procedure TAutomation.CUGet(const uri: string);
var
  r: string;
begin
  r:=URLGet('http://localhost:8080/'+uri);
end;

procedure TAutomation.PullFlags;
var
  json: TJSONObject;
begin
  json:=TJSONObject(GetJSON(URLGet('https://***REDACTED***/config?calls=no')));
  if json.Booleans['HOME'] then
    CUSettings^.flags:=CUSettings^.flags+[afHome]
  else
    CUSettings^.flags:=CUSettings^.flags-[afHome];
  if json.Booleans['SLEEPING'] then
    CUSettings^.flags:=CUSettings^.flags+[afSleeping]
  else
    CUSettings^.flags:=CUSettings^.flags-[afSleeping]
end;

procedure TAutomation.InitBlock(info: PBlockInfo);
begin
  LogInfo('Initializing Home Automation Memory Block...');
  FBlockID:=FCard.FindFree;
  { TODO : Should check to see if it returns a 0. }
  FBlock:=FCard.ReadBlock(FBlockID);
  info^.title:='Home Automation';
  info^.appno:=$50;
  info^.typno:=CU_TYPNO;
  info^.nextid:=0;
  info^.total:=SizeOf(TCUSettings);
  PullFlags;
  CUSettings^.running:=True;
  CUSettings^.vmrss:=VmRSS;
  FBlock.Write(CUSettings^, SizeOf(CUSettings^));
  FCard.WriteBlock(FBlockID, FBlock, info);
end;

procedure TAutomation.ReadState;
begin
  {$IFDEF DEBUG}LogInfo('Reading State...');{$ENDIF}
  if Assigned(FBlock) then
    FBlock.Free;
  FBlock:=FCard.ReadBlock(FBlockID);
  FBlock.Read(CUSettings^, SizeOf(CUSettings^));
end;

procedure TAutomation.WriteState;
begin
  {$IFDEF DEBUG}LogInfo('Writing State...');{$ENDIF}
  if not Assigned(FBlock) then
    Exit;
  CUSettings^.vmrss:=VmRSS;
  FBlock.Position:=0;
  FBlock.Write(CUSettings^, SizeOf(CUSettings^));
  FCard.WriteBlock(FBlockID, FBlock);
end;

procedure TAutomation.ProcessSync(card, blkid: integer);
var
  flags: TAutomationSettings;
  r: string;
begin
  {$IFDEF DEBUG}LogInfo('ProcessSync...');{$ENDIF}
  if FSyncLock then
  begin
    FSyncLock:=False;
    Exit;
  end;
  flags:=CUSettings^.flags;
  ReadState;
  if (afHome in CUSettings^.flags) and not (afHome in flags) then
    r:=URLGet('https://***REDACTED***/home_mode')
  else if (afHome in flags) and not (afHome in CUSettings^.flags) then
    r:=URLGet('https://***REDACTED***/away_mode');
  if (afSleeping in CUSettings^.flags) and not (afSleeping in flags) then
    r:=URLGet('https://***REDACTED***/sleeping_mode')
  else if (afSleeping in flags) and not (afSleeping in CUSettings^.flags) then
    r:=URLGet('https://***REDACTED***/awake_mode');
end;

procedure TAutomation.AppLoop;
var
  done, hourly: Boolean;
begin
  hourly:=False;
  Repeat
    Sleep(500);
    if (MinuteOf(Now) = 15) and (hourly = False) then
    begin
      hourly:=True;
      UpdateWeather;
    end;
    if (MinuteOf(Now) = 14) and (hourly = True) then
      hourly:=False;
    if CompareTime(Now, CUSettings^.sunset) = GreaterThanValue then
    begin
      if not (afDark in CUSettings^.flags) then
      begin
        CUSettings^.flags:=CUSettings^.flags+[afDark];
        if not (afSleeping in CUSettings^.flags) then
        begin
          CUSettings^.say:='It is after sunset Kevin, setting dark mode.';
          DarkMode;
        end;
        CUSettings^.log:='Setting dark mode.';
        WriteState;
      end;
    end
    else if CompareTime(Now, CUSettings^.sunrise) = GreaterThanValue then
    begin
      if afDark in CUSettings^.flags then
      begin
        CUSettings^.flags:=CUSettings^.flags-[afDark];
        if not (afSleeping in CUSettings^.flags) then
          CUSettings^.say:='It is now sunrise Kevin, turning off lights.';
        CUSettings^.log:='Setting day mode.';
        DayMode;
        WriteState;
      end;
    end;
    if (afWakeMe in CUSettings^.flags) and (afHome in CUSettings^.flags) then
      DoWakeMe;
    FCard.CheckEvents(Self, done);
  until not CUSettings^.running;
end;

procedure TAutomation.Say(msg: string);
begin
  CUSettings^.say:=msg;
  WriteState;
end;

procedure TAutomation.Log(msg: string);
begin
  CUSettings^.log:=msg;
  WriteState;
end;

procedure TAutomation.UpdateSun;
begin
  LogInfo('Performing calculation for Sunrise and Sunset...');
  CalcSunriseSet(51.0486, -114.0708, -7, CUSettings^.sunrise, CUSettings^.sunset);
  WriteState;
end;

procedure TAutomation.UpdateWeather;
var
  info: PBlockInfo;
  blkid: integer;
  blk: TMemoryStream;
  weather: PWeatherData;
begin
  LogInfo('Updating the weather in the Memory Card Server...');
  New(info);
  New(weather);
  try
    blkid:=FCard.FindType(WEATHER_TYPNO, info);
    if blkid = 0 then
    begin
      info^.title:='Calgary Weather';
      info^.appno:=$50;
      info^.typno:=WEATHER_TYPNO;
      info^.nextid:=0;
      info^.total:=SizeOf(weather^);
      blkid:=FCard.FindFree;
      blk:=FCard.ReadBlock(blkid);
      blk.Write(weather_cache.weather^, SizeOf(weather_cache.weather^));
      FCard.WriteBlock(blkid, blk, info);
      Exit;
    end;
    blk:=FCard.ReadBlock(blkid);
    blk.Read(weather^, SizeOf(weather^));
    if (weather_cache.weather^.temperature <> weather^.temperature) or
       (weather_cache.weather^.conditions <> weather^.conditions) then
    begin
      LogInfo('Weather Forecast changed, updating...');
      blk.Position:=0;
      blk.Write(weather_cache.weather^, SizeOf(weather_cache.weather^));
      FCard.WriteBlock(blkid, blk, info);
    end;
  finally
    blk.Free;
    Dispose(weather);
    Dispose(info);
  end;
end;

procedure TAutomation.LoadLightData;
var
  blk: TMemoryStream;
begin
  LogInfo('Loading Light Data...');
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

procedure TAutomation.WriteLightData;
var
  blk: TMemoryStream;
begin
  if LightSettings = Nil then
    LoadLightData;
  LogInfo('Writing Light Data...');
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

procedure TAutomation.DayMode;
var
  i: integer;
begin
  for i:=Low(Lights) to High(Lights) do
    Lights[i].lit:=False;
  WriteLightData;
end;

procedure TAutomation.DarkMode;
var
  i, lgt: Integer;
begin
  for i:=Low(Lights) to High(Lights) do
    if LightNames[i] = 'LightStrips' then
      Lights[i].lit:=True;
  WriteLightData;
end;

procedure TAutomation.BrightMode;
var
  i: integer;
begin
  for i:=Low(Lights) to High(Lights) do
    with Lights[i] do
    begin
      lit:=True;
      hue:=34494;
      bri:=254;
      sat:=232;
    end;
  WriteLightData;
end;

procedure TAutomation.DoWakeMe;
var
  hr, wd: word;
  relax_hr, wakeup_hr, work_hr: word;
begin
  hr:=HourOf(Now);
  wd:=DayOfTheWeek(Now);
  relax_hr:=21;
  wakeup_hr:=7;
  work_hr:=8;
  if (wd = 5) or (wd = 6) then
    wakeup_hr:=9;
  if (afSleeping in CUSettings^.flags) and (hr = wakeup_hr) then
  begin
    if afDark in CUSettings^.flags then
      BrightMode;
    if CUSettings^.say <> WAKEUP_MSG then
      Say(WAKEUP_MSG);
  end;
end;

{$IFDEF UNIX}
procedure TAutomation.HandleSignal;
begin
  if Assigned(CUSettings) then
    CUSettings^.running:=False
  else
    Halt(2);
end;
{$ENDIF}

procedure TAutomation.DoRun;
var
  info: PBlockInfo;
begin
  SetupLog('Automation.log');
  LogInfo('Automation Server starting...');
  LogInfo('Memory Usage before doing much: '+VmRSS);
  InitConfig(Self);

  LogInfo('Connecting to Memory Card Server...');
  FCard:=TNetCard.Create(CmdParams^.server, CmdParams^.port);
  FCard.Authenticate(CmdParams^.key);
  FCard.SelectCard(CmdParams^.card);

  New(CUSettings);

  New(info);
  LogInfo('Searching Memory Card Server for Settings...');
  FBlockID:=FCard.FindType(CU_TYPNO, info);
  if FBlockID = 0 then
    InitBlock(info)
  else
    ReadState;
  FLightBlk:=FCard.FindType(LIGHT_TYPNO, info);
  Dispose(info);

  if not CUSettings^.running then
  begin
    CUSettings^.running:=True;
    WriteState;
  end;

  FCard.OnSync:=@ProcessSync;
  FSyncLock:=False;

  {$IFDEF UNIX}OnSignal:=@HandleSignal;{$ENDIF}

  try
    UpdateSun;
    UpdateWeather;
    LoadLightData;
    LogInfo('Subscribing to Home Automation Settings...');
    FCard.Subscribe(FBlockID);
    AppLoop;
  finally
    LogInfo('AppLoop Ended.');
    if Assigned(FBlock) then
      FBlock.Free;
    FCard.Free;
    Dispose(CUSettings);
  end;

  if Assigned(LightSettings) then
    Dispose(LightSettings);

  // stop program loop
  Terminate;
end;

constructor TAutomation.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TAutomation.Destroy;
begin
  inherited Destroy;
end;

procedure TAutomation.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TAutomation;
begin
  Application:=TAutomation.Create(nil);
  Application.Title:='Home Automation';
  Application.Run;
  Application.Free;
end.

