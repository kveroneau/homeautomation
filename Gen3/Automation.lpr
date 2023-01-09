program Automation;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, netcard, cutypes, cmdparse, suncalc,
  DateUtils, Math, opensslsockets, ecweather, forecast
  { you can add units after this };

type

  { TAutomation }

  TAutomation = class(TCustomApplication)
  private
    FCard: TNetCard;
    FBlockID: integer;
    FBlock: TMemoryStream;
    FSyncLock: Boolean;
    procedure InitBlock(info: PBlockInfo);
    procedure ReadState;
    procedure WriteState;
    procedure ProcessSync(card, blkid: integer);
    procedure AppLoop;
    procedure Say(msg: string);
    procedure Log(msg: string);
    procedure UpdateSun;
    procedure UpdateWeather;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TAutomation }

procedure TAutomation.InitBlock(info: PBlockInfo);
begin
  FBlockID:=FCard.FindFree;
  { TODO : Should check to see if it returns a 0. }
  FBlock:=FCard.ReadBlock(FBlockID);
  info^.title:='Home Automation';
  info^.appno:=$50;
  info^.typno:=CU_TYPNO;
  info^.nextid:=0;
  info^.total:=SizeOf(TCUSettings);
  CUSettings^.flags:=[afHome, afPaused];
  CUSettings^.running:=True;
  FBlock.Write(CUSettings^, SizeOf(CUSettings^));
  FCard.WriteBlock(FBlockID, FBlock, info);
end;

procedure TAutomation.ReadState;
begin
  if Assigned(FBlock) then
    FBlock.Free;
  FBlock:=FCard.ReadBlock(FBlockID);
  FBlock.Read(CUSettings^, SizeOf(CUSettings^));
end;

procedure TAutomation.WriteState;
begin
  if not Assigned(FBlock) then
    Exit;
  FBlock.Position:=0;
  FBlock.Write(CUSettings^, SizeOf(CUSettings^));
  FCard.WriteBlock(FBlockID, FBlock);
end;

procedure TAutomation.ProcessSync(card, blkid: integer);
begin
  {$IFDEF DEBUG}WriteLn('ProcessSync...');{$ENDIF}
  if FSyncLock then
  begin
    FSyncLock:=False;
    Exit;
  end;
  ReadState;
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
        CUSettings^.say:='Kevin, it is currently after sunset.';
        WriteState;
      end;
    end
    else if CompareTime(Now, CUSettings^.sunrise) = GreaterThanValue then
    begin
      if afDark in CUSettings^.flags then
      begin
        CUSettings^.flags:=CUSettings^.flags-[afDark];
        CUSettings^.say:='Kevin, it is now sunrise.';
        WriteState;
      end;
    end;
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
    if (weather_cache.weather^.temperature <> weather^.temperature) and
       (weather_cache.weather^.conditions <> weather^.conditions) then
    begin
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

procedure TAutomation.DoRun;
var
  info: PBlockInfo;
begin
  InitConfig(Self);

  FCard:=TNetCard.Create(CmdParams^.server, CmdParams^.port);
  FCard.Authenticate(CmdParams^.key);
  FCard.SelectCard(CmdParams^.card);

  New(CUSettings);

  New(info);
  FBlockID:=FCard.FindType(CU_TYPNO, info);
  if FBlockID = 0 then
    InitBlock(info)
  else
    ReadState;
  Dispose(info);

  if not CUSettings^.running then
  begin
    CUSettings^.running:=True;
    WriteState;
  end;

  FCard.OnSync:=@ProcessSync;
  FSyncLock:=False;

  try
    UpdateSun;
    UpdateWeather;
    FCard.Subscribe(FBlockID);
    AppLoop;
  finally
    WriteLn('AppLoop Ended.');
    if Assigned(FBlock) then
      FBlock.Free;
    FCard.Free;
    Dispose(CUSettings);
  end;


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

