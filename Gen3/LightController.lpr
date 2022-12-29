program LightController;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, cutypes, phue, fpjson, memcard,
  netcard
  { you can add units after this };

type

  { TLightController }

  TLightController = class(TCustomApplication)
  private
    FHue: THueBridge;
    FLights: Array of TLight;
    FCard: TNetCard;
    FBlock: TMemoryStream;
    FBlockID: Integer;
    FSyncLock, FFading: Boolean;
    procedure AdaptState;
    procedure AdaptAndSync;
    procedure EnforceState;
    procedure WriteState;
    procedure ReadState;
    procedure ProcessSync(card, blkid: integer);
    procedure FadeLights;
    procedure AppLoop;
    procedure InitBlock(info: PBlockInfo);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TLightController }

procedure TLightController.AdaptState;
var
  lgt: TJSONObject;
  i: Integer;
begin
  {$IFDEF DEBUG}WriteLn('Adapting State...');{$ENDIF}
  SetLength(Lights, FHue.LightCount+1);
  for i:=1 to FHue.LightCount do
    with Lights[i] do
    begin
      lgt:=FHue.Lights[i];
      LightNames[i]:=lgt.Strings['name'];
      lit:=lgt.Objects['state'].Booleans['on'];
      bri:=lgt.Objects['state'].Integers['bri'];
      hue:=lgt.Objects['state'].Integers['hue'];
      sat:=lgt.Objects['state'].Integers['sat'];
    end;
  SetLength(FLights, FHue.LightCount+1);
  Move(Lights[0], FLights[0], SizeOf(TLight)*(FHue.LightCount+1));
end;

procedure TLightController.AdaptAndSync;
begin
  AdaptState;
  FSyncLock:=True;
  WriteState;
end;

procedure TLightController.EnforceState;
var
  lgt: TJSONObject;
  i: Integer;
begin
  {$IFDEF DEBUG}WriteLn('Enforcing State...');{$ENDIF}
  for i:=1 to FHue.LightCount do
    with Lights[i] do
    begin
      lgt:=FHue.Lights[i];
      if lit <> lgt.Objects['state'].Booleans['on'] then
      begin
        if lit = True then
          FHue.SetState(i, lit, bri, hue, sat)
        else
          FLights[i].lit:=True; { This will trigger the fade routine. }
      end;
      if bri <> lgt.Objects['state'].Integers['bri'] then
        FHue.SetState(i, lit, bri, hue, sat)
      else if hue <> lgt.Objects['state'].Integers['hue'] then
        FHue.SetState(i, lit, bri, hue, sat)
      else if sat <> lgt.Objects['state'].Integers['sat'] then
        FHue.SetState(i, lit, bri, hue, sat);
    end;
end;

procedure TLightController.WriteState;
begin
  FBlock.Position:=0;
  FBlock.Write(LightSettings^, SizeOf(LightSettings^));
  FBlock.Write(LightNames[0], 21*(FHue.LightCount+1));
  FBlock.Write(Lights[0], SizeOf(TLight)*(FHue.LightCount+1));
  FCard.WriteBlock(FBlockID, FBlock);
end;

procedure TLightController.ReadState;
begin
  {$IFDEF DEBUG}WriteLn('Reading state...');{$ENDIF}
  if Assigned(FBlock) then
    FBlock.Free;
  FBlock:=FCard.ReadBlock(FBlockID);
  FBlock.Read(LightSettings^, SizeOf(LightSettings^));
  if LightSettings^.mode = lmAdapt then
  begin
    FHue.Refresh;
    AdaptState;
    FSyncLock:=True;
    WriteState;
  end
  else if LightSettings^.mode = lmEnforce then
  begin
    FBlock.Read(LightNames[0], 21*(FHue.LightCount+1));
    FBlock.Read(Lights[0], SizeOf(TLight)*(FHue.LightCount+1));
    Move(Lights[0], FLights[0], SizeOf(TLight)*(FHue.LightCount+1));
    FHue.Refresh;
    EnforceState;
  end;
end;

procedure TLightController.ProcessSync(card, blkid: integer);
begin
  {$IFDEF DEBUG}WriteLn('ProcessSync...');{$ENDIF}
  if FSyncLock then
  begin
    FSyncLock:=False;
    Exit;
  end;
  ReadState;
end;

procedure TLightController.FadeLights;
var
  i: Integer;
begin
  FFading:=False;
  for i:=1 to FHue.LightCount do
    if Lights[i].lit <> FLights[i].lit then
    begin
      if FLights[i].bri < 10 then
      begin
        FHue.SetState(i, False, FLights[i].bri, FLights[i].hue, FLights[i].sat);
        FLights[i].lit:=False;
      end
      else
      begin
        Dec(FLights[i].bri, 10);
        FHue.SetState(i, True, FLights[i].bri, FLights[i].hue, FLights[i].sat);
        FFading:=True;
      end;
    end;
end;

procedure TLightController.AppLoop;
var
  c: integer;
  done: Boolean;
begin
  c:=0;
  repeat
    Sleep(1000);
    FCard.CheckEvents(Self, done);
    FadeLights;
    if not FFading then
      Inc(c)
    else
      c:=1;
    if c mod 60 = 0 then
    begin
      {$IFDEF DEBUG}WriteLn(' Checking State...');{$ENDIF}
      { TODO : Debating on adding sunrise/sunset checking here }
      FHue.Refresh;
      case LightSettings^.mode of
        lmAdapt: AdaptAndSync;
        lmEnforce: EnforceState;
      end;
    end;
  until not LightSettings^.running;
end;

procedure TLightController.InitBlock(info: PBlockInfo);
begin
  FBlockID:=FCard.FindFree;
  FBlock:=FCard.ReadBlock(FBlockID);
  info^.title:='Lights Data';
  info^.appno:=$79;
  info^.typno:=LIGHT_TYPNO;
  info^.nextid:=0;
  info^.total:=11;
  LightSettings^.count:=FHue.LightCount;
  LightSettings^.mode:=lmEnforce;
  LightSettings^.running:=True;
  FBlock.Write(LightSettings^, SizeOf(LightSettings^));
  AdaptState;
  FBlock.Write(LightNames[0], 21*(FHue.LightCount+1));
  FBlock.Write(Lights[0], SizeOf(TLight)*(FHue.LightCount+1));
  FCard.WriteBlock(FBlockID, FBlock, info);
end;

procedure TLightController.DoRun;
var
  ErrorMsg: String;
  info: PBlockInfo;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  FCard:=TNetCard.Create('cherry.home.lan', 3845);
  FCard.Authenticate('HomeCU');
  FCard.SelectCard(2);

  FHue:=THueBridge.Create(Self);
  SetLength(Lights, FHue.LightCount+1);
  SetLength(LightNames, FHue.LightCount+1);
  SetLength(FLights, FHue.LightCount+1);

  New(LightSettings);

  New(info);
  FBlockID:=FCard.FindType(LIGHT_TYPNO, info);
  if FBlockID = 0 then
    InitBlock(info)
  else
    ReadState;
  Dispose(info);

  if not LightSettings^.running then
  begin
    LightSettings^.running:=True;
    WriteState;
  end;

  FCard.OnSync:=@ProcessSync;
  FCard.Subscribe(FBlockID);
  FSyncLock:=False;

  AppLoop;
  WriteLn('AppLoop Ended.');
  FBlock.Free;
  FCard.Free;
  SetLength(Lights, 0);
  SetLength(LightNames, 0);
  Dispose(LightSettings);

  // stop program loop
  Terminate;
end;

constructor TLightController.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TLightController.Destroy;
begin
  inherited Destroy;
end;

procedure TLightController.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TLightController;
begin
  Application:=TLightController.Create(nil);
  Application.Title:='Light Controller';
  Application.Run;
  Application.Free;
end.

