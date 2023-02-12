program Scheduler;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, cmdparse, logger, signals, cuclient, cutypes;

type

  { TScheduler }

  TScheduler = class(TCustomApplication)
  private
    FRunning: Boolean;
    FClient: THomeCU;
    FSchedule: Array of TCUSchedule;
    procedure HandleSignal;
    procedure HandleSync(Sender: TObject);
    procedure GenerateSchedule;
    procedure AppLoop;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TScheduler }

procedure TScheduler.HandleSignal;
begin
  LogWarn('Stop Signal requested.');
  FRunning:=False;
end;

procedure TScheduler.HandleSync(Sender: TObject);
begin
  LogInfo('Sync Event Received!');
end;

procedure TScheduler.GenerateSchedule;
begin

end;

procedure TScheduler.AppLoop;
begin
  FRunning:=True;
  LogInfo('AppLoop started.');
  repeat
    FClient.CheckEvents;
    Sleep(500);
  until not FRunning;
  LogInfo('AppLoop ended.');
end;

procedure TScheduler.DoRun;
begin
  InitConfig(Self);
  SetupLog('Scheduler.log');
  LogInfo('Scheduler System starting...');
  OnSignal:=@HandleSignal;

  FClient:=THomeCU.Create(Self);
  FClient.OnSync:=@HandleSync;
  {FClient.LoadAll:=True;}
  FClient.Connect;

  AppLoop;

  SetLength(FSchedule, 0);

  // stop program loop
  Terminate;
end;

constructor TScheduler.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TScheduler.Destroy;
begin
  inherited Destroy;
end;

procedure TScheduler.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TScheduler;
begin
  Application:=TScheduler.Create(nil);
  Application.Title:='Scheduler';
  Application.Run;
  Application.Free;
end.

