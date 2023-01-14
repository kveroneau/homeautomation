unit signals;

{$mode objfpc}{$H+}

interface

uses
  BaseUnix;

type
  TSignalEvent = procedure of object;
  TSignalProc = procedure;

var
  OnSignal: TSignalEvent;
  OnSignalProc: TSignalProc;

implementation

var
  oa, na: psigactionrec;

procedure HandleSignal(sig: cint); cdecl;
begin
  if (sig = SIGTERM) or (sig = SIGINT) then
  begin
    if Assigned(OnSignal) then
      OnSignal
    else if Assigned(OnSignalProc) then
      OnSignalProc
    else
      Halt(2);
  end;
end;

procedure HookSignals;
begin
  New(oa);
  New(na);
  na^.sa_handler:=sigactionhandler(@HandleSignal);
  FillChar(na^.sa_mask, SizeOf(na^.sa_mask), #0);
  na^.sa_flags:=0;
  {$IFDEF Linux}
  na^.sa_restorer:=Nil;
  {$ENDIF}
  if FPSigaction(SIGINT, na, oa) <> 0 then
  begin
    WriteLn('Unable to trap SIGINT Signal!');
    Halt(1);
  end;
  if FPSigaction(SIGTERM, na, oa) <> 0 then
  begin
    WriteLn('Unable to trap SIGTERM Signal!');
    Halt(1);
  end;
end;

initialization
  OnSignal:=Nil;
  OnSignalProc:=Nil;
  HookSignals;

finalization
  Dispose(na);
  Dispose(oa);

end.

