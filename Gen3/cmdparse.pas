unit cmdparse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustApp;

type
  PCmdParams = ^TCmdParams;
  TCmdParams = record
    server: string;
    port: word;
    key: string;
    card: byte;
  end;

var
  CmdParams: PCmdParams;

procedure InitConfig(App: TCustomApplication);

implementation

procedure InitConfig(App: TCustomApplication);
var
  ErrorMsg: string;
  ParamsOk: Boolean;
begin
  if CmdParams <> Nil then
    Exit;
  with App do
  begin
    // quick check parameters
    ErrorMsg:=CheckOptions('hs:p:k:c:', ['help','server:','port:','key:','card:']);
    if ErrorMsg<>'' then
      raise Exception.Create(ErrorMsg);

    // parse parameters
    if HasOption('h', 'help') then begin
      {WriteHelp;}
      Terminate;
      Exit;
    end;

    ParamsOk:=True;
    if not HasOption('s', 'server') then
      ParamsOk:=False;
    if not HasOption('p', 'port') then
      ParamsOk:=False;
    if not HasOption('k', 'key') then
      ParamsOk:=False;
    if not HasOption('c', 'card') then
      ParamsOk:=False;
    if not ParamsOk then
      raise Exception.Create('Params not met.');

    New(CmdParams);
    with CmdParams^ do
    begin
      server:=GetOptionValue('s', 'server');
      port:=StrToInt(GetOptionValue('p', 'port'));
      key:=GetOptionValue('k', 'key');
      card:=StrToInt(GetOptionValue('c', 'card'));
    end;
  end;
end;

initialization
  CmdParams:=Nil;

finalization
  if CmdParams <> Nil then
    Dispose(CmdParams);

end.

