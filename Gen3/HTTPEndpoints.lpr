program HTTPEndpoints;

{$mode objfpc}{$H+}

uses
  sysutils, fphttpapp, endpoints, netcard, cmdparse, logger;

begin
  SetupLog('http.log');
  LogInfo('HTTPEndpoints starting...');
  Application.Title:='Home Automation Endpoints';
  Application.Port:=8080;
  Application.LegacyRouting:=True;
  Application.DefaultModuleName:='cu';
  Application.Initialize;
  try
    InitConfig(Application);
    Application.Run;
  except
    On Exception do Application.Terminate;
  end;
  LogInfo('HTTPEndpoints has stopped.');
end.

