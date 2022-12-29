program HTTPEndpoints;

{$mode objfpc}{$H+}

uses
  fphttpapp, endpoints, netcard;

begin
  Application.Title:='Home Automation Endpoints';
  Application.Port:=8080;
  Application.LegacyRouting:=True;
  Application.DefaultModuleName:='cu';
  Application.Initialize;
  Application.Run;
end.

