program LightApp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, huecontrol
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Light Control App';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TLightControlForm, LightControlForm);
  Application.Run;
end.

