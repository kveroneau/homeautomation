program HomeManager;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms, HomeWindow, DayWindow
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Home Manager';
  Application.Scaled:=True;
  Application.ExceptionDialog:=aedOkMessageBox;
  Application.Initialize;
  Application.CreateForm(THomeForm, HomeForm);
  Application.CreateForm(TDayForm, DayForm);
  Application.Run;
end.

