unit DayWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  TDaySelected = (dsNone, dsSunday, dsMonday, dsTuesday, dsWednesday,
                  dsThursday, dsFriday, dsSaturday, dsToday);

  { TDayForm }

  TDayForm = class(TForm)
    SunBtn: TButton;
    MonBtn: TButton;
    TueBtn: TButton;
    WedBtn: TButton;
    ThurDay: TButton;
    FriBtn: TButton;
    SatBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FriBtnClick(Sender: TObject);
    procedure MonBtnClick(Sender: TObject);
    procedure SatBtnClick(Sender: TObject);
    procedure SunBtnClick(Sender: TObject);
    procedure ThurDayClick(Sender: TObject);
    procedure TueBtnClick(Sender: TObject);
    procedure WedBtnClick(Sender: TObject);
  private
    FDaySelected: TDaySelected;
  public
    function GetDay: Boolean;
    property DaySelected: TDaySelected read FDaySelected;
  end;

var
  DayForm: TDayForm;

implementation

{$R *.lfm}

{ TDayForm }

procedure TDayForm.SunBtnClick(Sender: TObject);
begin
  FDaySelected:=dsSunday;
end;

procedure TDayForm.ThurDayClick(Sender: TObject);
begin
  FDaySelected:=dsThursday;
end;

procedure TDayForm.TueBtnClick(Sender: TObject);
begin
  FDaySelected:=dsTuesday;
end;

procedure TDayForm.WedBtnClick(Sender: TObject);
begin
  FDaySelected:=dsWednesday;
end;

procedure TDayForm.MonBtnClick(Sender: TObject);
begin
  FDaySelected:=dsMonday;
end;

procedure TDayForm.SatBtnClick(Sender: TObject);
begin
  FDaySelected:=dsSaturday;
end;

procedure TDayForm.FriBtnClick(Sender: TObject);
begin
  FDaySelected:=dsFriday;
end;

procedure TDayForm.FormCreate(Sender: TObject);
begin

end;

function TDayForm.GetDay: Boolean;
var
  r: TModalResult;
begin
  Result:=False;
  FDaySelected:=dsNone;
  r:=ShowModal;
  if r <> mrOK then
    FDaySelected:=dsNone
  else
    Result:=True;
end;

end.

