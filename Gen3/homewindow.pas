unit HomeWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, EditBtn, Grids, cutypes, cuclient, DateUtils, netcard,
  DayWindow, TypInfo, ssockets;

type

  { THomeForm }

  THomeForm = class(TForm)
    ImageList: TImageList;
    Label2: TLabel;
    Label3: TLabel;
    Conditions: TLabel;
    Label4: TLabel;
    FutureTitle: TLabel;
    Label5: TLabel;
    FutureTemp: TLabel;
    Label6: TLabel;
    FutureOutlook: TLabel;
    ScheduleEditor: TTabSheet;
    ScheduleGrid: TStringGrid;
    Temperature: TLabel;
    LightSat: TLabeledEdit;
    LightHue: TLabeledEdit;
    LightLit: TCheckBox;
    GroupBox2: TGroupBox;
    LgtVmRSS: TLabeledEdit;
    LightsRunning: TCheckBox;
    LightMode: TComboBox;
    GroupBox1: TGroupBox;
    IsRunning: TCheckBox;
    HomeFlags: TCheckGroup;
    Label1: TLabel;
    LightCount: TLabeledEdit;
    LightList: TListBox;
    LightBright: TTrackBar;
    Timer: TTimer;
    ToolBar: TToolBar;
    NewBtn: TToolButton;
    OpenBtn: TToolButton;
    SaveBtn: TToolButton;
    CurrentBtn: TToolButton;
    ToolButton1: TToolButton;
    AddRowBtn: TToolButton;
    VmRSS: TLabeledEdit;
    Sunset: TLabeledEdit;
    Sunrise: TLabeledEdit;
    SayMsg: TLabeledEdit;
    LogMsg: TLabeledEdit;
    StatusBar: TStatusBar;
    TabBar: TPageControl;
    HomeCU: TTabSheet;
    Lights: TTabSheet;
    Forecast: TTabSheet;
    procedure AddRowBtnClick(Sender: TObject);
    procedure CurrentBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure LightListClick(Sender: TObject);
    procedure LogMsgKeyPress(Sender: TObject; var Key: char);
    procedure NewBtnClick(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure SayMsgKeyPress(Sender: TObject; var Key: char);
    procedure TimerTimer(Sender: TObject);
  private
    FClient: THomeCU;
    FLight: Byte;
    FScheduleDay: TDaySelected;
    FStatusCount: integer;
    procedure SetStatus(const value: string);
    procedure DoSync(Sender: TObject);
    procedure ClearSchedule;
    procedure LoadSchedule(const typno: byte);
    procedure SaveSchedule(const typno: byte);
  public

  end;

var
  HomeForm: THomeForm;

implementation

{$R *.lfm}

{ THomeForm }

procedure THomeForm.FormCreate(Sender: TObject);
begin
  FScheduleDay:=dsNone;
  FClient:=THomeCU.Create(Self);
  FClient.LoadAll:=True;
  FClient.OnSync:=@DoSync;
  FClient.OnLog:=@SetStatus;
  FClient.OnSay:=@SetStatus;
  SetStatus('Connecting to Memory Card Server...');
  try
    FClient.Connect;
    CurrentBtnClick(Self);
    FStatusCount:=0;
    Timer.Enabled:=True;
  except
    On ESocketError do SetStatus('Unable to connect to Memory Card Server.');
  end;
end;

procedure THomeForm.CurrentBtnClick(Sender: TObject);
begin
  LoadSchedule(SCHED_TYPNO);
  FScheduleDay:=dsToday;
end;

procedure THomeForm.AddRowBtnClick(Sender: TObject);
begin
  ScheduleGrid.InsertRowWithValues(ScheduleGrid.RowCount, ['','','']);
end;

procedure THomeForm.FormResize(Sender: TObject);
begin
  TabBar.Width:=ClientWidth;
  TabBar.Height:=ClientHeight-StatusBar.Height;
  LightList.Height:=Lights.ClientHeight-Lights.Top;
  ScheduleEditor.Width:=TabBar.ClientWidth;
  ScheduleEditor.Height:=TabBar.ClientHeight;
  ScheduleGrid.Width:=ScheduleEditor.ClientWidth;
  ScheduleGrid.Height:=ScheduleEditor.ClientHeight-ToolBar.Height;
end;

procedure THomeForm.LightListClick(Sender: TObject);
begin
  FLight:=LightList.ItemIndex+1;
  with cutypes.Lights[FLight] do
  begin
    LightLit.Checked:=lit;
    LightBright.Position:=bri;
    LightHue.Text:=IntToStr(hue);
    LightSat.Text:=IntToStr(sat);
  end;
end;

procedure THomeForm.LogMsgKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    FClient.Log(LogMsg.Text);
end;

procedure THomeForm.NewBtnClick(Sender: TObject);
begin
  ClearSchedule;
  FScheduleDay:=dsNone;
end;

procedure THomeForm.OpenBtnClick(Sender: TObject);
begin
  if not DayForm.GetDay then
    Exit;
  FScheduleDay:=DayForm.DaySelected;
  WriteLn(Ord(DayForm.DaySelected));
  WriteLn(GetEnumName(TypeInfo(TDaySelected), Ord(DayForm.DaySelected)));
  LoadSchedule(SCHED_BASE+Ord(FScheduleDay));
end;

procedure THomeForm.SaveBtnClick(Sender: TObject);
begin
  if FScheduleDay = dsNone then
  begin
    if not DayForm.GetDay then
      Exit;
    FScheduleDay:=DayForm.DaySelected;
  end;
  if FScheduleDay = dsToday then
    SaveSchedule(SCHED_TYPNO)
  else
    SaveSchedule(SCHED_BASE+Ord(FScheduleDay));
end;

procedure THomeForm.SayMsgKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    FClient.Say(SayMsg.Text);
end;

procedure THomeForm.TimerTimer(Sender: TObject);
begin
  FClient.CheckEvents;
  Inc(FStatusCount);
  if FStatusCount > 20 then
  begin
    SetStatus('System Ready.');
    FStatusCount:=0;
  end;
end;

procedure THomeForm.SetStatus(const value: string);
begin
  StatusBar.SimpleText:=value;
  Application.ProcessMessages;
end;

procedure THomeForm.DoSync(Sender: TObject);
var
  i: Integer;
begin
  SetStatus('Populating Data...');
  HomeFlags.Checked[0]:=FClient.HomeMode;
  HomeFlags.Checked[1]:=FClient.SleepMode;
  HomeFlags.Checked[2]:=FClient.DarkMode;
  HomeFlags.Checked[3]:=FClient.WakeMe;
  HomeFlags.Checked[4]:=FClient.Paused;
  HomeFlags.Checked[5]:=FClient.Rooms;
  IsRunning.Checked:=CUSettings^.running;
  LogMsg.Text:=CUSettings^.log;
  SayMsg.Text:=CUSettings^.say;
  Sunrise.Text:=TimeToStr(CUSettings^.sunrise);
  Sunset.Text:=TimeToStr(CUSettings^.sunset);
  VmRSS.Text:=CUSettings^.vmrss;
  LightCount.Text:=IntToStr(LightSettings^.count);
  if LightSettings^.mode = lmAdapt then
    LightMode.ItemIndex:=0
  else
    LightMode.ItemIndex:=1;
  LightsRunning.Checked:=LightSettings^.running;
  LgtVmRSS.Text:=LightSettings^.vmrss;
  for i:=1 to High(cutypes.Lights) do
    LightList.Items.Add(LightNames[i]);
  Temperature.Caption:=FClient.Weather^.temperature;
  Conditions.Caption:=FClient.Weather^.conditions;
  FutureTitle.Caption:=FClient.Weather^.forecast[0].title;
  FutureTemp.Caption:=FClient.Weather^.forecast[0].temperature;
  FutureOutlook.Caption:=FClient.Weather^.forecast[0].outlook;
  SetStatus('System Ready.');
end;

procedure THomeForm.ClearSchedule;
var
  i: integer;
begin
  for i:=1 to ScheduleGrid.RowCount-1 do
    ScheduleGrid.DeleteRow(1);
end;

procedure THomeForm.LoadSchedule(const typno: byte);
var
  i, blkid: integer;
  info: TBlockInfo;
  blk: TMemoryStream;
  itm: PCUSchedule;
  s: string;
begin
  ClearSchedule;
  blkid:=FClient.Card.FindType(typno, @info);
  if blkid = 0 then
  begin
    SetStatus('Schedule not found.');
    FScheduleDay:=dsNone;
    Exit;
  end;
  blk:=FClient.Card.ReadBlock(blkid);
  New(itm);
  try
    for i:=0 to info.total-1 do
    begin
      blk.Read(itm^, SizeOf(itm^));
      s:=GetEnumName(TypeInfo(TScheduleAction), Ord(itm^.action));
      ScheduleGrid.InsertRowWithValues(ScheduleGrid.RowCount, [TimeToStr(itm^.dt), s, itm^.param]);
    end;
  finally
    blk.Free;
    Dispose(itm);
  end;
end;

procedure THomeForm.SaveSchedule(const typno: byte);
var
  i, blkid: integer;
  info: TBlockInfo;
  blk: TMemoryStream;
  itm: PCUSchedule;
begin
  blkid:=FClient.Card.FindType(typno, @info);
  if blkid = 0 then
  begin
    info.appno:=$60;
    info.typno:=typno;
    info.nextid:=0;
    info.title:=GetEnumName(TypeInfo(TDaySelected), Ord(FScheduleDay));
    blkid:=FClient.Card.FindFree;
  end;
  blk:=FClient.Card.ReadBlock(blkid);
  New(itm);
  try
    info.total:=ScheduleGrid.RowCount-1;
    for i:=1 to ScheduleGrid.RowCount-1 do
    begin
      itm^.dt:=StrToTime(ScheduleGrid.Cells[0,i]);
      itm^.triggered:=False;
      itm^.action:=TScheduleAction(GetEnumValue(TypeInfo(TScheduleAction), ScheduleGrid.Cells[1,i]));
      itm^.param:=ScheduleGrid.Cells[2,i];
      blk.Write(itm^, SizeOf(itm^));
    end;
    FClient.Card.WriteBlock(blkid, blk, @info);
    SetStatus('Schedule Saved.');
  finally
    blk.Free;
    Dispose(itm);
  end;
end;

end.

