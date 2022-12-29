unit huecontrol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, memcard,
  cutypes, netcard;

type

  { TLightControlForm }

  TLightControlForm = class(TForm)
    Brightness: TEdit;
    ApplyBtn: TButton;
    LightMode: TComboBox;
    StopBtn: TButton;
    Hue: TEdit;
    Saturation: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LightOn: TCheckBox;
    LightList: TListBox;
    procedure ApplyBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LightListClick(Sender: TObject);
    procedure LightModeChange(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
  private
    FCard: TNetCard;
    FBlock: TMemoryStream;
    FBlockID: integer;
    procedure PushState;
    procedure PullState;
    procedure ProcessSync(card, blkid: integer);
  public

  end;

var
  LightControlForm: TLightControlForm;

implementation

{$R *.lfm}

{ TLightControlForm }

procedure TLightControlForm.FormCreate(Sender: TObject);
var
  info: PBlockInfo;
  i: integer;
begin
  FCard:=TNetCard.Create('cherry.home.lan', 3845);
  FCard.Authenticate('HomeCU');
  FCard.SelectCard(2);
  New(info);
  FBlockID:=FCard.FindType(LIGHT_TYPNO, info);
  Dispose(info);
  if FBlockID = 0 then
  begin
    ShowMessage('No Light Data Found!');
    Exit;
  end;
  New(LightSettings);
  PullState;
  for i:=1 to LightSettings^.count do
    LightList.Items.Add(LightNames[i]);
  FCard.OnSync:=@ProcessSync;
  FCard.Subscribe(FBlockID);
  Application.OnIdle:=@FCard.CheckEvents;
end;

procedure TLightControlForm.ApplyBtnClick(Sender: TObject);
var
  lgt: integer;
begin
  lgt:=LightList.ItemIndex+1;
  Lights[lgt].lit:=LightOn.Checked;
  Lights[lgt].bri:=StrToInt(Brightness.Text);
  Lights[lgt].hue:=StrToInt(Hue.Text);
  Lights[lgt].sat:=StrToInt(Saturation.Text);
  PushState;
end;

procedure TLightControlForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FBlock) then
    FBlock.Free;
  if Assigned(FCard) then
    FCard.Free;
  if Assigned(LightSettings) then
    Dispose(LightSettings);
end;

procedure TLightControlForm.LightListClick(Sender: TObject);
var
  lgt: integer;
begin
  lgt:=LightList.ItemIndex+1;
  LightOn.Checked:=Lights[lgt].lit;
  Brightness.Text:=IntToStr(Lights[lgt].bri);
  Hue.Text:=IntToStr(Lights[lgt].hue);
  Saturation.Text:=IntToStr(Lights[lgt].sat);
end;

procedure TLightControlForm.LightModeChange(Sender: TObject);
begin
  if LightMode.ItemIndex = 0 then
    LightSettings^.mode:=lmAdapt
  else if LightMode.ItemIndex = 1 then
    LightSettings^.mode:=lmEnforce;
  PushState;
end;

procedure TLightControlForm.StopBtnClick(Sender: TObject);
begin
  LightSettings^.running:=False;
  PushState;
end;

procedure TLightControlForm.PushState;
begin
  FBlock.Position:=0;
  FBlock.Write(LightSettings^, SizeOf(LightSettings^));
  FBlock.Write(LightNames[0], 21*(LightSettings^.count+1));
  FBlock.Write(Lights[0], SizeOf(TLight)*(LightSettings^.count+1));
  FCard.WriteBlock(FBlockID, FBlock);
end;

procedure TLightControlForm.PullState;
begin
  if Assigned(FBlock) then
    FBlock.Free;
  FBlock:=FCard.ReadBlock(FBlockID);
  FBlock.Read(LightSettings^, SizeOf(LightSettings^));
  if LightSettings^.mode = lmAdapt then
    LightMode.ItemIndex:=0
  else if LightSettings^.mode = lmEnforce then
    LightMode.ItemIndex:=1;
  SetLength(LightNames, LightSettings^.count+1);
  FBlock.Read(LightNames[0], 21*(LightSettings^.count+1));
  SetLength(Lights, LightSettings^.count+1);
  FBlock.Read(Lights[0], SizeOf(TLight)*(LightSettings^.count+1));
end;

procedure TLightControlForm.ProcessSync(card, blkid: integer);
begin
  WriteLn('Incoming Sync!');
  PullState;
end;

end.

