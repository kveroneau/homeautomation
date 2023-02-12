unit cutypes;

{$mode objfpc}{$H+}

interface

uses DateUtils;

type
  TLightMode = (lmAdapt, lmEnforce);

  PLightSettings = ^TLightSettings;
  TLightSettings = record
    count: Byte;
    mode: TLightMode;
    running: Boolean;
    vmrss: string[10];
  end;

  PLight = ^TLight;
  TLight = record
    lit: Boolean;
    bri: Byte;
    hue, sat: Integer;
  end;

  TAutomationFlags = (afHome, afSleeping, afDark, afWakeMe, afPaused, afRooms, afRes1, afRes2);
  TAutomationSettings = bitpacked set of TAutomationFlags;

  PCUSettings = ^TCUSettings;
  TCUSettings = packed record
    flags: TAutomationSettings;
    running: Boolean;
    log, say: string[100];
    sunrise, sunset: TDateTime;
    vmrss: string[10];
  end;

  {$PACKENUM 1}
  TScheduleAction = (saNotify, saEndpoint, saSpecial);

  PCUSchedule = ^TCUSchedule;
  TCUSchedule = packed record
    dt: TDateTime;
    triggered: Boolean;
    action: TScheduleAction;
    param: string[60];
  end;

var
  Lights: Array of TLight;
  LightSettings: PLightSettings;
  LightNames: Array of String[20];
  CUSettings: PCUSettings;

const
  LIGHT_TYPNO = $89;
  CU_TYPNO = $88;
  WEATHER_TYPNO = $87;
  SCHED_TYPNO = $86;
  SCHED_BASE = $30;

implementation

initialization
  LightSettings:=Nil;
  CUSettings:=Nil;
end.

