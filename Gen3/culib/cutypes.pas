unit cutypes;

{$mode objfpc}{$H+}

interface

type
  TLightMode = (lmAdapt, lmEnforce);

  PLightSettings = ^TLightSettings;
  TLightSettings = record
    count: Byte;
    mode: TLightMode;
    running: Boolean;
  end;

  PLight = ^TLight;
  TLight = record
    lit: Boolean;
    bri: Byte;
    hue, sat: Integer;
  end;

var
  Lights: Array of TLight;
  LightSettings: PLightSettings;
  LightNames: Array of String[20];

const
  LIGHT_TYPNO = $89;

implementation

initialization
  LightSettings:=Nil;
end.

