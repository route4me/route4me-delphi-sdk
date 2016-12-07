unit GetAvoidanceZonesUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetAvoidanceZones = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

uses AvoidanceZoneUnit;

procedure TGetAvoidanceZones.Execute;
var
  ErrorString: String;
  AvoidanceZones: TAvoidanceZoneList;
begin
  AvoidanceZones := Route4MeManager.AvoidanceZone.GetList(ErrorString);
  try
    WriteLn('');

    if (AvoidanceZones.Count > 0) then
      WriteLn(Format('GetAvoidanceZones executed successfully, %d zones returned',
        [AvoidanceZones.Count]))
    else
      WriteLn(Format('GetAvoidanceZones error: "%s"', [ErrorString]));
  finally
    FreeAndNil(AvoidanceZones);
  end;
end;

end.
