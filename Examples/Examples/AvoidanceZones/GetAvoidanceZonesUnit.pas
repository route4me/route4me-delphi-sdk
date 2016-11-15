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
  AvoidanceZones: TAvoidanceZoneArray;
  i: integer;
begin
  AvoidanceZones := Route4MeManager.AvoidanceZone.GetList(ErrorString);
  try
    WriteLn('');

    if (Length(AvoidanceZones) > 0) then
      WriteLn(Format('GetAvoidanceZones executed successfully, %d zones returned',
        [Length(AvoidanceZones)]))
    else
      WriteLn(Format('GetAvoidanceZones error: "%s"', [ErrorString]));
  finally
    for i := Length(AvoidanceZones) - 1 downto 0 do
      FreeAndNil(AvoidanceZones[i]);
  end;
end;

end.
