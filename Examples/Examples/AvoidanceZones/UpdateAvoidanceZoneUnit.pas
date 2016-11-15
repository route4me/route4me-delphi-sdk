unit UpdateAvoidanceZoneUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TUpdateAvoidanceZone = class(TBaseExample)
  public
    procedure Execute(TerritoryId: String);
  end;

implementation

uses AvoidanceZoneUnit, TerritoryUnit, AvoidanceZoneParametersUnit, EnumsUnit;

procedure TUpdateAvoidanceZone.Execute(TerritoryId: String);
var
  ErrorString: String;
  Parameters: TAvoidanceZoneParameters;
  Territory: TTerritory;
  AvoidanceZone: TAvoidanceZone;
begin
  Parameters := TAvoidanceZoneParameters.Create();
  try
    Parameters.TerritoryId := TerritoryId;
    Parameters.TerritoryName := 'Test Territory Updated';
    Parameters.TerritoryColor := 'ff00ff';
    Territory := TTerritory.Create();
    Territory.TerritoryType := TTerritoryType.ttCircle;
    Territory.AddDataItem('38.41322259056806,-78.501953234');
    Territory.AddDataItem('3000');
    Parameters.Territory := Territory;

    AvoidanceZone := Route4MeManager.AvoidanceZone.Update(Parameters, ErrorString);
    try
      WriteLn('');

      if (AvoidanceZone <> nil) then
      begin
        WriteLn('UpdateAvoidanceZone executed successfully');
        WriteLn(Format('Territory ID: %s', [AvoidanceZone.TerritoryId.Value]));
      end
      else
        WriteLn(Format('UpdateAvoidanceZone error: "%s"', [ErrorString]));
    finally
      FreeAndNil(AvoidanceZone);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
