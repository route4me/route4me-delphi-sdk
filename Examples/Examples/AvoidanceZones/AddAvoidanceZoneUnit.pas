unit AddAvoidanceZoneUnit;

interface

uses SysUtils, BaseExampleUnit, NullableBasicTypesUnit;

type
  TAddAvoidanceZone = class(TBaseExample)
  public
    function Execute: NullableString;
  end;

implementation

uses TerritoryUnit, AvoidanceZoneUnit, AvoidanceZoneParametersUnit, EnumsUnit;

function TAddAvoidanceZone.Execute: NullableString;
var
  ErrorString: String;
  Parameters: TAvoidanceZoneParameters;
  Territory: TTerritory;
  AvoidanceZone: TAvoidanceZone;
begin
  Result := NullableString.Null;

  Parameters := TAvoidanceZoneParameters.Create();
  try
    Parameters.TerritoryName := 'Test Territory';
    Parameters.TerritoryColor := 'ff0000';
    Territory := TTerritory.Create;
    Territory.TerritoryType := TTerritoryType.ttCircle;
    Territory.AddDataItem('37.569752822786455,-77.47833251953125');
    Territory.AddDataItem('5000');
    Parameters.Territory := Territory;

    AvoidanceZone := Route4MeManager.AvoidanceZone.Add(Parameters, ErrorString);
    try
      WriteLn('');

      if (AvoidanceZone <> nil) then
      begin
        WriteLn('AddAvoidanceZone executed successfully');
        WriteLn(Format('Territory ID: %s', [AvoidanceZone.TerritoryId.Value]));

        Result := AvoidanceZone.TerritoryId;
      end
      else
        WriteLn(Format('AddAvoidanceZone error: "%s"', [ErrorString]));
    finally
      FreeAndNil(AvoidanceZone);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
