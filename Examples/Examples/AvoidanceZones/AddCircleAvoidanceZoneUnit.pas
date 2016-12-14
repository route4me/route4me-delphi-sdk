unit AddCircleAvoidanceZoneUnit;

interface

uses SysUtils, BaseExampleUnit, NullableBasicTypesUnit;

type
  TAddCircleAvoidanceZone = class(TBaseExample)
  public
    function Execute: NullableString;
  end;

implementation

uses TerritoryContourUnit, AvoidanceZoneUnit;

function TAddCircleAvoidanceZone.Execute: NullableString;
var
  ErrorString: String;
  AvoidanceZone: TAvoidanceZone;
  Territory: TTerritoryContour;
  NewAvoidanceZone: TAvoidanceZone;
  TerritoryName, TerritoryColor: String;
begin
  Result := NullableString.Null;

  TerritoryName := 'Circle Territory';
  TerritoryColor := 'ff0000';
  Territory := TTerritoryContour.MakeCircleContour(
    37.5697528227865, -77.4783325195313, 5000);
  AvoidanceZone := TAvoidanceZone.Create(TerritoryName, TerritoryColor, Territory);
  try
    NewAvoidanceZone := Route4MeManager.AvoidanceZone.Add(AvoidanceZone, ErrorString);
    try
      WriteLn('');

      if (NewAvoidanceZone <> nil) then
      begin
        WriteLn('AddCircleAvoidanceZone executed successfully');
        WriteLn(Format('Territory ID: %s', [NewAvoidanceZone.TerritoryId.Value]));

        Result := NewAvoidanceZone.TerritoryId;
      end
      else
        WriteLn(Format('AddCircleAvoidanceZone error: "%s"', [ErrorString]));
    finally
      FreeAndNil(NewAvoidanceZone);
    end;
  finally
    FreeAndNil(AvoidanceZone);
  end;
end;

end.
