unit AddRectangularAvoidanceZoneUnit;

interface

uses SysUtils, BaseExampleUnit, NullableBasicTypesUnit;

type
  TAddRectangularAvoidanceZone = class(TBaseExample)
  public
    function Execute: NullableString;
  end;

implementation

uses TerritoryContourUnit, AvoidanceZoneUnit;

function TAddRectangularAvoidanceZone.Execute: NullableString;
var
  ErrorString: String;
  AvoidanceZone: TAvoidanceZone;
  Territory: TRectangularTerritory;
  NewAvoidanceZone: TAvoidanceZone;
  TerritoryName, TerritoryColor: String;
begin
  Result := NullableString.Null;

  TerritoryName := 'Rect Territory';
  TerritoryColor := 'ff0000';
  Territory := TRectangularTerritory.Create(
    43.5166885350291, -109.3798828125, 46.9802523552188, -101.865234375);
  AvoidanceZone := TAvoidanceZone.Create(TerritoryName, TerritoryColor, Territory);
  try
    NewAvoidanceZone := Route4MeManager.AvoidanceZone.Add(AvoidanceZone, ErrorString);
    try
      WriteLn('');

      if (NewAvoidanceZone <> nil) then
      begin
        WriteLn('AddRectangularAvoidanceZone executed successfully');
        WriteLn(Format('Territory ID: %s', [NewAvoidanceZone.TerritoryId.Value]));

        Result := NewAvoidanceZone.TerritoryId;
      end
      else
        WriteLn(Format('AddRectangularAvoidanceZone error: "%s"', [ErrorString]));
    finally
      FreeAndNil(NewAvoidanceZone);
    end;
  finally
    FreeAndNil(AvoidanceZone);
  end;
end;

end.
