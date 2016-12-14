unit AddRectangularTerritoryUnit;

interface

uses SysUtils, BaseExampleUnit, NullableBasicTypesUnit;

type
  TAddRectangularTerritory = class(TBaseExample)
  public
    function Execute: NullableString;
  end;

implementation

uses TerritoryContourUnit, TerritoryUnit;

function TAddRectangularTerritory.Execute: NullableString;
var
  ErrorString: String;
  TerritoryContour: TTerritoryContour;
  TerritoryName, TerritoryColor: String;
  TerritoryId: NullableString;
begin
  Result := NullableString.Null;

  TerritoryName := 'Rect Territory';
  TerritoryColor := 'ff0000';
  TerritoryContour := TTerritoryContour.MakeRectangularContour(
    43.5166885350291, -109.3798828125, 46.9802523552188, -101.865234375);

  TerritoryId := Route4MeManager.Territory.Add(
    TerritoryName, TerritoryColor, TerritoryContour, ErrorString);

  WriteLn('');

  if (TerritoryId.IsNotNull) then
  begin
    WriteLn('AddRectangularTerritory executed successfully');
    WriteLn(Format('Territory ID: %s', [Result.Value]));
  end
  else
    WriteLn(Format('AddRectangularTerritory error: "%s"', [ErrorString]));

  Result := TerritoryId;
end;

end.
