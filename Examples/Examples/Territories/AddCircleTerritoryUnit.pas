unit AddCircleTerritoryUnit;

interface

uses SysUtils, BaseExampleUnit, NullableBasicTypesUnit;

type
  TAddCircleTerritory = class(TBaseExample)
  public
    function Execute: NullableString;
  end;

implementation

uses TerritoryContourUnit;

function TAddCircleTerritory.Execute: NullableString;
var
  ErrorString: String;
  TerritoryName, TerritoryColor: String;
  TerritoryContour: TCircleTerritory;
  TerritoryId: NullableString;
begin
  TerritoryName := 'Circle Territory';
  TerritoryColor := 'ff0000';
  TerritoryContour := TCircleTerritory.Create(37.5697528227865, -77.4783325195313, 5000);

  TerritoryId := Route4MeManager.Territory.Add(
    TerritoryName, TerritoryColor, TerritoryContour, ErrorString);

  WriteLn('');

  if (TerritoryId.IsNotNull) then
  begin
    WriteLn('AddCircleTerritory executed successfully');
    WriteLn(Format('Territory ID: %s', [TerritoryId.Value]));
  end
  else
    WriteLn(Format('AddCircleTerritory error: "%s"', [ErrorString]));

  Result := TerritoryId;
end;

end.
