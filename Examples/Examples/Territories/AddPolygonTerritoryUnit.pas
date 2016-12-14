unit AddPolygonTerritoryUnit;

interface

uses SysUtils, BaseExampleUnit, NullableBasicTypesUnit;

type
  TAddPolygonTerritory = class(TBaseExample)
  public
    function Execute: NullableString;
  end;

implementation

uses TerritoryContourUnit, TerritoryUnit, PositionUnit;

function TAddPolygonTerritory.Execute: NullableString;
var
  ErrorString: String;
  TerritoryContour: TTerritoryContour;
  TerritoryName, TerritoryColor: String;
  TerritoryId: NullableString;
begin
  Result := NullableString.Null;

  TerritoryName := 'Polygon Territory';
  TerritoryColor := 'ff0000';
  TerritoryContour := TTerritoryContour.MakePolygonContour([
    TPosition.Create(37.7697528227865, -77.6783325195313),
    TPosition.Create(37.7588671630534, -77.6897480010986),
    TPosition.Create(37.7476396605445, -77.6917221069336),
    TPosition.Create(37.7465508430681, -77.6886322021484),
    TPosition.Create(37.7502255383101, -77.6812507629394),
    TPosition.Create(37.7479799127443, -77.6749851226806),
    TPosition.Create(37.7332796020606, -77.6411678314209),
    TPosition.Create(37.7443051067953, -77.6317264556884),
    TPosition.Create(37.7664192584704, -77.6684619903564)
  ]);

  TerritoryId := Route4MeManager.Territory.Add(
    TerritoryName, TerritoryColor, TerritoryContour, ErrorString);

  WriteLn('');

  if (TerritoryId.IsNotNull) then
  begin
    WriteLn('AddPolygonTerritory executed successfully');
    WriteLn(Format('Territory ID: %s', [Result.Value]));
  end
  else
    WriteLn(Format('AddPolygonTerritory error: "%s"', [ErrorString]));

  Result := TerritoryId;
end;

end.
