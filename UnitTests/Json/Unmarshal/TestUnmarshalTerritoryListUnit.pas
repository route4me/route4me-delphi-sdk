unit TestUnmarshalTerritoryListUnit;

interface

uses
  TestFramework, SysUtils,
  TestBaseJsonUnmarshalUnit,
  System.Generics.Collections;

type
  TTestUnmarshalTerritoryList = class(TTestBaseJsonUnmarshal)
  published
    procedure TerritoryListFindResponse;
  end;

implementation

{ TTestOptimizationParametersToJson }

uses
  Classes, System.JSON, TerritoryUnit, MarshalUnMarshalUnit,
  GetTerritoriesResponseUnit;

procedure TTestUnmarshalTerritoryList.TerritoryListFindResponse;
const
  Etalon =
    '[{"territory_id":"1DC2DCE2F08035B7CC017874451B137C","territory_name":"Circle Territory","territory_color":"ff0000",' +
      '"member_id":1,"addresses":[],"territory":{"type":"circle","data":["37.5697528227865,-77.4783325195313","5000"]}},' +
      '{"territory_id":"1DF50C0BAD3C66DE13D129698CD63F09","territory_name":"Polygon Territory","territory_color":"ff0000",' +
      '"member_id":1,"addresses":[],"territory":{"type":"poly","data":["37.74763966054,-77.69172210693"]}}]';
var
  Actual: TGetTerritoriesResponse;
  JsonValue: TJSONValue;
begin
  JsonValue := TJSONObject.ParseJSONValue(Etalon);
  try
    Actual := TMarshalUnMarshal.FromJson(TGetTerritoriesResponse, JsonValue) as TGetTerritoriesResponse;

    CheckEquals(2, Length(Actual.Territories));
    CheckEquals('Circle Territory', Actual.Territories[0].Name);
    CheckEquals('Polygon Territory', Actual.Territories[1].Name);
  finally
    FreeAndNil(JsonValue);
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('JSON\Unmarshal\', TTestUnmarshalTerritoryList.Suite);
end.
