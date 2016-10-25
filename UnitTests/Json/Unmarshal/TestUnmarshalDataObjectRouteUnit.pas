unit TestUnmarshalDataObjectRouteUnit;

interface

uses
  TestFramework, SysUtils,
  TestBaseJsonUnmarshalUnit, IDataObjectRouteProviderUnit;

type
  TTestUnmarshalDataObjectRoute = class(TTestBaseJsonUnmarshal)
  private
    procedure CheckEquals(Etalon: IDataObjectRouteProvider; TestName: String);
  published
    procedure SampleDataObjectRoute();
  end;

implementation

{ TTestOptimizationParametersToJson }

uses
  Classes, System.JSON,
  SampleDataObjectRouteProviderUnit,
  DataObjectUnit, MarshalUnMarshalUnit;

procedure TTestUnmarshalDataObjectRoute.CheckEquals(
  Etalon: IDataObjectRouteProvider; TestName: String);
var
  ActualList: TStringList;
  Actual: TDataObjectRoute;
  JsonFilename: String;
  DataObjectRoute: TDataObjectRoute;
  JsonValue: TJSONValue;
begin
  JsonFilename := EtalonFilename(TestName);
  ActualList := TStringList.Create;
  try
    ActualList.LoadFromFile(JsonFilename);

    JsonValue := TJSONObject.ParseJSONValue(ActualList.Text);
    try
      DataObjectRoute := Etalon.DataObjectRoute;
      Actual := TMarshalUnMarshal.FromJson(DataObjectRoute.ClassType, JsonValue) as TDataObjectRoute;
    finally
      FreeAndNil(JsonValue);
    end;

    CheckTrue(DataObjectRoute.Equals(Actual));
  finally
    FreeAndNil(ActualList);
  end;
  Etalon := nil;
end;

procedure TTestUnmarshalDataObjectRoute.SampleDataObjectRoute;
begin
  CheckEquals(
    TSampleDataObjectRouteTestDataProvider.Create,
    'DataObjectRouteToJson\SampleDataObjectRoute');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('JSON\Unmarshal\', TTestUnmarshalDataObjectRoute.Suite);
end.
