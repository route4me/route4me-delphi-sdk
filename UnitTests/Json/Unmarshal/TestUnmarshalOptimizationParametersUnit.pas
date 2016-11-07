unit TestUnmarshalOptimizationParametersUnit;

interface

uses
  TestFramework, SysUtils,
  TestBaseJsonUnmarshalUnit,
  IOptimizationParametersProviderUnit;

type
  TTestUnmarshalOptimizationParameters = class(TTestBaseJsonUnmarshal)
  private
    procedure CheckEquals(Etalon: IOptimizationParametersProvider; TestName: String);
  published
    procedure SingleDriverRoute10Stops();
  end;

implementation

{ TTestOptimizationParametersToJson }

uses
  Classes, System.JSON,
  SingleDriverRoute10StopsTestDataProviderUnit,
  OptimizationParametersUnit, MarshalUnMarshalUnit;

procedure TTestUnmarshalOptimizationParameters.CheckEquals(
  Etalon: IOptimizationParametersProvider; TestName: String);
var
  ActualList: TStringList;
  Actual: TOptimizationParameters;
  JsonFilename: String;
  OptimizationParameters: TOptimizationParameters;
  JsonValue: TJSONValue;
begin
  JsonFilename := EtalonFilename(TestName);
  ActualList := TStringList.Create;
  try
    ActualList.LoadFromFile(JsonFilename);

    OptimizationParameters := Etalon.OptimizationParametersForResponse;
    try
      JsonValue := TJSONObject.ParseJSONValue(ActualList.Text);
      try
          Actual := TMarshalUnMarshal.FromJson(
            OptimizationParameters.ClassType, JsonValue) as TOptimizationParameters;
      finally
        FreeAndNil(JsonValue);
      end;

      CheckTrue(OptimizationParameters.Equals(Actual));
    finally
      FreeAndNil(OptimizationParameters);
    end;
  finally
    FreeAndNil(ActualList);
  end;
  Etalon := nil;
end;

procedure TTestUnmarshalOptimizationParameters.SingleDriverRoute10Stops;
begin
  CheckEquals(
    TSingleDriverRoute10StopsTestDataProvider.Create,
    'OptimizationParametersFromJson\SingleDriverRoute10Stops');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('JSON\Unmarshal\', TTestUnmarshalOptimizationParameters.Suite);
end.
