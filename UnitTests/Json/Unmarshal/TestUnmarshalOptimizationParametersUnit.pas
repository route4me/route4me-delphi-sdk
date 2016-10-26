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

    procedure MultipleDepotMultipleDriver();
    procedure MultipleDepotMultipleDriverTimeWindow();
    procedure MultipleDepotMultipleDriverWith24StopsTimeWindow();

    procedure SingleDepotMultipleDriverNoTimeWindow();
    procedure SingleDriverMultipleTimeWindows();
    procedure SingleDriverRoundTrip();
  published
    procedure SingleDriverRoute10Stops();
  end;

implementation

{ TTestOptimizationParametersToJson }

uses
  Classes, System.JSON,
  SingleDriverRoute10StopsTestDataProviderUnit,
  SingleDriverRoundTripTestDataProviderUnit,
  SingleDriverMultipleTimeWindowsTestDataProviderUnit,
  SingleDepotMultipleDriverNoTimeWindowTestDataProviderUnit,
  MultipleDepotMultipleDriverTestDataProviderUnit,
  MultipleDepotMultipleDriverTimeWindowTestDataProviderUnit,
  MultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProviderUnit,
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

    OptimizationParameters := Etalon.OptimizationParametersForResponce;
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

procedure TTestUnmarshalOptimizationParameters.MultipleDepotMultipleDriver();
begin
  CheckEquals(
    TMultipleDepotMultipleDriverTestDataProvider.Create,
    'OptimizationParametersFromJson\MultipleDepotMultipleDriver');
end;

procedure TTestUnmarshalOptimizationParameters.MultipleDepotMultipleDriverTimeWindow;
begin
  CheckEquals(
    TMultipleDepotMultipleDriverTimeWindowTestDataProvider.Create,
    'OptimizationParametersFromJson\MultipleDepotMultipleDriverTimeWindow');
end;

procedure TTestUnmarshalOptimizationParameters.MultipleDepotMultipleDriverWith24StopsTimeWindow;
begin
  CheckEquals(
    TMultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProvider.Create,
    'OptimizationParametersFromJson\MultipleDepotMultipleDriverWith24StopsTimeWindow');
end;

procedure TTestUnmarshalOptimizationParameters.SingleDepotMultipleDriverNoTimeWindow;
begin
  CheckEquals(
    TSingleDepotMultipleDriverNoTimeWindowTestDataProvider.Create,
    'OptimizationParametersFromJson\SingleDepotMultipleDriverNoTimeWindow');
end;

procedure TTestUnmarshalOptimizationParameters.SingleDriverMultipleTimeWindows;
begin
  CheckEquals(
    TSingleDriverMultipleTimeWindowsTestDataProvider.Create,
    'OptimizationParametersFromJson\SingleDriverMultipleTimeWindows');
end;

procedure TTestUnmarshalOptimizationParameters.SingleDriverRoundTrip;
begin
  CheckEquals(
    TSingleDriverRoundTripTestDataProvider.Create,
    'OptimizationParametersFromJson\SingleDriverRoundTrip');
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
