unit TestUnmarshalOptimizationParametersUnit;

interface

uses
  TestFramework,
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
    procedure SingleDriverRoute10Stops();
  published

    procedure SingleDriverRoundTrip();
  end;

implementation

{ TTestOptimizationParametersToJson }

uses
  Classes,
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
begin
  JsonFilename := EtalonFilename(TestName);
  ActualList := TStringList.Create;
  try
    ActualList.LoadFromFile(JsonFilename);

    OptimizationParameters := Etalon.OptimizationParameters;
    Actual := TMarshalUnMarshal.FromJson(OptimizationParameters.ClassType, ActualList.Text) as TOptimizationParameters;
    CheckTrue(OptimizationParameters.Equals(Actual));
  finally
    ActualList.Free;
  end;
  Etalon := nil;
end;

procedure TTestUnmarshalOptimizationParameters.MultipleDepotMultipleDriver();
begin
  CheckEquals(
    TMultipleDepotMultipleDriverTestDataProvider.Create,
    'OptimizationParametersToJson\MultipleDepotMultipleDriver');
end;

procedure TTestUnmarshalOptimizationParameters.MultipleDepotMultipleDriverTimeWindow;
begin
  CheckEquals(
    TMultipleDepotMultipleDriverTimeWindowTestDataProvider.Create,
    'OptimizationParametersToJson\MultipleDepotMultipleDriverTimeWindow');
end;

procedure TTestUnmarshalOptimizationParameters.MultipleDepotMultipleDriverWith24StopsTimeWindow;
begin
  CheckEquals(
    TMultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProvider.Create,
    'OptimizationParametersToJson\MultipleDepotMultipleDriverWith24StopsTimeWindow');
end;

procedure TTestUnmarshalOptimizationParameters.SingleDepotMultipleDriverNoTimeWindow;
begin
  CheckEquals(
    TSingleDepotMultipleDriverNoTimeWindowTestDataProvider.Create,
    'OptimizationParametersToJson\SingleDepotMultipleDriverNoTimeWindow');
end;

procedure TTestUnmarshalOptimizationParameters.SingleDriverMultipleTimeWindows;
begin
  CheckEquals(
    TSingleDriverMultipleTimeWindowsTestDataProvider.Create,
    'OptimizationParametersToJson\SingleDriverMultipleTimeWindows');
end;

procedure TTestUnmarshalOptimizationParameters.SingleDriverRoundTrip;
begin
  CheckEquals(
    TSingleDriverRoundTripTestDataProvider.Create,
    'OptimizationParametersToJson\SingleDriverRoundTrip');
end;

procedure TTestUnmarshalOptimizationParameters.SingleDriverRoute10Stops;
begin
  CheckEquals(
    TSingleDriverRoute10StopsTestDataProvider.Create,
    'OptimizationParametersToJson\SingleDriverRoute10Stops');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('JSON\Unmarshal\', TTestUnmarshalOptimizationParameters.Suite);
end.
