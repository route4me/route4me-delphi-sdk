unit TestUnmarshalOptimizationParametersUnit;

interface

uses
  TestFramework,
  TestBaseJsonUnmarshalUnit;

type
  TTestUnmarshalOptimizationParameters = class(TTestBaseJsonUnmarshal)
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
  IOptimizationParametersProviderUnit,
  SingleDriverRoute10StopsTestDataProviderUnit,
  SingleDriverRoundTripTestDataProviderUnit,
  SingleDriverMultipleTimeWindowsTestDataProviderUnit,
  SingleDepotMultipleDriverNoTimeWindowTestDataProviderUnit,
  MultipleDepotMultipleDriverTestDataProviderUnit,
  MultipleDepotMultipleDriverTimeWindowTestDataProviderUnit,
  MultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProviderUnit;

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
    EtalonFilename('OptimizationParametersToJson\MultipleDepotMultipleDriverTimeWindow'));
end;

procedure TTestUnmarshalOptimizationParameters.MultipleDepotMultipleDriverWith24StopsTimeWindow;
begin
  CheckEquals(
    TMultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProvider.Create,
    EtalonFilename('OptimizationParametersToJson\MultipleDepotMultipleDriverWith24StopsTimeWindow'));
end;

procedure TTestUnmarshalOptimizationParameters.SingleDepotMultipleDriverNoTimeWindow;
begin
  CheckEquals(
    TSingleDepotMultipleDriverNoTimeWindowTestDataProvider.Create,
    EtalonFilename('OptimizationParametersToJson\SingleDepotMultipleDriverNoTimeWindow'));
end;

procedure TTestUnmarshalOptimizationParameters.SingleDriverMultipleTimeWindows;
begin
  CheckEquals(
    TSingleDriverMultipleTimeWindowsTestDataProvider.Create,
    EtalonFilename('OptimizationParametersToJson\SingleDriverMultipleTimeWindows'));
end;

procedure TTestUnmarshalOptimizationParameters.SingleDriverRoundTrip;
begin
  CheckEquals(
    TSingleDriverRoundTripTestDataProvider.Create,
    EtalonFilename('OptimizationParametersToJson\SingleDriverRoundTrip'));
end;

procedure TTestUnmarshalOptimizationParameters.SingleDriverRoute10Stops;
begin
  CheckEquals(
    TSingleDriverRoute10StopsTestDataProvider.Create,
    EtalonFilename('OptimizationParametersToJson\SingleDriverRoute10Stops'));
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('JSON\Unmarshal\', TTestUnmarshalOptimizationParameters.Suite);
end.
