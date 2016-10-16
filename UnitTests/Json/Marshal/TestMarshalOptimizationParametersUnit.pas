unit TestMarshalOptimizationParametersUnit;

interface

uses
  TestFramework, TestBaseJsonMarshalUnit;

type
  TTestMarshalOptimizationParameters = class(TTestBaseJsonMarshal)
  published
    procedure MultipleDepotMultipleDriver();
    procedure MultipleDepotMultipleDriverTimeWindow();
    procedure MultipleDepotMultipleDriverWith24StopsTimeWindow();

    procedure SingleDepotMultipleDriverNoTimeWindow();
    procedure SingleDriverMultipleTimeWindows();
    procedure SingleDriverRoundTrip();
    procedure SingleDriverRoute10Stops();
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

procedure TTestMarshalOptimizationParameters.MultipleDepotMultipleDriver();
var
  DataProvider: IOptimizationParametersProvider;
begin
  DataProvider := TMultipleDepotMultipleDriverTestDataProvider.Create;
  try
    CheckEquals(
      EtalonFilename('OptimizationParametersToJson\MultipleDepotMultipleDriver'),
      DataProvider.OptimizationParameters.ToJsonString);
  finally
    DataProvider := nil;
  end;
end;

procedure TTestMarshalOptimizationParameters.MultipleDepotMultipleDriverTimeWindow;
var
  DataProvider: IOptimizationParametersProvider;
begin
  DataProvider := TMultipleDepotMultipleDriverTimeWindowTestDataProvider.Create;
  try
    CheckEquals(
      EtalonFilename('OptimizationParametersToJson\MultipleDepotMultipleDriverTimeWindow'),
      DataProvider.OptimizationParameters.ToJsonString);
  finally
    DataProvider := nil;
  end;
end;

procedure TTestMarshalOptimizationParameters.MultipleDepotMultipleDriverWith24StopsTimeWindow;
var
  DataProvider: IOptimizationParametersProvider;
begin
  DataProvider := TMultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProvider.Create;
  try
    CheckEquals(
      EtalonFilename('OptimizationParametersToJson\MultipleDepotMultipleDriverWith24StopsTimeWindow'),
      DataProvider.OptimizationParameters.ToJsonString);
  finally
    DataProvider := nil;
  end;
end;

procedure TTestMarshalOptimizationParameters.SingleDepotMultipleDriverNoTimeWindow;
var
  DataProvider: IOptimizationParametersProvider;
begin
  DataProvider := TSingleDepotMultipleDriverNoTimeWindowTestDataProvider.Create;
  try
    CheckEquals(
      EtalonFilename('OptimizationParametersToJson\SingleDepotMultipleDriverNoTimeWindow'),
      DataProvider.OptimizationParameters.ToJsonString);
  finally
    DataProvider := nil;
  end;
end;

procedure TTestMarshalOptimizationParameters.SingleDriverMultipleTimeWindows;
var
  DataProvider: IOptimizationParametersProvider;
begin
  DataProvider := TSingleDriverMultipleTimeWindowsTestDataProvider.Create;
  try
    CheckEquals(
      EtalonFilename('OptimizationParametersToJson\SingleDriverMultipleTimeWindows'),
      DataProvider.OptimizationParameters.ToJsonString);
  finally
    DataProvider := nil;
  end;
end;

procedure TTestMarshalOptimizationParameters.SingleDriverRoundTrip;
var
  DataProvider: IOptimizationParametersProvider;
begin
  DataProvider := TSingleDriverRoundTripTestDataProvider.Create;
  try
    CheckEquals(
      EtalonFilename('OptimizationParametersToJson\SingleDriverRoundTrip'),
      DataProvider.OptimizationParameters.ToJsonString);
  finally
    DataProvider := nil;
  end;
end;

procedure TTestMarshalOptimizationParameters.SingleDriverRoute10Stops;
var
  DataProvider: IOptimizationParametersProvider;
begin
  DataProvider := TSingleDriverRoute10StopsTestDataProvider.Create;
  try
    CheckEquals(
      EtalonFilename('OptimizationParametersToJson\SingleDriverRoute10Stops'),
      DataProvider.OptimizationParameters.ToJsonString);
  finally
    DataProvider := nil;
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('JSON\Marshal\', TTestMarshalOptimizationParameters.Suite);
end.
