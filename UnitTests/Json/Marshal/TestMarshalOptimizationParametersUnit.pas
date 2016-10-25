unit TestMarshalOptimizationParametersUnit;

interface

uses
  TestFramework, SysUtils, TestBaseJsonMarshalUnit;

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
  MultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProviderUnit,
  OptimizationParametersUnit;

procedure TTestMarshalOptimizationParameters.MultipleDepotMultipleDriver();
var
  DataProvider: IOptimizationParametersProvider;
  OptimizationParameters: TOptimizationParameters;
begin
  DataProvider := TMultipleDepotMultipleDriverTestDataProvider.Create;
  OptimizationParameters := DataProvider.OptimizationParametersForRequest;
  try
    CheckEquals(
      EtalonFilename('OptimizationParametersToJson\MultipleDepotMultipleDriver'),
      OptimizationParameters.ToJsonString);
  finally
    FreeAndNil(OptimizationParameters);
  end;
end;

procedure TTestMarshalOptimizationParameters.MultipleDepotMultipleDriverTimeWindow;
var
  DataProvider: IOptimizationParametersProvider;
  OptimizationParameters: TOptimizationParameters;
begin
  DataProvider := TMultipleDepotMultipleDriverTimeWindowTestDataProvider.Create;
  OptimizationParameters := DataProvider.OptimizationParametersForRequest;
  try
    CheckEquals(
      EtalonFilename('OptimizationParametersToJson\MultipleDepotMultipleDriverTimeWindow'),
      OptimizationParameters.ToJsonString);
  finally
    FreeAndNil(OptimizationParameters);
  end;
end;

procedure TTestMarshalOptimizationParameters.MultipleDepotMultipleDriverWith24StopsTimeWindow;
var
  DataProvider: IOptimizationParametersProvider;
  OptimizationParameters: TOptimizationParameters;
begin
  DataProvider := TMultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProvider.Create;
  OptimizationParameters := DataProvider.OptimizationParametersForRequest;
  try
    CheckEquals(
      EtalonFilename('OptimizationParametersToJson\MultipleDepotMultipleDriverWith24StopsTimeWindow'),
      OptimizationParameters.ToJsonString);
  finally
    FreeAndNil(OptimizationParameters);
  end;
end;

procedure TTestMarshalOptimizationParameters.SingleDepotMultipleDriverNoTimeWindow;
var
  DataProvider: IOptimizationParametersProvider;
  OptimizationParameters: TOptimizationParameters;
begin
  DataProvider := TSingleDepotMultipleDriverNoTimeWindowTestDataProvider.Create;
  OptimizationParameters := DataProvider.OptimizationParametersForRequest;
  try
    CheckEquals(
      EtalonFilename('OptimizationParametersToJson\SingleDepotMultipleDriverNoTimeWindow'),
      OptimizationParameters.ToJsonString);
  finally
    FreeAndNil(OptimizationParameters);
  end;
end;

procedure TTestMarshalOptimizationParameters.SingleDriverMultipleTimeWindows;
var
  DataProvider: IOptimizationParametersProvider;
  OptimizationParameters: TOptimizationParameters;
begin
  DataProvider := TSingleDriverMultipleTimeWindowsTestDataProvider.Create;
  OptimizationParameters := DataProvider.OptimizationParametersForRequest;
  try
    CheckEquals(
      EtalonFilename('OptimizationParametersToJson\SingleDriverMultipleTimeWindows'),
      OptimizationParameters.ToJsonString);
  finally
    FreeAndNil(OptimizationParameters);
  end;
end;

procedure TTestMarshalOptimizationParameters.SingleDriverRoundTrip;
var
  DataProvider: IOptimizationParametersProvider;
  OptimizationParameters: TOptimizationParameters;
begin
  DataProvider := TSingleDriverRoundTripTestDataProvider.Create;
  OptimizationParameters := DataProvider.OptimizationParametersForRequest;
  try
    CheckEquals(
      EtalonFilename('OptimizationParametersToJson\SingleDriverRoundTrip'),
      OptimizationParameters.ToJsonString);
  finally
    FreeAndNil(OptimizationParameters);
  end;
end;

procedure TTestMarshalOptimizationParameters.SingleDriverRoute10Stops;
var
  DataProvider: IOptimizationParametersProvider;
  OptimizationParameters: TOptimizationParameters;
begin
  DataProvider := TSingleDriverRoute10StopsTestDataProvider.Create;
  OptimizationParameters := DataProvider.OptimizationParametersForRequest;
  try
    CheckEquals(
      EtalonFilename('OptimizationParametersToJson\SingleDriverRoute10Stops'),
      OptimizationParameters.ToJsonString);
  finally
    FreeAndNil(OptimizationParameters);
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('JSON\Marshal\', TTestMarshalOptimizationParameters.Suite);
end.
