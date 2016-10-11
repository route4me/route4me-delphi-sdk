unit TestMultipleDepotMultipleDriverUnit;

interface

uses
  TestFramework, TestRoute4MeManagerUnit,
  DataObjectUnit;

type
  TTestMultipleDepotMultipleDriver = class(TTestRoute4MeManager)
  protected
    procedure InitOptimizationParametersProvider; override;
    procedure CheckResult(dataObject: TDataObject); override;
  end;

implementation

uses
  SingleDriverRoundTripTestDataProviderUnit;

procedure TTestMultipleDepotMultipleDriver.CheckResult(dataObject: TDataObject);
begin
end;

procedure TTestMultipleDepotMultipleDriver.InitOptimizationParametersProvider;
begin
  FTestDataOptimizationParametersProvider := TSingleDriverRoundTripTestDataProvider.Create;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('Optimization', TTestMultipleDepotMultipleDriver.Suite);
end.
