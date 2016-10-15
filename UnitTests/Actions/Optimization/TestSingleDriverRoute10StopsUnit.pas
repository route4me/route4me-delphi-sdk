unit TestSingleDriverRoute10StopsUnit;

interface

uses
  TestFramework, TestRoute4MeManagerUnit,
  DataObjectUnit;

type
  TTestSingleDriverRoute10Stops = class(TTestOptimization)
  protected
    procedure InitOptimizationParametersProvider; override;
    procedure CheckResult(dataObject: TDataObject); override;
  end;

implementation

uses
  SingleDriverRoute10StopsTestDataProviderUnit, AddressUnit;

procedure TTestSingleDriverRoute10Stops.CheckResult(dataObject: TDataObject);
const
  ExpectedSequenceNo: TArray<integer> = [0,1,8,3,7,6,5,2,4,9];
var
  i: integer;
begin
  CheckEquals(Length(ExpectedSequenceNo), Length(dataObject.Addresses));
  for i := 0 to Length(dataObject.Addresses) - 1 do
    CheckEquals(ExpectedSequenceNo[i], dataObject.Addresses[i].SequenceNo);
end;

procedure TTestSingleDriverRoute10Stops.InitOptimizationParametersProvider;
begin
  FTestDataOptimizationParametersProvider := TSingleDriverRoute10StopsTestDataProvider.Create;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('Optimization', TTestSingleDriverRoute10Stops.Suite);
end.
