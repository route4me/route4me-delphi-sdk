unit TestSingleDriverRoundTripUnit;

interface

uses
  TestFramework, TestRoute4MeManagerUnit,
  DataObjectUnit;

type
  TTestSingleDriverRoundTrip = class(TTestOptimization)
  protected
    procedure InitOptimizationParametersProvider; override;
    procedure CheckResult(dataObject: TDataObject); override;
  end;

implementation

uses
  SingleDriverRoundTripTestDataProviderUnit, AddressUnit;

procedure TTestSingleDriverRoundTrip.CheckResult(dataObject: TDataObject);
const
  ExpectedSequenceNo: TArray<integer> = [0,1,2,3,4,5,6,7,8];
var
  i: integer;
begin
  CheckEquals(Length(ExpectedSequenceNo), Length(dataObject.Addresses));
  for i := 0 to Length(dataObject.Addresses) - 1 do
    CheckEquals(ExpectedSequenceNo[i], dataObject.Addresses[i].SequenceNo);
end;

procedure TTestSingleDriverRoundTrip.InitOptimizationParametersProvider;
begin
  FTestDataOptimizationParametersProvider := TSingleDriverRoundTripTestDataProvider.Create;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('Optimization', TTestSingleDriverRoundTrip.Suite);
end.
