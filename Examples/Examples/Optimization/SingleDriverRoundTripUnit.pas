unit SingleDriverRoundTripUnit;

interface

uses SysUtils, DataObjectUnit, BaseOptimizationExampleUnit;

type
  TSingleDriverRoundTrip = class(TBaseOptimizationExample)
  public
    function Execute: TDataObject;
  end;

implementation

uses
  IOptimizationParametersProviderUnit, OptimizationParametersUnit,
  SingleDriverRoundTripTestDataProviderUnit;

function TSingleDriverRoundTrip.Execute: TDataObject;
var
  DataProvider: IOptimizationParametersProvider;
  ErrorString: String;
  Parameters: TOptimizationParameters;
begin
  DataProvider := TSingleDriverRoundTripTestDataProvider.Create;

  Parameters := DataProvider.OptimizationParameters;
  try
    Result := Route4MeManager.Optimization.Run(Parameters, ErrorString);
  finally
    FreeAndNil(Parameters);
  end;

  PrintExampleOptimizationResult('SingleDriverRoundTrip', Result, ErrorString);
end;

end.
