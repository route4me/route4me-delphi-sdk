unit SingleDriverRoute10StopsUnit;

interface

uses SysUtils, DataObjectUnit, BaseOptimizationExampleUnit;

type
  TSingleDriverRoute10Stops = class(TBaseOptimizationExample)
  public
    function Execute: TDataObject;
  end;

implementation

uses
  IOptimizationParametersProviderUnit, OptimizationParametersUnit,
  SingleDriverRoute10StopsTestDataProviderUnit;

function TSingleDriverRoute10Stops.Execute: TDataObject;
var
  DataProvider: IOptimizationParametersProvider;
  ErrorString: String;
  Parameters: TOptimizationParameters;
begin
  DataProvider := TSingleDriverRoute10StopsTestDataProvider.Create;

  Parameters := DataProvider.OptimizationParameters;
  try
    Result := Route4MeManager.Optimization.Run(Parameters, ErrorString);
  finally
    FreeAndNil(Parameters);
  end;

  PrintExampleOptimizationResult('SingleDriverRoute10Stops', Result, ErrorString);
end;

end.
