unit SingleDriverRoute10StopsUnit;

interface

uses
  SysUtils,
  Route4MeExamplesUnit, DataObjectUnit, BaseOptimizationExampleUnit;

type
  TSingleDriverRoute10Stops = class(TBaseOptimizationExample)
  public
    function Run: TDataObject;
  end;

implementation

uses
  IOptimizationParametersProviderUnit, OptimizationParametersUnit,
  SingleDriverRoute10StopsTestDataProviderUnit;

function TSingleDriverRoute10Stops.Run: TDataObject;
var
  DataProvider: IOptimizationParametersProvider;
  ErrorString: String;
  DataObject: TDataObject;
  Parameters: TOptimizationParameters;
begin
  DataProvider := TSingleDriverRoute10StopsTestDataProvider.Create;

  // Run the query
  Parameters := DataProvider.OptimizationParameters;
  try
    DataObject := Route4MeManager.Optimization.Run(Parameters, ErrorString);
  finally
    FreeAndNil(Parameters);
  end;

  // Output the result
  PrintExampleOptimizationResult('SingleDriverRoute10Stops', DataObject, ErrorString);

  Result := DataObject;
end;

end.
