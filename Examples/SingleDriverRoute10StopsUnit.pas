unit SingleDriverRoute10StopsUnit;

interface

uses SysUtils, Route4MeExamplesUnit, DataObjectUnit;

type
  TSingleDriverRoute10Stops = class(TRoute4MeExamples)
  public
    function Run(): TDataObject;
  end;

implementation

{ TSingleDriverRoute10Stops }

uses IOptimizationParametersProviderUnit, OptimizationParametersUnit,
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
