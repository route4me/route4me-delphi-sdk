unit SingleDriverRoute10StopsUnit;

interface

uses Route4MeExamplesUnit, DataObjectUnit;

type
  TSingleDriverRoute10Stops = class helper for TRoute4MeExamples
  public
    function SingleDriverRoute10Stops: TDataObject;
  end;

implementation

{ TSingleDriverRoute10Stops }

uses
  IOptimizationParametersProviderUnit,
  SingleDriverRoute10StopsTestDataProviderUnit, OptimizationParametersUnit;

function TSingleDriverRoute10Stops.SingleDriverRoute10Stops: TDataObject;
var
  DataProvider: IOptimizationParametersProvider;
  OptimizationParameters: TOptimizationParameters;
  ErrorString: String;
  DataObject: TDataObject;
begin
  DataProvider := TSingleDriverRoute10StopsTestDataProvider.Create;
  OptimizationParameters := DataProvider.OptimizationParameters;

  // Run the query
  DataObject := Route4MeManager.Optimization.Run(OptimizationParameters, ErrorString);

  // Output the result
  PrintExampleOptimizationResult('SingleDriverRoute10Stops', DataObject, ErrorString);

  Result := DataObject;
end;

end.
