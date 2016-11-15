unit MultipleDepotMultipleDriverTimeWindowUnit;

interface

uses SysUtils, DataObjectUnit, BaseOptimizationExampleUnit;

type
  TMultipleDepotMultipleDriverTimeWindow = class(TBaseOptimizationExample)
  public
    function Execute: TDataObject;
  end;

implementation

uses
  IOptimizationParametersProviderUnit, OptimizationParametersUnit,
  MultipleDepotMultipleDriverTimeWindowTestDataProviderUnit;

function TMultipleDepotMultipleDriverTimeWindow.Execute: TDataObject;
var
  DataProvider: IOptimizationParametersProvider;
  ErrorString: String;
  Parameters: TOptimizationParameters;
begin
  DataProvider := TMultipleDepotMultipleDriverTimeWindowTestDataProvider.Create;

  Parameters := DataProvider.OptimizationParameters;
  try
    Result := Route4MeManager.Optimization.Run(Parameters, ErrorString);
  finally
    FreeAndNil(Parameters);
  end;

  PrintExampleOptimizationResult(
    'MultipleDepotMultipleDriverTimeWindow', Result, ErrorString);
end;

end.
