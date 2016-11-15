unit SingleDepotMultipleDriverNoTimeWindowUnit;

interface

uses SysUtils, DataObjectUnit, BaseOptimizationExampleUnit;

type
  TSingleDepotMultipleDriverNoTimeWindow = class(TBaseOptimizationExample)
  public
    function Execute: TDataObject;
  end;

implementation

uses
  IOptimizationParametersProviderUnit, OptimizationParametersUnit,
  SingleDepotMultipleDriverNoTimeWindowTestDataProviderUnit;

function TSingleDepotMultipleDriverNoTimeWindow.Execute: TDataObject;
var
  DataProvider: IOptimizationParametersProvider;
  ErrorString: String;
  Parameters: TOptimizationParameters;
begin
  DataProvider := TSingleDepotMultipleDriverNoTimeWindowTestDataProvider.Create;

  Parameters := DataProvider.OptimizationParameters;
  try
    Result := Route4MeManager.Optimization.Run(Parameters, ErrorString);
  finally
    FreeAndNil(Parameters);
  end;

  PrintExampleOptimizationResult(
    'SingleDepotMultipleDriverNoTimeWindow', Result, ErrorString);
end;

end.
