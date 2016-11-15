unit MultipleDepotMultipleDriverWith24StopsTimeWindowUnit;

interface

uses SysUtils, DataObjectUnit, BaseOptimizationExampleUnit;

type
  TMultipleDepotMultipleDriverWith24StopsTimeWindow = class(TBaseOptimizationExample)
  public
    function Execute: TDataObject;
  end;

implementation

uses
  IOptimizationParametersProviderUnit, OptimizationParametersUnit,
  MultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProviderUnit;

function TMultipleDepotMultipleDriverWith24StopsTimeWindow.Execute: TDataObject;
var
  DataProvider: IOptimizationParametersProvider;
  ErrorString: String;
  Parameters: TOptimizationParameters;
begin
  DataProvider := TMultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProvider.Create;

  Parameters := DataProvider.OptimizationParameters;
  try
    Result := Route4MeManager.Optimization.Run(Parameters, ErrorString);
  finally
    FreeAndNil(Parameters);
  end;

  PrintExampleOptimizationResult(
    'MultipleDepotMultipleDriverWith24StopsTimeWindow', Result, ErrorString);
end;

end.
