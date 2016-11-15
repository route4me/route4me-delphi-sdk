unit SingleDriverMultipleTimeWindowsUnit;

interface

uses SysUtils, DataObjectUnit, BaseOptimizationExampleUnit;

type
  TSingleDriverMultipleTimeWindows = class(TBaseOptimizationExample)
  public
    function Execute: TDataObject;
  end;

implementation

uses
  IOptimizationParametersProviderUnit, OptimizationParametersUnit,
  SingleDriverMultipleTimeWindowsTestDataProviderUnit;

function TSingleDriverMultipleTimeWindows.Execute: TDataObject;
var
  DataProvider: IOptimizationParametersProvider;
  ErrorString: String;
  Parameters: TOptimizationParameters;
begin
  DataProvider := TSingleDriverMultipleTimeWindowsTestDataProvider.Create;

  Parameters := DataProvider.OptimizationParameters;
  try
    Result := Route4MeManager.Optimization.Run(Parameters, ErrorString);
  finally
    FreeAndNil(Parameters);
  end;

  PrintExampleOptimizationResult(
    'SingleDriverMultipleTimeWindows', Result, ErrorString);
end;

end.
