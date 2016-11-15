unit MultipleDepotMultipleDriverUnit;

interface

uses SysUtils, DataObjectUnit, BaseOptimizationExampleUnit;

type
  TMultipleDepotMultipleDriver = class(TBaseOptimizationExample)
  public
    function Execute: TDataObject;
  end;

implementation

uses
  IOptimizationParametersProviderUnit, OptimizationParametersUnit,
  MultipleDepotMultipleDriverTestDataProviderUnit;

function TMultipleDepotMultipleDriver.Execute: TDataObject;
var
  DataProvider: IOptimizationParametersProvider;
  ErrorString: String;
  Parameters: TOptimizationParameters;
begin
  DataProvider := TMultipleDepotMultipleDriverTestDataProvider.Create;

  Parameters := DataProvider.OptimizationParameters;
  try
    Result := Route4MeManager.Optimization.Run(Parameters, ErrorString);
  finally
    FreeAndNil(Parameters);
  end;

  PrintExampleOptimizationResult('MultipleDepotMultipleDriver', Result, ErrorString);
end;

end.
