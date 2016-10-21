unit OptimizationActionUnit;

interface

uses BaseActionUnit, DataObjectUnit, OptimizationParametersUnit;

type
  TOptimizationActions = class(TBaseAction)
  public
    function Run(OptimizationParameters: TOptimizationParameters;
      out ErrorString: String): TDataObject;
  end;

implementation

{ TOptimizationActions }

uses SettingsUnit;

function TOptimizationActions.Run(
  OptimizationParameters: TOptimizationParameters;
  out ErrorString: String): TDataObject;
begin
  Result := FConnection.Post(OptimizationParameters, TSettings.ApiHost, TDataObject, errorString) as TDataObject;
end;

end.
