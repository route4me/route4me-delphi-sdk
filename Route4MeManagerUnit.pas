unit Route4MeManagerUnit;

interface

uses OptimizationParametersUnit, DataObjectUnit, IRoute4MeManagerUnit;

type
  TRoute4MeManager = class(TInterfacedObject, IRoute4MeManager)
  private
    FApiKey: String;
  public
    constructor Create(ApiKey: String);

    function RunOptimization(optimizationParameters: TOptimizationParameters; out errorString: String): TDataObject;
  end;

implementation

{ TRoute4MeManager }

uses EnumsUnit, R4MeInfrastructureSettingsUnit;

constructor TRoute4MeManager.Create(ApiKey: String);
begin
  FApiKey := ApiKey;
end;

function TRoute4MeManager.RunOptimization(
  OptimizationParameters: TOptimizationParameters;
  out ErrorString: String): TDataObject;
begin
{    Result := GetJsonObjectFromAPI<TDataObject>(
      optimizationParameters, TR4MeInfrastructureSettings.ApiHost,
      HttpMethodType.Post, False, ErrorString);}
end;

end.
