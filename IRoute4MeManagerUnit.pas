unit IRoute4MeManagerUnit;

interface

uses OptimizationParametersUnit, DataObjectUnit;

type

  IRoute4MeManager = interface
    ['{2E31D4E6-C42A-4C9B-9ED5-445C3F6D6690}']
    function RunOptimization(optimizationParameters: TOptimizationParameters; out errorString: String): TDataObject;
  end;

implementation

end.
