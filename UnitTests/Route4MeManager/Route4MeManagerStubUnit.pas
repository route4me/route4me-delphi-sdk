unit Route4MeManagerStubUnit;

interface

uses IRoute4MeManagerUnit, OptimizationParametersUnit, DataObjectUnit;

type
  TRoute4MeManagerStub = class(TInterfacedObject, IRoute4MeManager)
  public
    function RunOptimization(optimizationParameters: TOptimizationParameters; out errorString: String): TDataObject;
  end;

implementation

{ TRoute4MeManagerStub }

function TRoute4MeManagerStub.RunOptimization(
  optimizationParameters: TOptimizationParameters;
  out errorString: String): TDataObject;
begin

end;

end.
