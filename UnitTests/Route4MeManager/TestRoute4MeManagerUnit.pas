unit TestRoute4MeManagerUnit;

interface

uses
  TestFramework, System.Generics.Collections,
  Route4MeManagerUnit,
  BaseRoute4MeTestUnit, DataObjectUnit,
  IOptimizationParametersProviderUnit, IRoute4MeManagerUnit;

type
  TTestRoute4MeManager = class abstract(TBaseRoute4MeTest)
  protected
    FRoute4MeManager: IRoute4MeManager;
    FTestDataOptimizationParametersProvider: IOptimizationParametersProvider;

    procedure CheckResult(dataObject: TDataObject); virtual; abstract;
    procedure InitOptimizationParametersProvider; virtual; abstract;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RunOptimization();
  end;

implementation

procedure TTestRoute4MeManager.RunOptimization;
var
  errorString: String;
  dataObject: TDataObject;
begin
  dataObject := FRoute4MeManager.RunOptimization(
    FTestDataOptimizationParametersProvider.OptimizationParameters, errorString);

  CheckResult(dataObject);
end;

procedure TTestRoute4MeManager.SetUp;
begin
  // Create the manager with the api key
  FRoute4MeManager := TRoute4MeManager.Create(c_ApiKey);

  InitOptimizationParametersProvider;
end;

procedure TTestRoute4MeManager.TearDown;
begin
  FTestDataOptimizationParametersProvider := nil;
  FRoute4MeManager := nil;
end;

end.
