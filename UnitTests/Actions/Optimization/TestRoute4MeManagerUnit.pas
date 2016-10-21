unit TestRoute4MeManagerUnit;

interface

uses
  TestFramework, System.Generics.Collections,
  Route4MeManagerUnit,
  BaseRoute4MeTestUnit, DataObjectUnit,
  IOptimizationParametersProviderUnit, IRoute4MeManagerUnit;

type
  TTestOptimization = class abstract(TBaseRoute4MeTest)
  protected
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

procedure TTestOptimization.RunOptimization;
var
  errorString: String;
  dataObject: TDataObject;
begin
  dataObject := FRoute4MeManager.Optimization.Run(
    FTestDataOptimizationParametersProvider.OptimizationParameters, errorString);

  CheckResult(dataObject);
end;

procedure TTestOptimization.SetUp;
begin
  inherited;

  InitOptimizationParametersProvider;
end;

procedure TTestOptimization.TearDown;
begin
  FTestDataOptimizationParametersProvider := nil;

  inherited;
end;

end.
