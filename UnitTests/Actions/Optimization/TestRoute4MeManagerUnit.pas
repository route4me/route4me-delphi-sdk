unit TestRoute4MeManagerUnit;

interface

uses
  TestFramework, System.Generics.Collections, SysUtils,
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

uses OptimizationParametersUnit;

procedure TTestOptimization.RunOptimization;
var
  ErrorString: String;
  DataObject: TDataObject;
  OptimizationParameters: TOptimizationParameters;
begin
  OptimizationParameters := FTestDataOptimizationParametersProvider.OptimizationParametersForRequest;
  try
    DataObject := FRoute4MeManager.Optimization.Run(
      OptimizationParameters, ErrorString);
    try
      CheckResult(DataObject);
    finally
      FreeAndNil(DataObject);
    end;
  finally
    FreeAndNil(OptimizationParameters);
  end;
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
