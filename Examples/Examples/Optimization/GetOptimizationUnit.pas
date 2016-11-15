unit GetOptimizationUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetOptimization = class(TBaseExample)
  public
    procedure Execute(OptimizationProblemId: String);
  end;

implementation

uses
  OptimizationParametersUnit, DataObjectUnit, EnumsUnit;

procedure TGetOptimization.Execute(OptimizationProblemId: String);
var
  Parameters: TOptimizationParameters;
  DataObject: TDataObject;
  ErrorString: String;
begin
  Parameters := TOptimizationParameters.Create;
  try
    Parameters.OptimizationProblemID := OptimizationProblemId;

    DataObject := Route4MeManager.Optimization.Get(Parameters, ErrorString);
    try
      WriteLn('');

      if (DataObject <> nil) then
      begin
          WriteLn('GetOptimization executed successfully');
          WriteLn(Format('Optimization Problem ID: %s',
            [DataObject.OptimizationProblemId]));
          WriteLn(Format('State: %s',
            [TOptimizationDescription[TOptimizationState(DataObject.State)]]));
      end
      else
        WriteLn(Format('GetOptimization error: "%s"', [ErrorString]));
    finally
      FreeAndNil(DataObject);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
