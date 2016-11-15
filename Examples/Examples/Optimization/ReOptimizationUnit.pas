unit ReOptimizationUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TReOptimization = class(TBaseExample)
  public
    procedure Execute(OptimizationProblemId: String);
  end;

implementation

uses OptimizationParametersUnit, DataObjectUnit, EnumsUnit;

procedure TReOptimization.Execute(OptimizationProblemId: String);
var
  Parameters: TOptimizationParameters;
  ErrorString: String;
  DataObject: TDataObject;
begin
  Parameters := TOptimizationParameters.Create;
  try
    Parameters.OptimizationProblemID := OptimizationProblemId;
    Parameters.ReOptimize := True;

    DataObject := Route4MeManager.Optimization.Update(Parameters, ErrorString);

    WriteLn('');
    try
      if (DataObject <> nil) then
      begin
        WriteLn('ReOptimization executed successfully');
        WriteLn(Format('Optimization Problem ID: %s',
          [DataObject.OptimizationProblemId]));
        WriteLn(Format('State: %s',
          [TOptimizationDescription[TOptimizationState(DataObject.State)]]));
      end
      else
        WriteLn(Format('ReOptimization error: "%s"', [ErrorString]));
    finally
      FreeAndNil(DataObject);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
