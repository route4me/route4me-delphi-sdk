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
  DataObject: TDataObject;
  ErrorString: String;
begin
  DataObject := Route4MeManager.Optimization.Get(OptimizationProblemId, ErrorString);
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
end;

end.
