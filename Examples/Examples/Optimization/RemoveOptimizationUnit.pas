unit RemoveOptimizationUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TRemoveOptimization = class(TBaseExample)
  public
    procedure Execute(OptimizationProblemId: String);
  end;

implementation

procedure TRemoveOptimization.Execute(OptimizationProblemId: String);
var
  ErrorString: String;
  Removed: boolean;
begin
  Removed := Route4MeManager.Optimization.Remove(OptimizationProblemID, ErrorString);

  WriteLn('');

  if (Removed) then
  begin
    WriteLn('RemoveOptimization executed successfully');
    WriteLn(Format('Optimization Problem ID: %s', [OptimizationProblemID]));
  end
  else
    WriteLn(Format('RemoveOptimization error: "%s"', [ErrorString]));
end;

end.
