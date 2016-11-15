unit RemoveDestinationFromOptimizationUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TRemoveDestinationFromOptimization = class(TBaseExample)
  public
    procedure Execute(
      OptimizationId: String; DestinationId: integer; AndReOptimize: boolean);
  end;

implementation

procedure TRemoveDestinationFromOptimization.Execute(
  OptimizationId: String; DestinationId: integer; AndReOptimize: boolean);
var
  ErrorString: String;
  Removed: boolean;
begin
  Removed := Route4MeManager.Optimization.RemoveDestination(
    OptimizationId, DestinationId, ErrorString);

  WriteLn('');

  if (Removed) then
  begin
    WriteLn('RemoveAddressFromOptimization executed successfully');
    WriteLn(Format('Optimization Problem ID: %s, Destination ID: %d',
      [OptimizationId, DestinationId]));
  end
  else
    WriteLn(Format('RemoveAddressFromOptimization error: %s', [ErrorString]));
end;

end.
