unit ResequenceAllRouteDestinationsUnit;

interface

uses SysUtils, BaseOptimizationExampleUnit, DataObjectUnit;

type
  TResequenceAllRouteDestinations = class(TBaseOptimizationExample)
  public
    procedure Execute(RouteId: String);
  end;

implementation

uses EnumsUnit;

procedure TResequenceAllRouteDestinations.Execute(RouteId: String);
var
  DisableOptimization: boolean;
  Optimize: TOptimize;
  ErrorString: String;
begin
  DisableOptimization := False;
  Optimize := TOptimize.Distance;

  Route4MeManager.Route.ResequenceAll(
    RouteId, DisableOptimization, Optimize, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    WriteLn('ResequenceAllRouteDestinations executed successfully');
    WriteLn('');
  end
  else
    WriteLn(Format('ResequenceAllRouteDestinations error "%s"', [ErrorString]));
end;

end.
