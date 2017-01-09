unit GetRouteUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetRoute = class(TBaseExample)
  public
    procedure Execute(RouteId: String; GetRouteDirections, GetRoutePathPoints: boolean);
  end;

implementation

uses RouteParametersQueryUnit, DataObjectUnit, EnumsUnit;

procedure TGetRoute.Execute(RouteId: String; GetRouteDirections,
  GetRoutePathPoints: boolean);
var
  ErrorString: String;
  Route: TDataObjectRoute;
begin
  Route := Route4MeManager.Route.Get(
    RouteId, GetRouteDirections, GetRoutePathPoints, ErrorString);

  WriteLn('');
  try
    if (Route <> nil) then
    begin
      WriteLn('GetRoute executed successfully');
      WriteLn(Format('Route ID: %s', [Route.RouteId]));
      WriteLn(Format('State: %s',
        [TOptimizationDescription[TOptimizationState(Route.State)]]));
      if (Length(Route.Directions) > 0) then
        WriteLn(Format('Directions: length = %d', [Length(Route.Directions)]));
      if (Length(Route.Path) > 0) then
        WriteLn(Format('Path: length = %d', [Length(Route.Path)]));
    end
    else
      WriteLn(Format('GetRoute error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Route);
  end;
end;

end.
