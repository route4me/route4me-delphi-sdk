unit UpdateRouteUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TUpdateRoute = class(TBaseExample)
  public
    procedure Execute(RouteId: String);
  end;

implementation

uses RouteParametersUnit, RouteParametersQueryUnit, DataObjectUnit, EnumsUnit;

procedure TUpdateRoute.Execute(RouteId: String);
var
  RouteParameters: TRouteParametersQuery;
  ErrorString: String;
  Route: TDataObjectRoute;
begin
  RouteParameters := TRouteParametersQuery.Create;
  try
    RouteParameters.RouteId := RouteId;
    RouteParameters.Parameters := TRouteParameters.Create;
    TRouteParameters(RouteParameters.Parameters.Value).RouteName := 'New name of the route';

    Route := Route4MeManager.Route.Update(RouteParameters, ErrorString);

    WriteLn('');
    try
      if (Route <> nil) then
      begin
        WriteLn('UpdateRoute executed successfully');
        WriteLn(Format('Route ID: %s', [Route.RouteId]));
      end
      else
        WriteLn(Format('UpdateRoute error: %s', [ErrorString]));
    finally
      FreeAndNil(Route);
    end;
  finally
    FreeAndNil(RouteParameters);
  end;
end;

end.
