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
  Parameters: TRouteParametersQuery;
  ErrorString: String;
  Route: TDataObjectRoute;
begin
  Parameters := TRouteParametersQuery.Create;
  try
    Parameters.RouteId := RouteId;

    if (GetRouteDirections) then
      Parameters.Directions := True;

    if (GetRoutePathPoints) then
      Parameters.RoutePathOutput := TRoutePathOutput.rpoPoints;

    Route := Route4MeManager.Route.Get(Parameters, ErrorString);

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
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
