unit ReoptimizeRouteUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TReoptimizeRoute = class(TBaseExample)
  public
    procedure Execute(RouteId: String);
  end;

implementation

uses DataObjectUnit, EnumsUnit, RouteParametersQueryUnit;

procedure TReoptimizeRoute.Execute(RouteId: String);
var
  Parameters: TRouteParametersQuery;
  ErrorString: String;
  Route: TDataObjectRoute;
begin
  Parameters := TRouteParametersQuery.Create;
  try
    Parameters.RouteId := RouteId;
    Parameters.ReOptimize := True;

    Route := Route4MeManager.Route.Update(Parameters, ErrorString);

    WriteLn('');
    try
      if (Route <> nil) then
      begin
        WriteLn('ReoptimizeRoute executed successfully');
        WriteLn(Format('Route ID: %s', [Route.RouteId]));
      end
      else
        WriteLn(Format('ReoptimizeRoute error: "%s"', [ErrorString]));
    finally
      FreeAndNil(Route);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
