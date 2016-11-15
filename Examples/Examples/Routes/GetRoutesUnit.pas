unit GetRoutesUnit;

interface

uses SysUtils, BaseExampleUnit, DataObjectUnit;

type
  TGetRoutes = class(TBaseExample)
  public
    function Execute: TDataObjectRouteArray;
  end;

implementation

uses RouteParametersQueryUnit;

function TGetRoutes.Execute: TDataObjectRouteArray;
var
  Parameters: TRouteParametersQuery;
  ErrorString: String;
  Route: TDataObjectRoute;
  i: integer;
begin
  Parameters := TRouteParametersQuery.Create();
  try
    Parameters.Limit := 10;
    Parameters.Offset := 5;

    Result := Route4MeManager.Route.GetList(Parameters, ErrorString);
    WriteLn('');

    if (Length(Result) > 0) then
    begin
      WriteLn(Format('GetRoutes executed successfully, %d routes returned',
        [Length(Result)]));
      WriteLn('');

      for Route in Result do
        WriteLn(Format('RouteId: %s', [Route.RouteId]));
    end
    else
      WriteLn(Format('GetRoutes error "%s"', [ErrorString]));
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
