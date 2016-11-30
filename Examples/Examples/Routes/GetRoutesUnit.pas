unit GetRoutesUnit;

interface

uses SysUtils, BaseExampleUnit, DataObjectUnit;

type
  TGetRoutes = class(TBaseExample)
  public
    function Execute(Limit, Offset: integer): TDataObjectRouteList;
  end;

implementation

function TGetRoutes.Execute(Limit, Offset: integer): TDataObjectRouteList;
var
  ErrorString: String;
  Route: TDataObjectRoute;
begin
  Result := Route4MeManager.Route.GetList(Limit, Offset, ErrorString);
  WriteLn('');

  if (Result <> nil) and (Result.Count > 0) then
  begin
    WriteLn(Format('GetRoutes executed successfully, %d routes returned',
      [Result.Count]));
    WriteLn('');

    for Route in Result do
      WriteLn(Format('RouteId: %s', [Route.RouteId]));
  end
  else
    WriteLn(Format('GetRoutes error "%s"', [ErrorString]));
end;

end.
