unit SearchRoutesForSpecifiedTextUnit;

interface

uses SysUtils, BaseExampleUnit, DataObjectUnit;

type
  TSearchRoutesForSpecifiedText = class(TBaseExample)
  public
    function Execute(Text: String): TDataObjectRouteList;
  end;

implementation

function TSearchRoutesForSpecifiedText.Execute(Text: String): TDataObjectRouteList;
var
  ErrorString: String;
  Route: TDataObjectRoute;
begin
  Result := Route4MeManager.Route.GetList(Text, ErrorString);
  WriteLn('');

  if (Result <> nil) and (Result.Count > 0) then
  begin
    WriteLn(Format('SearchRoutesForSpecifiedText executed successfully, %d routes returned',
      [Result.Count]));
    WriteLn('');

    for Route in Result do
      WriteLn(Format('RouteId: %s', [Route.RouteId]));
  end
  else
    WriteLn(Format('SearchRoutesForSpecifiedText error "%s"', [ErrorString]));
end;

end.
