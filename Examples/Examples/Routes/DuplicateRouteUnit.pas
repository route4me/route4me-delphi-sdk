unit DuplicateRouteUnit;

interface

uses SysUtils, BaseExampleUnit, NullableBasicTypesUnit;

type
  TDuplicateRoute = class(TBaseExample)
  public
    function Execute(RouteId: String): NullableString;
  end;

implementation

uses RouteParametersQueryUnit;

function TDuplicateRoute.Execute(RouteId: String): NullableString;
var
  ErrorString: String;
  Parameters: TRouteParametersQuery;
begin
  Parameters := TRouteParametersQuery.Create;
  try
    Parameters.RouteId := RouteId;

    Result := Route4MeManager.Route.Duplicate(Parameters, ErrorString);

    WriteLn('');

    if (Result.IsNotNull) then
    begin
      WriteLn(Format('DuplicateRoute executed successfully, duplicated route ID: %s',
        [Result.Value]));
      WriteLn('');
    end
    else
      WriteLn(Format('DuplicateRoute error "%s"', [ErrorString]));
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
