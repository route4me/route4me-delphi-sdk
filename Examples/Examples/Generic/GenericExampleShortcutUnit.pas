unit GenericExampleShortcutUnit;

interface

uses SysUtils, BaseExampleUnit, IConnectionUnit;

type
  TGenericExampleShortcut = class(TBaseExample)
  public
    procedure Execute(Connection: IConnection);
  end;

implementation

uses Route4MeManagerUnit, DataObjectUnit, RouteParametersQueryUnit;

procedure TGenericExampleShortcut.Execute(Connection: IConnection);
var
  ErrorMessage: String;
  Parameters: TRouteParametersQuery;
  Routes: TDataObjectRouteList;
  Route: TDataObjectRoute;
  Route4Me: TRoute4MeManager;
  Limit, Offset: integer;
begin
  Route4Me := TRoute4MeManager.Create(Connection);
  try
    Limit := 10;
    Offset := 5;

    Routes := Route4Me.Route.GetList(Limit, Offset, ErrorMessage);
    try
      if (Routes <> nil) and (Routes.Count > 0) then
      begin
        WriteLn(Format(
          'GenericExampleShortcut executed successfully, %d routes returned',
          [Routes.Count]));
        WriteLn('');

        for Route in Routes do
          WriteLn(Format('RouteID: %s', [Route.RouteId]));
      end
      else
        WriteLn(Format('GenericExampleShortcut error "%s"', [ErrorMessage]));
    finally
      FreeAndNil(Routes);
    end;
  finally
    FreeAndNil(Route4Me);
  end;
end;

end.
