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
  Routes: TArray<TDataObjectRoute>;
  Route: TDataObjectRoute;
  Route4Me: TRoute4MeManager;
begin
  Route4Me := TRoute4MeManager.Create(Connection);
  try
    Parameters := TRouteParametersQuery.Create();
    try
      Parameters.Limit := 10;
      Parameters.Offset := 5;

      Routes := Route4Me.Route.GetList(Parameters, ErrorMessage);

      if (Length(Routes) > 0) then
      begin
        WriteLn(Format(
          'GenericExampleShortcut executed successfully, %d routes returned',
          [Length(Routes)]));
        WriteLn('');

        for Route in Routes do
          WriteLn(Format('RouteID: %s', [Route.RouteId]));
      end
      else
        WriteLn(Format('GenericExampleShortcut error "%s"', [ErrorMessage]));
    finally
      FreeAndNil(Parameters);
    end;
  finally
    FreeAndNil(Route4Me);
  end;
end;

end.
