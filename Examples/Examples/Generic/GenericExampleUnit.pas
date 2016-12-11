unit GenericExampleUnit;

interface

uses SysUtils, BaseExampleUnit, IConnectionUnit;

type
  TGenericExample = class(TBaseExample)
  public
    procedure Execute(Connection: IConnection);
  end;

implementation

uses Route4MeManagerUnit, GenericParametersUnit, DataObjectUnit, SettingsUnit;

procedure TGenericExample.Execute(Connection: IConnection);
var
  Parameters: TGenericParameters;
  DataObjects: TDataObjectRouteList;
  Route: TDataObjectRoute;
  Uri: String;
  Route4Me: TRoute4MeManager;
  ErrorMessage: String;
begin
  Route4Me := TRoute4MeManager.Create(Connection);
  try
    Parameters := TGenericParameters.Create();
    try
      // number of records per page
      Parameters.AddParameter('limit', '10');
      // the page offset starting at zero
      Parameters.AddParameter('Offset', '5');

      Uri := TSettings.EndPoints.Main + '/api.v4/route.php';
      DataObjects := Route4Me.Connection.Get(
        Uri, Parameters, TDataObjectRouteList, ErrorMessage) as TDataObjectRouteList;
      try
        WriteLn('');

        if (DataObjects <> nil) then
        begin
          WriteLn(Format('GenericExample executed successfully, %d routes returned',
            [DataObjects.Count]));
          WriteLn('');

          for Route in DataObjects do
            WriteLn(Format('RouteID: %s', [Route.RouteID]));
        end
        else
          WriteLn(Format('GenericExample error "%s"', [ErrorMessage]));
      finally
        FreeAndNil(DataObjects);
      end;
    finally
      FreeAndNil(Parameters);
    end;
  finally
    FreeAndNil(Route4Me);
  end;
end;

end.
