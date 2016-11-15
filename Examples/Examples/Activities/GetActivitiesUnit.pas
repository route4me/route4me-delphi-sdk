unit GetActivitiesUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetActivities = class(TBaseExample)
  public
    procedure Execute(RouteId: String);
  end;

implementation

uses ActivityUnit, ActivityParametersUnit;

procedure TGetActivities.Execute(RouteId: String);
var
  ErrorString: String;
  Parameters: TActivityParameters;
  Activities: TActivityArray;
  Activity: TActivity;
  i: integer;
begin
  Parameters := TActivityParameters.Create();
  try
    Parameters.RouteId := RouteId;
    Parameters.Limit := 10;
    Parameters.Offset := 0;

    Activities := Route4MeManager.Activity.GetActivityFeed(Parameters, ErrorString);
    try
      WriteLn('');

      if (Length(Activities) > 0) then
      begin
        WriteLn(Format('GetActivities executed successfully, %d activities returned',
          [Length(Activities)]));
        WriteLn('');

        for Activity in Activities do
          WriteLn(Format('Activity id: %s', [Activity.ActivityId.Value]));

        WriteLn('');
      end
      else
        WriteLn(Format('GetActivities error: "%s"', [ErrorString]));
    finally
      for i := Length(Activities) - 1 downto 0 do
        FreeAndNil(Activities[i]);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
