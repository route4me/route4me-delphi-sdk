unit GetDestinationInsertedActivitiesUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetDestinationInsertedActivities = class(TBaseExample)
  public
    procedure Execute(RouteId: String);
  end;

implementation

uses ActivityUnit, EnumsUnit;

procedure TGetDestinationInsertedActivities.Execute(RouteId: String);
var
  ErrorString: String;
  Limit, Offset, Total: integer;
  Activities: TActivityList;
begin
  Limit := 10;
  Offset := 0;
  Activities := Route4MeManager.ActivityFeed.GetActivities(RouteId,
    TActivityType.atInsertDestination, Limit, Offset, Total, ErrorString);
  try
    WriteLn('');

    if (ErrorString = EmptyStr) then
    begin
      WriteLn(Format('GetDestinationInsertedActivities executed successfully, ' +
        '%d activities returned, %d total', [Activities.Count, Total]));
      WriteLn('');
    end
    else
      WriteLn(Format('GetDestinationInsertedActivities error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Activities);
  end;
end;

end.
