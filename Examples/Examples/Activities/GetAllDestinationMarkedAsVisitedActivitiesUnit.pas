unit GetAllDestinationMarkedAsVisitedActivitiesUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetAllDestinationMarkedAsVisitedActivities = class(TBaseExample)
  public
    procedure Execute();
  end;

implementation

uses ActivityUnit, EnumsUnit;

procedure TGetAllDestinationMarkedAsVisitedActivities.Execute();
var
  ErrorString: String;
  Activities: TActivityList;
  Limit, Offset, Total: integer;
begin
  Limit := 10;
  Offset := 0;
  Activities := Route4MeManager.ActivityFeed.GetActivities(
    TActivityType.atMarkDestinationVisited, Limit, Offset, Total, ErrorString);
  try
    WriteLn('');

    if (ErrorString = EmptyStr) then
    begin
      WriteLn(Format('GetAllDestinationMarkedAsVisitedActivities executed successfully, ' +
        '%d activities returned, %d total', [Activities.Count, Total]));
      WriteLn('');
    end
    else
      WriteLn(Format('GetAllDestinationMarkedAsVisitedActivities error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Activities);
  end;
end;

end.
