unit GetDestinationMovedActivitiesUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetDestinationMovedActivities = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

uses ActivityUnit, EnumsUnit;

procedure TGetDestinationMovedActivities.Execute;
var
  ErrorString: String;
  Limit, Offset, Total: integer;
  Activities: TActivityList;
begin
  Limit := 10;
  Offset := 0;
  Activities := Route4MeManager.ActivityFeed.GetActivities(
    TActivityType.atMoveDestination, Limit, Offset, Total, ErrorString);
  try
    WriteLn('');

    if (ErrorString = EmptyStr) then
    begin
      WriteLn(Format('GetDestinationMovedActivities executed successfully, ' +
        '%d activities returned, %d total', [Activities.Count, Total]));
      WriteLn('');
    end
    else
      WriteLn(Format('GetDestinationMovedActivities error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Activities);
  end;
end;

end.
