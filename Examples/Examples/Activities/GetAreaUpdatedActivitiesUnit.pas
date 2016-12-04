unit GetAreaUpdatedActivitiesUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetAreaUpdatedActivities = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

uses ActivityUnit, EnumsUnit;

procedure TGetAreaUpdatedActivities.Execute;
var
  ErrorString: String;
  Limit, Offset, Total: integer;
  Activities: TActivityList;
begin
  Limit := 10;
  Offset := 0;
  Activities := Route4MeManager.ActivityFeed.GetActivities(
    TActivityType.atAreaUpdated, Limit, Offset, Total, ErrorString);
  try
    WriteLn('');

    if (ErrorString = EmptyStr) then
    begin
      WriteLn(Format('GetAreaUpdatedActivities executed successfully, ' +
        '%d activities returned, %d total', [Activities.Count, Total]));
      WriteLn('');
    end
    else
      WriteLn(Format('GetAreaUpdatedActivities error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Activities);
  end;
end;

end.
