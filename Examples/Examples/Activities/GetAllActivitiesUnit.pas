unit GetAllActivitiesUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetAllActivities = class(TBaseExample)
  public
    procedure Execute(Limit, Offset: integer);
  end;

implementation

uses ActivityUnit;

procedure TGetAllActivities.Execute(Limit, Offset: integer);
var
  ErrorString: String;
  Activities: TActivityList;
  Activity: TActivity;
  Total: integer;
begin
  Activities := Route4MeManager.ActivityFeed.GetAllActivities(
    Limit, Offset, Total, ErrorString);
  try
    WriteLn('');

    if (Activities <> nil) and (Activities.Count > 0) then
    begin
      WriteLn(Format('GetActivities executed successfully, %d activities returned',
        [Activities.Count]));
      WriteLn('');

      for Activity in Activities do
        WriteLn(Format('Activity id: %s', [Activity.Id.Value]));

      WriteLn('');
    end
    else
      WriteLn(Format('GetActivities error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Activities);
  end;
end;

end.
