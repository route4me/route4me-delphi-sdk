unit GetAreaRemovedActivitiesUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetAreaRemovedActivities = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

uses ActivityUnit, EnumsUnit;

procedure TGetAreaRemovedActivities.Execute;
var
  ErrorString: String;
  Limit, Offset, Total: integer;
  Activities: TActivityList;
begin
  Limit := 10;
  Offset := 0;
  Activities := Route4MeManager.ActivityFeed.GetActivities(
    TActivityType.atAreaRemoved, Limit, Offset, Total, ErrorString);
  try
    WriteLn('');

    if (ErrorString = EmptyStr) then
    begin
      WriteLn(Format('ActivityAreaRemoved executed successfully, ' +
        '%d activities returned, %d total', [Activities.Count, Total]));
      WriteLn('');
    end
    else
      WriteLn(Format('ActivityAreaRemoved error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Activities);
  end;
end;

end.
