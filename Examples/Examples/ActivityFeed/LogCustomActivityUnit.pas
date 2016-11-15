unit LogCustomActivityUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TLogCustomActivity = class(TBaseExample)
  public
    function Execute(Message, RouteId: String): boolean;
  end;

implementation

uses ActivityUnit;

function TLogCustomActivity.Execute(Message, RouteId: String): boolean;
var
  Activity: TActivity;
  ErrorString: String;
begin
  Activity := TActivity.Create();
  try
    Activity.ActivityType := 'user_message';
    Activity.ActivityMessage := Message;
    Activity.RouteId := routeId;

    Result := Route4MeManager.Activity.LogCustomActivity(Activity, ErrorString);

    WriteLn('');

    if (Result) then
      WriteLn('LogCustomActivity executed successfully')
    else
      WriteLn(Format('LogCustomActivity error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Activity);
  end;
end;

end.
