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
  ErrorString: String;
begin
  Result := Route4MeManager.ActivityFeed.LogCustomActivity(RouteId, Message, ErrorString);

  WriteLn('');

  if (Result) then
    WriteLn('LogCustomActivity executed successfully')
  else
    WriteLn(Format('LogCustomActivity error: "%s"', [ErrorString]));
end;

end.
