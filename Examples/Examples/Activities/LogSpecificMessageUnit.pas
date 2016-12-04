unit LogSpecificMessageUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TLogSpecificMessage = class(TBaseExample)
  public
    function Execute(Message, RouteId: String): boolean;
  end;

implementation

function TLogSpecificMessage.Execute(Message, RouteId: String): boolean;
var
  ErrorString: String;
begin
  Result := Route4MeManager.ActivityFeed.LogSpecificMessage(RouteId, Message, ErrorString);

  WriteLn('');

  if (Result) then
    WriteLn('LogSpecificMessage executed successfully')
  else
    WriteLn(Format('LogSpecificMessage error: "%s"', [ErrorString]));
end;

end.
