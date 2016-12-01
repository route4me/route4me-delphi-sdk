unit DriverArrivedEarlyUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TDriverArrivedEarly = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

procedure TDriverArrivedEarly.Execute;
var
  ErrorString: String;
begin
  Route4MeManager.ActivityFeed.DriverArrivedEarly(ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
    WriteLn('DriverArrivedEarly executed successfully')
  else
    WriteLn(Format('DriverArrivedEarly error: "%s"', [ErrorString]));
end;

end.
