unit DriverArrivedOnTimeUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TDriverArrivedOnTime = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

procedure TDriverArrivedOnTime.Execute;
var
  ErrorString: String;
begin
  Route4MeManager.ActivityFeed.DriverArrivedOnTime(ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
    WriteLn('DriverArrivedOnTime executed successfully')
  else
    WriteLn(Format('DriverArrivedOnTime error: "%s"', [ErrorString]));
end;

end.
