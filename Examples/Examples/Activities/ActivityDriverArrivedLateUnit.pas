unit ActivityDriverArrivedLateUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TDriverArrivedLate = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

procedure TDriverArrivedLate.Execute;
var
  ErrorString: String;
begin
  Route4MeManager.ActivityFeed.DriverArrivedLate(ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
    WriteLn('DriverArrivedLate executed successfully')
  else
    WriteLn(Format('DriverArrivedLate error: "%s"', [ErrorString]));
end;

end.
