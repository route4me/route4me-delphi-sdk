unit ActivityGeofenceLeftUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGeofenceLeft = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

procedure TGeofenceLeft.Execute;
var
  ErrorString: String;
begin
  Route4MeManager.ActivityFeed.GeofenceLeft(ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
    WriteLn('GeofenceLeft executed successfully')
  else
    WriteLn(Format('GeofenceLeft error: "%s"', [ErrorString]));
end;

end.
