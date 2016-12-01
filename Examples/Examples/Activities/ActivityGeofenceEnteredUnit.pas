unit ActivityGeofenceEnteredUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGeofenceEntered = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

procedure TGeofenceEntered.Execute;
var
  ErrorString: String;
begin
  Route4MeManager.ActivityFeed.GeofenceEntered(ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
    WriteLn('GeofenceEntered executed successfully')
  else
    WriteLn(Format('GeofenceEntered error: "%s"', [ErrorString]));
end;

end.
