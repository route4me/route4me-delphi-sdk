unit GetVehicleUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetVehicle = class(TBaseExample)
  public
    procedure Execute(VehicleId: String);
  end;

implementation

uses VehicleUnit;

procedure TGetVehicle.Execute(VehicleId: String);
var
  ErrorString: String;
  Vehicle: TVehicle;
begin
  Vehicle := Route4MeManager.Vehicle.Get(VehicleId, ErrorString);
  try
    WriteLn('');

    if (Vehicle <> nil) then
    begin
      WriteLn('GetVehicle executed successfully');
      WriteLn(Format('Vehicle ID: %s', [Vehicle.Id.Value]));
    end
    else
      WriteLn(Format('GetVehicle error: %s', [ErrorString]));
  finally
    FreeAndNil(Vehicle);
  end;
end;

end.
