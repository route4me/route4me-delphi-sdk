unit GetVehiclesUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetVehicles = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

uses VehicleUnit;

procedure TGetVehicles.Execute;
var
  ErrorString: String;
  Vehicles: TVehicleList;
begin
  Vehicles := Route4MeManager.Vehicle.GetList(ErrorString);
  try
    WriteLn('');

    if (Vehicles.Count > 0) then
      WriteLn(Format('GetVehicles executed successfully, %d vehicles returned',
        [Vehicles.Count]))
    else
      WriteLn(Format('GetVehicles error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Vehicles);
  end;
end;

end.
