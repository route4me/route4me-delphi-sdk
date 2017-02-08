unit TestVehicleSamplesUnit;

interface

uses
  TestFramework, Classes, SysUtils,
  BaseTestOnlineExamplesUnit;

type
  TTestVehicleSamples = class(TTestOnlineExamples)
  private
    procedure GetVehicle; // working, but slowly and incorrectly
    procedure GetVehicles; // working, but slowly
  published
  end;

implementation

uses
  VehicleUnit;

procedure TTestVehicleSamples.GetVehicle;
var
  ErrorString: String;
  VehicleId: String;
  Vehicle: TVehicle;
begin
  VehicleId := '0A18C14AB42F6B6D7E830CE4082493E3';
  Vehicle := FRoute4MeManager.Vehicle.Get(VehicleId, ErrorString);
  try
    CheckEquals(EmptyStr, ErrorString);
    CheckNotNull(Vehicle);
  finally
    FreeAndNil(Vehicle);
  end;

  VehicleId := 'random_id_dDFsd2@D3d';
  Vehicle := FRoute4MeManager.Vehicle.Get(VehicleId, ErrorString);
  try
    CheckNotEquals(EmptyStr, ErrorString);
    CheckNull(Vehicle);
  finally
    FreeAndNil(Vehicle);
  end;
end;

procedure TTestVehicleSamples.GetVehicles;
var
  ErrorString: String;
  Vehicles: TVehicleList;
begin
  Vehicles := FRoute4MeManager.Vehicle.GetList(ErrorString);
  try
    CheckEquals(EmptyStr, ErrorString);
    CheckNotNull(Vehicles);
    CheckTrue(Vehicles.Count > 0);
  finally
    FreeAndNil(Vehicles);
  end;
end;

initialization
  RegisterTest('Examples\Online\Vehicle\', TTestVehicleSamples.Suite);
end.
