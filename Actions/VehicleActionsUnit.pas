unit VehicleActionsUnit;

interface

uses
  SysUtils, BaseActionUnit, System.Generics.Collections, VehicleUnit;

type
  TVehicleActions = class(TBaseAction)
  public
    function Get(VehicleId: String; out ErrorString: String): TVehicle;
    function GetList(out ErrorString: String): TVehicleList;
  end;

implementation

uses
  GenericParametersUnit, SettingsUnit;

{ TVehicleActions }

function TVehicleActions.Get(VehicleId: String;
  out ErrorString: String): TVehicle;
var
  Parameters: TGenericParameters;
begin
  // todo 5: возвращает список всех автомобилей, а не только VehicleId. Спросил у Олега.
  Parameters := TGenericParameters.Create;
  try
    Parameters.AddParameter('vehicle_id', VehicleId);

    Result := FConnection.Get(TSettings.EndPoints.Vehicles,
      Parameters, TVehicle, ErrorString) as TVehicle;

    if (Result = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'Vehicle not got';
  finally
    FreeAndNil(Parameters);
  end;
end;

function TVehicleActions.GetList(out ErrorString: String): TVehicleList;
var
  Parameters: TGenericParameters;
  Vehicles: TVehicleList;
begin
  Result := TVehicleList.Create();

  Parameters := TGenericParameters.Create;
  try
    Vehicles := FConnection.Get(TSettings.EndPoints.Vehicles,
      Parameters, TVehicleList, ErrorString) as TVehicleList;
    try
      if (Vehicles = nil) and (ErrorString = EmptyStr) then
        ErrorString := 'Vehicles not got';

      if (Vehicles <> nil) then
      begin
        Result.AddRange(Vehicles);
        Vehicles.OwnsObjects := False;
      end;
    finally
      FreeAndNil(Vehicles);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
