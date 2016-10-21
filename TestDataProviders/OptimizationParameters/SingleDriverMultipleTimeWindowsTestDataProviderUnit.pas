unit SingleDriverMultipleTimeWindowsTestDataProviderUnit;

interface

uses
  SysUtils,
  BaseOptimizationParametersProviderUnit, AddressUnit, RouteParametersUnit;

type
  TSingleDriverMultipleTimeWindowsTestDataProvider = class(TBaseOptimizationParametersProvider)
  protected
    function MakeAddresses(): TArray<TAddress>; override;
    function MakeRouteParameters(): TRouteParameters; override;
  public

  end;

implementation

{ TSingleDriverMultipleTimeWindowsTestDataProvider }

uses
  DateUtils,
  EnumsUnit, UtilsUnit, NullableBasicTypesUnit;

function TSingleDriverMultipleTimeWindowsTestDataProvider.MakeAddresses: TArray<TAddress>;
var
  Address: TAddress;
begin
  Result := TArray<TAddress>.Create();

  Address := TAddress.Create(
    '3634 W Market St, Fairlawn, OH 44333', 41.135762259364, -81.629313826561, NullableInteger.Null);
  // all possible originating locations are depots, should be marked as true
  // stylistically we recommend all depots should be at the top of the destinations list
  Address.IsDepot := True;
  AddAddress(Address, Result);

  AddAddress(TAddress.Create(
    '1218 Ruth Ave, Cuyahoga Falls, OH 44221', 41.135762259364, -81.629313826561, 300,
    //together these two specify the time window of a destination
    //seconds offset relative to the route start time for the open availability of a destination
    6 * 3600 + 00 * 60,
    //seconds offset relative to the route end time for the open availability of a destination
    6 * 3600 + 30 * 60,
    // Second 'TimeWindowStart'
    7 * 3600 + 00 * 60,
    // Second 'TimeWindowEnd'
    7 * 3600 + 20 * 60),
    Result);

  AddAddress(TAddress.Create(
    '512 Florida Pl, Barberton, OH 44203', 41.003671512008, -81.598461046815, 300,
    7 * 3600 + 30 * 60, 7 * 3600 + 40 * 60, 8 * 3600 + 00 * 60, 8 * 3600 + 10 * 60),
    Result);
  AddAddress(TAddress.Create(
    '512 Florida Pl, Barberton, OH 44203', 41.003671512008, -81.598461046815, 100,
    8 * 3600 + 30 * 60, 8 * 3600 + 40 * 60, 8 * 3600 + 50 * 60, 9 * 3600 + 00 * 60),
    Result);
  AddAddress(TAddress.Create(
    '3495 Purdue St, Cuyahoga Falls, OH 44221', 41.162971496582, -81.479049682617, 300,
    9 * 3600 + 00 * 60, 9 * 3600 + 15 * 60, 9 * 3600 + 30 * 60, 9 * 3600 + 45 * 60),
    Result);
  AddAddress(TAddress.Create(
    '1659 Hibbard Dr, Stow, OH 44224', 41.194505989552, -81.443351581693, 300,
    10 * 3600 + 00 * 60, 10 * 3600 + 15 * 60, 10 * 3600 + 30 * 60, 10 * 3600 + 45 * 60),
    Result);
  AddAddress(TAddress.Create(
    '2705 N River Rd, Stow, OH 44224', 41.145240783691, -81.410247802734, 300,
    11 * 3600 + 00 * 60, 11 * 3600 + 15 * 60, 11 * 3600 + 30 * 60, 11 * 3600 + 45 * 60),
    Result);
  AddAddress(TAddress.Create(
    '10159 Bissell Dr, Twinsburg, OH 44087', 41.340042114258, -81.421226501465, 300,
    12 * 3600 + 00 * 60, 12 * 3600 + 15 * 60, 12 * 3600 + 30 * 60, 12 * 3600 + 45 * 60),
    Result);
  AddAddress(TAddress.Create(
    '367 Cathy Dr, Munroe Falls, OH 44262', 41.148578643799, -81.429229736328, 300,
    13 * 3600 + 00 * 60, 13 * 3600 + 15 * 60, 13 * 3600 + 30 * 60, 13 * 3600 + 45 * 60),
    Result);
  AddAddress(TAddress.Create(
    '367 Cathy Dr, Munroe Falls, OH 44262', 41.148578643799, -81.429229736328, 300,
    14 * 3600 + 00 * 60, 14 * 3600 + 15 * 60, 14 * 3600 + 30 * 60, 14 * 3600 + 45 * 60),
    Result);
  AddAddress(TAddress.Create(
    '512 Florida Pl, Barberton, OH 44203', 41.003671512008, -81.598461046815, 300,
    15 * 3600 + 00 * 60, 15 * 3600 + 15 * 60, 15 * 3600 + 30 * 60, 15 * 3600 + 45 * 60),
    Result);
  AddAddress(TAddress.Create(
    '559 W Aurora Rd, Northfield, OH 44067', 41.315116882324, -81.558746337891, 50,
    16 * 3600 + 00 * 60, 16 * 3600 + 15 * 60, 16 * 3600 + 30 * 60, 17 * 3600 + 00 * 60),
    Result);
end;

function TSingleDriverMultipleTimeWindowsTestDataProvider.MakeRouteParameters: TRouteParameters;
begin
  Result := TRouteParameters.Create();
  Result.AlgorithmType := Integer(TAlgorithmType.TSP);
  Result.StoreRoute := False;
  Result.RouteName := 'Single Driver Multiple TimeWindows 12 Stops';
  Result.RouteDate := 53583232;//TUtils.ConvertToUnixTimestamp(IncDay(Now, 1));
  Result.RouteTime := 5 * 3600 + 30 * 60;
  Result.Optimize := TOptimizeDescription[Distance];
  Result.DistanceUnit := TDistanceUnitDescription[MI];
  Result.DeviceType := TDeviceTypeDescription[Web];
end;

end.
