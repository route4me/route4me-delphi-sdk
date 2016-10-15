unit MultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProviderUnit;

interface

uses
  SysUtils,
  BaseOptimizationParametersProviderUnit, AddressUnit, RouteParametersUnit;

type
  TMultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProvider = class(TBaseOptimizationParametersProvider)
  protected
    function MakeAddresses(): TArray<TAddress>; override;
    function MakeRouteParameters(): TRouteParameters; override;
  public

  end;

implementation

{ TMultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProvider }

uses
  DateUtils,
  EnumsUnit, UtilsUnit;

function TMultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProvider.MakeAddresses: TArray<TAddress>;
var
  FirstAddress: TAddress;
begin
  Result := TArray<TAddress>.Create();

  FirstAddress := TAddress.Create(
    '3634 W Market St, Fairlawn, OH 44333', 41.135762259364, -81.629313826561, 300);
  //indicate that this is a departure stop
  // single depot routes can only have one departure depot
  FirstAddress.IsDepot := True;
  //together these two specify the time window of a destination
  //seconds offset relative to the route start time for the open availability of a destination
  FirstAddress.TimeWindowStart := 28800;
  //seconds offset relative to the route end time for the open availability of a destination
  FirstAddress.TimeWindowEnd := 29465;
  AddAddress(FirstAddress, Result);

  AddAddress(TAddress.Create('1218 Ruth Ave, Cuyahoga Falls, OH 44221', 41.143505096435, -81.46549987793, 300, 29465, 30529), Result);
  AddAddress(TAddress.Create('512 Florida Pl, Barberton, OH 44203', 41.003671512008, -81.598461046815, 300, 30529, 33479), Result);
  AddAddress(TAddress.Create('512 Florida Pl, Barberton, OH 44203', 41.003671512008, -81.598461046815, 300, 33479, 33944), Result);
  AddAddress(TAddress.Create('3495 Purdue St, Cuyahoga Falls, OH 44221', 41.162971496582, -81.479049682617, 300, 33944, 34801), Result);
  AddAddress(TAddress.Create('1659 Hibbard Dr, Stow, OH 44224', 41.194505989552, -81.443351581693, 300, 34801, 36366), Result);
  AddAddress(TAddress.Create('2705 N River Rd, Stow, OH 44224', 41.145240783691, -81.410247802734, 300, 36366, 39173), Result);
  AddAddress(TAddress.Create('10159 Bissell Dr, Twinsburg, OH 44087', 41.340042114258, -81.421226501465, 300, 39173, 41617), Result);
  AddAddress(TAddress.Create('367 Cathy Dr, Munroe Falls, OH 44262', 41.148578643799, -81.429229736328, 300, 41617, 43660), Result);
  AddAddress(TAddress.Create('367 Cathy Dr, Munroe Falls, OH 44262', 41.148579, -81.42923, 300, 43660, 46392), Result);
  AddAddress(TAddress.Create('512 Florida Pl, Barberton, OH 44203', 41.003671512008, -81.598461046815, 300, 46392, 48089), Result);
  AddAddress(TAddress.Create('559 W Aurora Rd, Northfield, OH 44067', 41.315116882324, -81.558746337891, 300, 48089, 48449), Result);
  AddAddress(TAddress.Create('3933 Klein Ave, Stow, OH 44224', 41.169467926025, -81.429420471191, 300, 48449, 50152), Result);
  AddAddress(TAddress.Create('2148 8th St, Cuyahoga Falls, OH 44221', 41.136692047119, -81.493492126465, 300, 50152, 51682), Result);
  AddAddress(TAddress.Create('3731 Osage St, Stow, OH 44224', 41.161357879639, -81.42293548584, 300, 51682, 54379), Result);
  AddAddress(TAddress.Create('3862 Klein Ave, Stow, OH 44224', 41.167895123363, -81.429973393679, 300, 54379, 54879), Result);
  AddAddress(TAddress.Create('138 Northwood Ln, Tallmadge, OH 44278', 41.085464134812, -81.447411775589, 300, 54879, 56613), Result);
  AddAddress(TAddress.Create('3401 Saratoga Blvd, Stow, OH 44224', 41.148849487305, -81.407363891602, 300, 56613, 57052), Result);
  AddAddress(TAddress.Create('5169 Brockton Dr, Stow, OH 44224', 41.195003509521, -81.392700195312, 300, 57052, 59004), Result);
  AddAddress(TAddress.Create('5169 Brockton Dr, Stow, OH 44224', 41.195003509521, -81.392700195312, 300, 59004, 60027), Result);
  AddAddress(TAddress.Create('458 Aintree Dr, Munroe Falls, OH 44262', 41.1266746521, -81.445808410645, 300, 60027, 60375), Result);
  AddAddress(TAddress.Create('512 Florida Pl, Barberton, OH 44203', 41.003671512008, -81.598461046815, 300, 60375, 63891), Result);
  AddAddress(TAddress.Create('2299 Tyre Dr, Hudson, OH 44236', 41.250511169434, -81.420433044434, 300, 63891, 65277), Result);
  AddAddress(TAddress.Create('2148 8th St, Cuyahoga Falls, OH 44221', 41.136692047119, -81.493492126465, 300, 65277, 68545), Result);
end;

function TMultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProvider.MakeRouteParameters: TRouteParameters;
begin
  Result := TRouteParameters.Create();
  // specify capacitated vehicle routing with time windows and multiple depots, with multiple drivers
  Result.AlgorithmType := Integer(TAlgorithmType.CVRP_TW_MD);
  Result.StoreRoute := False;

  // set an arbitrary route name
  // this value shows up in the website, and all the connected mobile device
  Result.RouteName := 'Multiple Depot, Multiple Driver with 24 Stops, Time Window';
  // the route start date in UTC, unix timestamp seconds (Tomorrow)
  Result.RouteDate := 53583232; //TUtils.ConvertToUnixTimestamp(IncDay(Now, 1));
  // the time in UTC when a route is starting (7AM)
  Result.RouteTime := 60 * 60 * 7;
  // the maximum duration of a route
  Result.RouteMaxDuration := 86400;
  Result.VehicleCapacity := '1';
  Result.VehicleMaxDistanceMI := '10000';
  Result.Optimize := TOptimizeDescription[Distance];
  Result.DistanceUnit := TDistanceUnitDescription[MI];
  Result.DeviceType := TDeviceTypeDescription[Web];
  Result.TravelMode := TTravelModeDescription[Driving];
  Result.Metric := Integer(TMetric.Geodesic);
end;

end.
