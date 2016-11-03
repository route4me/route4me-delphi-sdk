unit MultipleDepotMultipleDriverTestDataProviderUnit;

interface

uses
  SysUtils,
  BaseOptimizationParametersProviderUnit, AddressUnit, RouteParametersUnit,
  OptimizationParametersUnit;

type
  TMultipleDepotMultipleDriverTestDataProvider = class(TBaseOptimizationParametersProvider)
  protected
    function MakeAddresses(): TAddressesArray; override;
    function MakeRouteParameters(): TRouteParameters; override;
    /// <summary>
    ///  After response some fields are changed from request.
    /// </summary>
    procedure CorrectForResponse(OptimizationParameters: TOptimizationParameters); override;
  public

  end;

implementation

{ TMultipleDepotMultipleDriverTestDataProvider }

uses
  DateUtils,
  EnumsUnit, UtilsUnit;

procedure TMultipleDepotMultipleDriverTestDataProvider.CorrectForResponse(
  OptimizationParameters: TOptimizationParameters);
begin
  inherited;

end;

function TMultipleDepotMultipleDriverTestDataProvider.MakeAddresses: TAddressesArray;
var
  FirstAddress: TAddress;
begin
  Result := TAddressesArray.Create();

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

  AddAddress(TAddress.Create(
    '1218 Ruth Ave, Cuyahoga Falls, OH 44221', 41.135762259364, -81.629313826561, 300, 29465, 30529),
    Result);
  AddAddress(TAddress.Create(
    '512 Florida Pl, Barberton, OH 44203', 41.003671512008, -81.598461046815, 300, 30529, 33779),
    Result);
  AddAddress(TAddress.Create(
    '512 Florida Pl, Barberton, OH 44203', 41.003671512008, -81.598461046815, 100, 33779, 33944),
    Result);
  AddAddress(TAddress.Create(
    '3495 Purdue St, Cuyahoga Falls, OH 44221', 41.162971496582, -81.479049682617, 300, 33944, 34801),
    Result);
  AddAddress(TAddress.Create(
    '1659 Hibbard Dr, Stow, OH 44224', 41.194505989552, -81.443351581693, 300, 34801, 36366),
    Result);
  AddAddress(TAddress.Create(
    '2705 N River Rd, Stow, OH 44224', 41.145240783691, -81.410247802734, 300, 36366, 39173),
    Result);
  AddAddress(TAddress.Create(
    '10159 Bissell Dr, Twinsburg, OH 44087', 41.340042114258, -81.421226501465, 300, 39173, 41617),
    Result);
  AddAddress(TAddress.Create(
    '367 Cathy Dr, Munroe Falls, OH 44262', 41.148578643799, -81.429229736328, 300, 41617, 43660),
    Result);
  AddAddress(TAddress.Create(
    '367 Cathy Dr, Munroe Falls, OH 44262', 41.148578643799, -81.429229736328, 300, 43660, 46392),
    Result);
  AddAddress(TAddress.Create(
    '512 Florida Pl, Barberton, OH 44203', 41.003671512008, -81.598461046815, 300, 46392, 48389),
    Result);
  AddAddress(TAddress.Create(
    '559 W Aurora Rd, Northfield, OH 44067', 41.315116882324, -81.558746337891, 50, 48389, 48449),
    Result);
  AddAddress(TAddress.Create(
    '3933 Klein Ave, Stow, OH 44224', 41.169467926025, -81.429420471191, 300, 48449, 50152),
    Result);
  AddAddress(TAddress.Create(
    '2148 8th St, Cuyahoga Falls, OH 44221', 41.136692047119, -81.493492126465, 300, 50152, 51982),
    Result);
  AddAddress(TAddress.Create(
    '3731 Osage St, Stow, OH 44224', 41.161357879639, -81.42293548584, 100, 51982, 52180),
    Result);
  AddAddress(TAddress.Create(
    '3731 Osage St, Stow, OH 44224', 41.161357879639, -81.42293548584, 300, 52180, 54379),
    Result);
end;

function TMultipleDepotMultipleDriverTestDataProvider.MakeRouteParameters: TRouteParameters;
begin
  Result := TRouteParameters.Create();
  // specify capacitated vehicle routing with time windows and multiple depots, with multiple drivers
  Result.AlgorithmType := TAlgorithmType.CVRP_TW_MD;

  // set an arbitrary route name
  // this value shows up in the website, and all the connected mobile device
  Result.RouteName := 'Multiple Depot, Multiple Driver';
  // the route start date in UTC, unix timestamp seconds (Tomorrow)
  Result.RouteDate := 53583232; //TUtils.ConvertToUnixTimestamp(IncDay(Now, 1));
  // the time in UTC when a route is starting (7AM)
  Result.RouteTime := 60 * 60 * 7;
  // the maximum duration of a route
  Result.RouteMaxDuration := 86400;
  Result.VehicleCapacity := '1';
  Result.VehicleMaxDistanceMI := '10000';
  Result.Optimize := TOptimize.Distance;
  Result.DistanceUnit := TDistanceUnit.MI;
  Result.DeviceType := TDeviceType.Web;
  Result.TravelMode := TTravelMode.Driving;
  Result.Metric := TMetric.Geodesic;
end;

end.
