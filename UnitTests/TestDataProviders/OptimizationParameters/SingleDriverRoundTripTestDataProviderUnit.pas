unit SingleDriverRoundTripTestDataProviderUnit;

interface

uses
  SysUtils,
  BaseOptimizationParametersProviderUnit, AddressUnit, RouteParametersUnit;

type
  TSingleDriverRoundTripTestDataProvider = class(TBaseOptimizationParametersProvider)
  protected
    function MakeAddresses(): TAddressesArray; override;
    function MakeRouteParameters(): TRouteParameters; override;
  public

  end;

implementation

{ TSingleDriverRoundTripTestDataProvider }

uses
  DateUtils,
  EnumsUnit, UtilsUnit;

function TSingleDriverRoundTripTestDataProvider.MakeAddresses: TAddressesArray;
var
  FirstAddress: TAddress;
begin
  Result := TAddressesArray.Create();

  FirstAddress := TAddress.Create(
    '754 5th Ave New York, NY 10019', 'Bergdorf Goodman', 40.7636197, -73.9744388, 0);
  //indicate that this is a departure stop
  // single depot routes can only have one departure depot
  FirstAddress.IsDepot := True;
  AddAddress(FirstAddress, Result);

  AddAddress(TAddress.Create(
    '717 5th Ave New York, NY 10022', 'Giorgio Armani', 40.7669692, -73.9693864, 0),
    Result);
  AddAddress(TAddress.Create(
    '888 Madison Ave New York, NY 10014', 'Ralph Lauren Women''s and Home', 40.7715154, -73.9669241, 0),
    Result);
  AddAddress(TAddress.Create(
    '1011 Madison Ave New York, NY 10075', 'Yigal Azrou''l', 40.7772129, -73.9669, 0),
    Result);
  AddAddress(TAddress.Create(
    '440 Columbus Ave New York, NY 10024', 'Frank Stella Clothier', 40.7808364, -73.9732729, 0),
    Result);
  AddAddress(TAddress.Create(
    '324 Columbus Ave #1 New York, NY 10023', 'Liana', 40.7803123, -73.9793079, 0),
    Result);
  AddAddress(TAddress.Create(
    '110 W End Ave New York, NY 10023', 'Toga Bike Shop', 40.7753077, -73.9861529, 0),
    Result);
  AddAddress(TAddress.Create(
    '555 W 57th St New York, NY 10019', 'BMW of Manhattan', 40.7718005, -73.9897716, 0),
    Result);
  AddAddress(TAddress.Create(
    '57 W 57th St New York, NY 10019', 'Verizon Wireless', 40.7558695, -73.9862019, 0),
    Result);
end;

function TSingleDriverRoundTripTestDataProvider.MakeRouteParameters: TRouteParameters;
begin
  Result := TRouteParameters.Create();
  Result.AlgorithmType := Integer(TAlgorithmType.TSP);
  Result.StoreRoute := False;
  Result.RouteName := 'Single Driver Round Trip';
  Result.RouteDate := 53583232;//TUtils.ConvertToUnixTimestamp(IncDay(Now, 1));
  Result.RouteTime := 60 * 60 * 7;
  Result.RouteMaxDuration := 86400;
  Result.VehicleCapacity := '1';
  Result.VehicleMaxDistanceMI := '10000';
  Result.Optimize := TOptimizeDescription[Distance];
  Result.DistanceUnit := TDistanceUnitDescription[MI];
  Result.DeviceType := TDeviceTypeDescription[Web];
  Result.TravelMode := TTravelModeDescription[Driving];
end;

end.

