unit AddOrderToRouteParameterProviderUnit;

interface

uses AddOrderToRouteRequestUnit;

type
  IAddOrderToRouteParameterProvider = interface
    ['{794E1351-AC94-40F7-8D02-DC4D7FFDFC59}']
    function GetParameters: TAddOrderToRouteRequest;
    function GetAddresses: TOrderedAddressArray;
  end;

  TAddOrderToRouteParameterProvider = class(TInterfacedObject, IAddOrderToRouteParameterProvider)
  public
    function GetParameters: TAddOrderToRouteRequest;
    function GetAddresses: TOrderedAddressArray;
  end;

implementation

{ TAddOrderToRouteParameterProvider }

uses RouteParametersUnit, EnumsUnit;

function TAddOrderToRouteParameterProvider.GetAddresses: TOrderedAddressArray;
var
  Address: TOrderedAddress;
begin
  SetLength(Result, 2);

  Address := TOrderedAddress.Create;
  Address.AddressString := '325 Broadway, New York, NY 10007, USA';
  Address.Alias := 'BK Restaurant #: 20333';
  Address.Latitude := 40.71615;
  Address.Longitude := -74.00505;
  Address.CurbsideLatitude := 40.71615;
  Address.CurbsideLongitude := -74.00505;
  Address.Phone := '(212) 227-7535';
  Address.OrderId := '7205704';
  Result[0] := Address;

  Address := TOrderedAddress.Create;
  Address.AddressString := '106 Fulton St, Farmingdale, NY 11735, USA';
  Address.Alias := 'BK Restaurant #: 17871';
  Address.Latitude := 40.73073;
  Address.Longitude := -73.459283;
  Address.CurbsideLatitude := 40.73073;
  Address.CurbsideLongitude := -73.459283;
  Address.Phone := '(212) 566-5132';
  Address.OrderId := '7205703';
  Result[1] := Address;
end;

function TAddOrderToRouteParameterProvider.GetParameters: TAddOrderToRouteRequest;
var
  RouteParameters: TRouteParameters;
  Address: TOrderedAddress;
begin
  Result := TAddOrderToRouteRequest.Create;

  for Address in GetAddresses do
    Result.AddAddress(Address);

  RouteParameters := TRouteParameters.Create;
  RouteParameters.RouteName := 'Wednesday 15th of June 2016 07:01 PM (+03:00)';
  RouteParameters.RouteDate := 1465948800;
  RouteParameters.RouteTime := 14400;
  RouteParameters.Optimize := TOptimize.Time;
  RouteParameters.RouteType := 'single';
  RouteParameters.AlgorithmType := TAlgorithmType.TSP;
  RouteParameters.RT := False;
  RouteParameters.LockLast := False;
  RouteParameters.MemberId := '1116';
  RouteParameters.DisableOptimization := False;

  Result.Parameters := RouteParameters;
end;

end.
